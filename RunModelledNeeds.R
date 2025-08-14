##
# A project to run a logistic regression model to predict whether a patient has
#   a long-term condition given specific predictor variables.
#   Predictions are aggregated by area, and observed/expected LTC occurences are
#   compared.
#
# PlumbeR is used to allow API calls, so that a user can pass LTC, area
#   grouping, and predictor variables from an app, and have the data returned
#   as a JSON.
#
# Inputs: none
# Outputs: JSON dataframe containing area-level prediction statistics
##

need_packages <- c(
  'plumber',
  'plyr',
  'RPostgreSQL',
  'caret',
  'e1071',
  'rlang',
  'config',
  'MASS',
  'xgboost',
  'tidyverse',
  'PRROC'
)

lapply(need_packages, library, character.only = TRUE)

config <- get()

# Load custom functions
invisible(
  lapply(
    Sys.glob("user_defined_functions/*.R"),
    function(x) source(x)
  )
)

# Get patient data
get_query <- function(query) {
  postgres <- dbConnect(
    RPostgreSQL::PostgreSQL(),
    dbname = config$sql_credentials$database,
    user = config$sql_credentials$uid,
    host = config$sql_credentials$server,
    password = config$sql_credentials$pwd,
    port = config$sql_credentials$port
  )

  response <- dbGetQuery(postgres, query)

  dbDisconnect(postgres)

  response
}

# Check DB Connection
get_query('SELECT * FROM imd_2019 LIMIT 1;') %>% print()

# Source plumbeR API and start listening
try_p <- tryCatch(
  {
    pr() %>% pr_filter('cors', function(req, res) {
      res$setHeader("Access-Control-Allow-Origin", "*")

      if (req$REQUEST_METHOD == "OPTIONS") {
        res$setHeader("Access-Control-Allow-Methods","*")
        res$setHeader(
          "Access-Control-Allow-Headers", req$HTTP_ACCESS_CONTROL_REQUEST_HEADERS
        )
        res$status <- 200
        return(list())
      } else {
        if (!"referer" %in% names(req$HEADERS)) {
          authorised <- 401
        } else if (
          !any(str_detect(req$HEADERS["referer"], c("plumber", "swagger", "__docs__")))
        ) {
          jwt <- req$HEADERS["authorization"]
          authorised <- tryCatch({
            GET(
              Sys.getenv('SITE_URL'),
              add_headers(authorization = unname(jwt))
            ) %>%
              status_code()
          },
          error = function(e) {
            return(401)
          })

          print(req$HEADERS)
        } else {
          authorised <- 200
        }

        if (authorised == 200) {
          plumber::forward()
        } else {
          plumber::forward()
        }
      }
    }) %>% pr_get('/modelled_needs_api', function(
      response_filter_1 = '', response_filter_2 = '',
      sampling = 'none',
      age_as_a_factor = 'N',
      area_level = 'GP Practice',
      filter_to_area = '',
      train_on_area = '',
      group_1 = '', group_2 = '', group_3 = '', group_4 = '', group_5 = '',
      group_6 = '', group_7 = '', group_8 = '', group_9 = '', group_10 = '',
      group_11 = '', group_12 = '', group_13 = '', group_14 = '', group_15 = '',
      group_16 = '', group_17 = '', group_18 = '', group_19 = '', group_20 = '',
      group_21 = '', group_22 = '', group_23 = '', group_24 = '', group_25 = '',
      group_26 = '', group_27 = '', group_28 = '', group_29 = '', group_30 = '',
      group_31 = ''
    ) {
      # Free up memory after previous model run
      gc()

      if (response_filter_1 == '' & response_filter_2 == '') {
        message('Please select a long-term condition, cohort, or count/cost variable to predict')
      } else {
        options(stringsAsFactors = FALSE)

        # Import lookup tables
        response_lookup <- get_response_lookup()
        area_lookup <- get_area_lookup()
        predictor_lookup <- get_predictor_lookup()

        # Filtering area/long-term condition/predictors to selected values
        area_level <- area_lookup %>%
          filter(area_full_name == area_level) %>%
          select(area_short_name) %>%
          as.character()

        keep_response <- data.frame(
          response_full_name = c(response_filter_1, response_filter_2)
        ) %>%
          inner_join(response_lookup, by = 'response_full_name') %>%
          select(response_short_name) %>%
          unlist() %>%
          as.character()

        if (identical(keep_response, character(0))) {
          keep_response <- response_filter_1
        }

        # Group input variables into one vector
        predictor_input <- c(
          group_1, group_2, group_3, group_4, group_5,
          group_6, group_7, group_8, group_9, group_10,
          group_11, group_12, group_13, group_14, group_15,
          group_16, group_17, group_18, group_19, group_20,
          group_21, group_22, group_23, group_24, group_25,
          group_26, group_27, group_28, group_29, group_30,
          group_31
        )

        # Number of input predictor variables
        n_predictors <- predictor_input %>%
          na_if('') %>%
          na_if('undefined') %>%
          na.omit() %>%
          length()

        # Join predictor variables into a string for use in formula
        predictor_formula <- data.frame(predictor_full_name = predictor_input) %>%
          inner_join(predictor_lookup, by = 'predictor_full_name') %>%
          select(predictor_short_name) %>%
          unlist() %>%
          paste(collapse = ' + ')

        # Formula for use in predictive model
        formula <- as.formula(paste('predict_var', predictor_formula, sep = ' ~ '))

        # String for use in SQL
        predictor_sql <- data.frame(predictor_full_name = predictor_input) %>%
          inner_join(predictor_lookup, by = 'predictor_full_name')  %>%
          pull(predictor_short_name) %>%
          c(keep_response, .) %>%
          unique() %>%
          {
            if (area_level != "ccg") . <- c("population_master.ccg", .)
            if (area_level != "pcn") . <- c("pcn", .)
            .
          } %>%
          str_c(collapse = ', ') %>%
        {paste("SELECT", ., ",", area_level,'FROM population_master LEFT JOIN imd_2019 ON lsoa = lsoa_code')}

        if (!filter_to_area %in% c('', 'undefined')) {
          filter_to_area <- str_replace_all(filter_to_area, 'and', '&')

          predictor_sql <- paste(
            predictor_sql,
            paste0("JOIN (values ('", filter_to_area, "')) t(ccg) ON t.ccg = population_master.ccg")
          )
        }

        patient_records <- get_query(predictor_sql)

        # Mutate dataframe to a format used by GLM
        patient_records <- patient_records %>%
          format_glm_data(area_level, keep_response, age_as_a_factor)

        # Specify training set
        if (! train_on_area %in% c('', 'undefined')) {
          train_on_area <- str_replace_all(
            train_on_area, 'Fylde and Wyre', 'Fylde & Wyre'
          )

          train_on_area <- str_replace_all(
            train_on_area, 'F and W', 'F&W'
          )

          train_on_area <- str_replace_all(
            train_on_area, 'Ansdell and St Annes', 'Ansdell & St Annes'
          )

          if (area_level == 'pcn') {
            training_records <- patient_records %>%
              filter(area_var %in% train_on_area | ccg %in% train_on_area)
          } else if (area_level == 'ccg_name') {
            training_records <- patient_records %>%
              filter(pcn %in% train_on_area | area_var %in% train_on_area)
          } else {
            training_records <- patient_records %>%
              filter(pcn %in% train_on_area | ccg %in% train_on_area)
          }
        } else {
          training_records <- patient_records
        }

        cat('Running logistic model...\n')

        # Base prevalence of long-term condition
        ltc_prevalence <- training_records %>%
          summarise(prevalence = sum(predict_var == 'Y') / n()) %>%
          as.numeric()

        # Run logistic model, predict outcomes, and build confusion matrix
        print(training_records)
        model <- glm(
          formula,
          data = training_records,
          family = binomial(link = 'logit')
        )
        pf <- predict(model, newdata = patient_records, type = 'response')
        pf_factor <- as.factor(ifelse(pf > ltc_prevalence, 'Y', 'N'))
        model_stats <- confusionMatrix(pf_factor, patient_records$predict_var)

        # Number of area groups
        n_areas <- patient_records %>%
          distinct(area_var) %>%
          nrow()

        # Combine patient records with predicted outcome and indicate significance
        model_match <- patient_records %>%
          compare_model_output(pf, n_predictors, 'logistic')

        # Calculate ROC and precision-recall curves for model analysis
        diagnostics <- patient_records %>% get_logistic_model_diagnostics(pf)

        # Calculate variable importance on predicted outcome
        variable_importance <- varImp(model) %>%
          rownames_to_column() %>%
          rename(
            model_variable = rowname,
            importance = Overall
          )

        model_output <- list(
          "model_match" = model_match,
          "diagnostics" = diagnostics,
          "importance" = variable_importance,
          "message" = "OK"
        )

        cat('\n\n Done! \n\n')

        return(model_output)
      }

      # Free up memory from model run (not sure if this'll work without rm())
      gc()
    }) %>%
      pr_get('/ping', function() {'Who dares disturb my slumber?'}) %>%
      pr_get('/get_ccg_list', function() {
        get_query(
          "SELECT DISTINCT ccg FROM population_master WHERE ccg != 'Not A Real Lancashire CCG';"
        ) %>%
          mutate(ccg = str_replace_all(ccg, '&', 'and'))
      }) %>%
      pr_get('/get_gp_list', function(ccg = '') {
        query <- 'SELECT DISTINCT practice FROM population_master'

        if (ccg != '') {
          ccg_list <- str_split(ccg, ",")

          query <- paste0(
            query,
            " WHERE ccg IN ('",
            paste(unlist(ccg_list), collapse = "', '"),
            "')"
          )
        }

        query <- paste(query, "AND ccg != 'Not A Real Lancashire CCG'")

        get_query(query)
      }) %>%
      pr_get('/get_ccg_pcn_list', function(ccg = '') {
        query <- 'SELECT DISTINCT ccg, pcn FROM population_master'

        if (ccg != '') {
          ccg_list <- ccg %>%
            str_replace_all(' and ', ' & ') %>%
            str_split(",")

          query <- paste0(
            query,
            " WHERE ccg IN ('",
            paste(unlist(ccg_list), collapse = "', '"),
            "')",
            "AND ccg != 'Not A Real Lancashire CCG'"
          )
        } else {
          query <- paste(query, "WHERE ccg != 'Not A Real Lancashire CCG'")
        }

        get_query(query)
      }) %>%
      pr_run(host = '0.0.0.0', port = 8092, docs = T)
  },
  error = function(cond) {
    cat("\n" + as.character(cond) + "\n");
    return(list(
      "message" = "Bad Request, Please Select Different Response Variable(s)",
      "error" = as.character(cond)
    ))
  }
)

cat("\n\nClosing PlumbeR Instance!\n\n")
