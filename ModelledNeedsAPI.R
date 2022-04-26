#* Filter CORS otherwise Angular's API call gets blocked
#* If the request isn't just for options then check the user has a valid Nexus
#*   Intelligence JWT token and only forward into the actual API if they do
#* @filter cors
cors <- function(req, res) {
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
      !any(str_detect(req$HEADERS["referer"], c("plumber", "swagger")))
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

      print(jwt)
      print(authorised)
    } else {
      authorised <- 200
    }

    if (authorised == 200) {
      plumber::forward()
    } else {
      plumber::forward() #return(list())
    }
  }
}

#* Get list of useful interventions for users to analyse for cohorts
#* @param cohort_input
#* @param min_date
#* @param phases
#* @get /search_cohort_interventions
function(cohort_input = '', min_date = 40, phases = '3,4,not_applicable') {
  library(rapiclient)
  library(tidyverse)
  library(lubridate)

  source('user_defined_functions/FormatSearchString.R')

  options(stringsAsFactors = FALSE)

  if(cohort_input == '') {
    return('Please Enter a Cohort To Search On')
  } else {
    # Only check the most recent trials
    min_date <- today() - years(as.numeric(min_date))

    # Choose trial phase
    phase_input <- str_split(phases, ',')

    cohort <- jsonlite::parse_json(jsonlite::parse_json(cohort_input)) %>%
      unlist()

    open_trials_api <- get_api("http://api.opentrials.net/v1/swagger.yaml")

    open_trials <- get_operations(
      open_trials_api, handle_response = content_or_warning
    )

    search_terms <- c(
      AgeDimension1 = 'min_age',
      AgeDimension2 = 'max_age',
      SexDimension = 'gender',
      LTCs2Dimension = 'conditions',
      LTCs2Dimension1 = 'conditions',
      LTCs2Dimension2 = 'conditions',
      LTCs2Dimension3 = 'conditions',
      LTCs2Dimension4 = 'conditions'
    )

    cohort <- cohort[names(cohort) %in% names(search_terms)]
    names(cohort) <- search_terms[names(cohort)]

    if ('conditions' %in% names(cohort)) {
      cohort <- c(cohort, brief_summary = cohort['conditions'])
    }

    if ('min_age' %in% names(cohort)) {
      age_min <- as.numeric(cohort['min_age'])

      cohort['min_age'] <- paste(
        seq(pmax(age_min - 5, 0), age_min + 5, 1),
        collapse = ' || '
      )
    }

    search_string <- paste(names(cohort), cohort, sep = ":") %>%
      tolower() %>%
      str_replace('conditions', 'conditions.name') %>%
      str_replace('min_age:', 'age_range.min_age:') %>%
      str_replace('max_age:', 'age_range.max_age:<=')

    if (length(str_match_all(search_string, 'conditions')) > 1) {
      multi_condition <- str_detect(search_string, 'conditions')

      if (length(str_match_all(search_string, 'brief')) > 1) {
        search_string <- c(
          search_string[!multi_condition],
          paste(search_string[multi_condition], collapse = ' || ')
        )

        search_string <- str_replace_all(
          search_string, 'brief_summary.conditions.name', 'brief_summary'
        )
      } else {
        search_string <- c(
          search_string[!multi_condition],
          paste(search_string[multi_condition], collapse = ' && ')
        )
      }
    }

    elastic_string <- format_search_string(
      search_string, phase = phase_input, date = min_date
    )

    top_list <- open_trials$searchTrials(q = elastic_string)

    # If the API call comes back empty then widen the search parameters
    if (length(str_match_all(search_string, 'max_age')) > 1 & top_list$total_count == 0) {
      elastic_string <- format_search_string(
        search_string, 'age_range.max.*', phase = phase_input, date = min_date
      )

      top_list <- open_trials$searchTrials(q = elastic_string)
    }
    if (length(str_match_all(search_string, 'min_age')) > 1 & top_list$total_count == 0) {
      elastic_string <- format_search_string(
        search_string, 'age_range.min.*', phase = phase_input, date = min_date
      )

      top_list <- open_trials$searchTrials(q = elastic_string)
    }
    if (length(str_match_all(search_string, 'gender')) > 1 & top_list$total_count == 0) {
      elastic_string <- format_search_string(
        search_string, 'gender.*', phase = phase_input, date = min_date
      )

      top_list <- open_trials$searchTrials(q = elastic_string)
    }

    best_papers <- lapply(top_list$items, function(t) {
      data.frame(
        title = str_replace_all(t$public_title, '\n', ''),
        source_id = t$source_id,
        sample_size = ifelse(
          length(t$target_sample_size) == 0, NA, t$target_sample_size
        ),
        status = t$status,
        registered = format(as.POSIXct(t$registration_date)),
        publications = length(t$documents),
        url = unlist(t$documents)['source_url'],
        age_range = paste(
          unlist(t$age_range)['min_age'],
          ' - ',
          unlist(t$age_range)['max_age']
        ),
        phase = unlist(t$study_phase)
      )
    }) %>%
      bind_rows() %>%
      arrange(desc(sample_size))

    return(best_papers)
  }
}

#* Get secondary usage counts aggregated by specialty
#* @param response_filter_1
#* @param response_filter_2
#* @param area_level
#* @param area_filter
#* @get /usage_by_group
function(
  response_filter_1 = '', response_filter_2 = '',
  area_filter = '',
  patient_records = patient_data,
  patient_activity = activity_data,
  area_level = 'gpp_code'
) {
  options(stringsAsFactors = FALSE)

  area_filter <- area_filter %>%
    str_replace_all('Fylde and Wyre', 'Fylde & Wyre') %>%
    str_replace_all('F and W', 'F&W') %>%
    str_replace_all('Ansdell and St Annes', 'Ansdell & St Annes')

  # Import lookup tables
  response_lookup <- get_response_lookup()
  area_lookup <- get_area_lookup()

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

  # Mutate dataframe to a format used by GLM
  secondary_usage <- patient_records %>%
    format_glm_data(area_level, keep_response, 'N') %>%
    left_join(patient_activity)

  if (area_filter == '') {
    area_filter <- unique(patient_records$area_var)
  }

  # See where secondary care usage is coming from, for this set of patients
  secondary_usage <- secondary_usage %>%
    filter(predict_var == 'Y' & area_var %in% area_filter) %>%
    count(specialty) %>%
    select(specialty, n)

  top_usage <- secondary_usage %>%
    arrange(desc(n)) %>%
    mutate(
      specialty = coalesce(specialty, 'No Attendances')
    )

  return(top_usage[1:21, ])
}

#* Logistic regression model to predict long-term conditions in patients.
#*   Probabilities are aggregated into area groupings (e.g. GP Practice) and
#*   expected & observed LTC occurances are compared.
#* @param response_filter_1
#* @param response_filter_2
#* @param group_1
#* @param group_2
#* @param group_3
#* @param group_4
#* @param group_5
#* @param group_6
#* @param group_7
#* @param group_8
#* @param group_9
#* @param group_10
#* @param group_11
#* @param group_12
#* @param group_13
#* @param group_14
#* @param group_15
#* @param group_16
#* @param group_17
#* @param group_18
#* @param group_19
#* @param group_20
#* @param group_21
#* @param group_22
#* @param group_23
#* @param group_24
#* @param group_25
#* @param group_26
#* @param group_27
#* @param group_28
#* @param group_29
#* @param group_30
#* @param group_31
#* @param area_level
#* @param sampling
#* @param age_as_a_factor
#* @param filter_to_area
#* @param train_on_area
#* @get /modelled_needs_api
function(
  response_filter_1 = '', response_filter_2 = '',
  sampling = 'none',
  age_as_a_factor = 'N',
  area_level = 'GP Practice',
  patient_records = patient_data,
  patient_activity = activity_data,
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

    # Filter to training set within specified area
    if (! filter_to_area %in% c('', 'undefined')) {
      filter_to_area <- str_replace(
        filter_to_area, 'Fylde and Wyre', 'Fylde & Wyre'
      )

      filter_to_area <- unlist(str_split(filter_to_area, ','))

      patient_records <- patient_records %>%
        filter(ccg_name %in% filter_to_area)
    }

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

      if (area_level == 'locality') {
        training_records <- patient_records %>%
          filter(area_var %in% train_on_area | ccg_name %in% train_on_area)
      } else if (area_level == 'ccg_name') {
        training_records <- patient_records %>%
          filter(locality %in% train_on_area | area_var %in% train_on_area)
      } else {
        training_records <- patient_records %>%
          filter(locality %in% train_on_area | ccg_name %in% train_on_area)
      }
    } else {
      training_records <- patient_records
    }

    # Choose which model to use, depending on response variable(s)
    #   If response variable =
    #     cohort: logistic model
    #     long-term condition: logistic model
    #     secondary care attendance: negative binomial model
    #     secondary care cost: negative binomial model + Monte Carlo estimation
    if (all(str_detect(keep_response, 'ltc')) ||
          str_detect(keep_response, '\\{')) {
      cat('Running logistic model...\n')

      # Base prevalence of long-term condition
      ltc_prevalence <- training_records %>%
        summarise(prevalence = sum(predict_var == 'Y') / n()) %>%
        as.numeric()

      # Run logistic model, predict outcomes, and build confusion matrix
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
    } else if (any(str_detect(keep_response, 'total_cost'))) {
      cat('Running Negative Binomial cost model...\n')

      # Testing running XGBoost
      xg_predictors <- data.frame(predictor_full_name = predictor_input) %>%
        inner_join(predictor_lookup, by = 'predictor_full_name') %>%
        select(predictor_short_name) %>%
        unlist() %>%
        as.character()

      xg_train <- training_records %>%
        select(xg_predictors) %>%
        mutate_if(is.factor, as.numeric) %>%
        as.matrix()

      xg_test <- patient_records %>%
        select(xg_predictors) %>%
        mutate_if(is.factor, as.numeric) %>%
        as.matrix()

      op_count <- xgboost(
        xg_train,
        label = training_records$op_appointments,
        objective = 'count:poisson',
        nrounds = 100
      )
      ip_count <- xgboost(
        xg_train,
        label = training_records$ip_elective,
        objective = 'count:poisson',
        nrounds = 100
      )
      ae_count <- xgboost(
        xg_train,
        label = training_records$ae_attendances,
        objective = 'count:poisson',
        nrounds = 100
      )

      model_match <- compare_cost_model_output(
        patient_records, ip_count, op_count, ae_count, cost_distribution, xg_predictors
      )

      # Compare distributions for observed and predicted cost
      diagnostics <- patient_records %>%
        mutate(predict_var = ip_elective + op_appointments + ae_attendances) %>%
        get_count_model_diagnostics(
          predict(ip_count, newdata = xg_test) +
            predict(op_count, newdata = xg_test) +
            predict(ae_count, newdata = xg_test)
        )

      model_output <- list(
        "model_match" = model_match,
        "diagnostics" = diagnostics,
        "message" = "OK"
      )

      cat('\n\n Done! \n\n')

      return(model_output)
    } else {
      cat('Running Negative Binomial count model...\n')

      # Testing running XGBoost
      xg_predictors <- data.frame(predictor_full_name = predictor_input) %>%
        inner_join(predictor_lookup, by = 'predictor_full_name') %>%
        select(predictor_short_name) %>%
        unlist() %>%
        as.character()

      xg_train <- training_records %>%
        select(xg_predictors) %>%
        mutate_if(is.factor, as.numeric) %>%
        as.matrix()

      xg_test <- patient_records %>%
        select(xg_predictors) %>%
        mutate_if(is.factor, as.numeric) %>%
        as.matrix()

      count <- xgboost(
        xg_train,
        label = patient_records$predict_var,
        nrounds = 100,
        objective = 'count:poisson'
      )

      model_match <- patient_records %>%
        compare_model_output(count, n_predictors, 'count', xg_predictors)

      # Compare distributions for observed and predicted attendance
      diagnostics <- patient_records %>%
        get_count_model_diagnostics(
          predict(count, newdata = xg_test)
        )

      model_output <- list(
        "model_match" = model_match,
        "diagnostics" = diagnostics,
        "message" = "OK"
      )

      cat('\n\n Done! \n\n')

      return(model_output)
    }
  }

  # Free up memory from model run (not sure if this'll work without rm())
  gc()
}
