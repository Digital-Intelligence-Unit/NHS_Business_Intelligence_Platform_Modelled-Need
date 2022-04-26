add_cohort_flag <- function(patient_df, cohort_input) {
  cat('Adding cohort flag...\n')

  n_selected_flag <- FALSE

  char_unfiltered <- patient_df %>%
    select_if(is.character) %>%
    apply(2, unique) %>%
    plyr::ldply(cbind) %>%
    rename(
      variable = .id,
      possible_value = `1`
    ) %>%
    group_by(variable) %>%
    summarise(range = list(possible_value)) %>%
    spread(key = variable, value = range)

  num_unfiltered <- patient_df %>%
    select_if(is.numeric) %>%
    apply(2, range, na.rm = TRUE) %>%
    as.data.frame()

  cohort <- jsonlite::parse_json(jsonlite::parse_json(cohort_input))

  if ('numberSelLtc' %in% names(cohort)) {
    n_selected_flag <- TRUE
  }

  match_names <- match(
    names(cohort),
    get_cohort_dimensions()$dim_full_name
  ) %>%
    na.omit()

  swap_names <- get_cohort_dimensions() %>%
    select(dim_short_name) %>%
    unlist() %>%
    as.character()

  names(cohort) <- swap_names[match_names]

  # LTCs are given as a list of names, change this to correspond to the dummy
  #   array variables
  if ('long_term_conditions' %in% names(cohort)) {
    swap_ltcs <- get_response_lookup() %>%
      filter(response_full_name %in% unlist(cohort$long_term_conditions)) %>%
      select(response_short_name) %>%
      unlist() %>%
      as.character()

      if (n_selected_flag) {
        patient_df <- patient_df %>%
          mutate(
            n_selected_ltc = rowSums(select(., swap_ltcs))
          )

        num_unfiltered <- bind_cols(
          num_unfiltered,
          data.frame(n_selected_ltc = range(patient_df$n_selected_ltc))
        )

        n_selected_ltc_df <- data.frame(
          n_selected_ltc = range(cohort$n_selected_ltc)
        )

        cohort <- c(
          cohort[!(names(cohort) %in% c('n_selected_ltc', 'long_term_conditions'))],
          n_selected_ltc_df
        )
      } else {
        swap_ltc <- rep(1, length(swap_ltcs))
        names(swap_ltc) <- swap_ltcs
        swap_ltc <- bind_rows(swap_ltc, swap_ltc)

        cohort <- c(
          cohort[names(cohort) != 'long_term_conditions'],
          swap_ltc
        )
      }
  }

  # Counting the number of long-term conditions. In the app these max out at
  #   "5+ long-term conditions", so the plus symbol is stripped and set to the
  #   max observed number of long-term conditions
  if ('other_ltc' %in% names(cohort)) {
    ltc_count <- suppressWarnings(
      as.numeric(cohort$other_ltc)
    )

    low_ltc_count <- min(ltc_count, na.rm = TRUE)

    if (anyNA(ltc_count)) {
      high_ltc_count <- max(num_unfiltered$other_ltc)
    } else {
      high_ltc_count <- max(ltc_count)
    }

    cohort$other_ltc <- c(low_ltc_count, high_ltc_count)
  }

  replace_num <- match(
    names(cohort),
    names(num_unfiltered)
  ) %>%
    na.omit()

  replace_char <- match(
    names(cohort),
    names(char_unfiltered)
  ) %>%
    na.omit()

  num_unfiltered[replace_num] <- unlist(
    cohort[names(cohort) %in% names(num_unfiltered)]
  )
  char_unfiltered[replace_char] <- cohort[names(cohort) %in% names(char_unfiltered)]

  patient_df <- patient_df %>% mutate(predict_var = 1)

  for (i in seq_along(names(num_unfiltered))) {
    patient_df <- patient_df %>%
      create_cohort_flag(
        variable = names(num_unfiltered)[i],
        cohort = num_unfiltered[i],
        type = 'numeric'
      )
  }

  for (i in seq_along(names(char_unfiltered))) {
    patient_df <- patient_df %>%
      create_cohort_flag(
        variable = names(char_unfiltered)[i],
        cohort = char_unfiltered[i],
        type = 'character'
      )
  }

  patient_df %>% mutate(
    predict_var = if_else(predict_var == 1, 'Y', 'N')
  )
}
