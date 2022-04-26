create_cohort_flag <- function(patient_df, variable, cohort, type) {
  cohort <- cohort[names(cohort) %in% variable] %>% unlist()

  if (type == 'numeric') {
    cohort <- as.numeric(cohort)
    cohort <- cohort[order(cohort)]

    if (length(cohort) == 1) {
      cohort <- c(cohort, cohort)
    }

    numeric_patient_data <- patient_df %>%
      select(variable) %>%
      unlist() %>%
      as.numeric()

    yes_no <- if_else(
      between(numeric_patient_data, cohort[1], cohort[2]),
      1,
      0
    )

    patient_df %>% mutate(
      predict_var = yes_no * predict_var
    ) %>%
    return()
  } else if (type == 'character') {
    character_patient_data <- patient_df %>%
      select(variable) %>%
      unlist() %>%
      as.character()

    yes_no <- if_else(character_patient_data %in% cohort, 1, 0)

    patient_df %>% mutate(
        predict_var = yes_no * predict_var
      ) %>%
      return()
  }
}
