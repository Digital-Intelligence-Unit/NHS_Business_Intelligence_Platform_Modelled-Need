# A function to mutate the format of a patient dataframe into one used by a GLM.
#
# Inputs: patient_df - patient-level dataframe containing LTC booleans,
#                         predictor variables, and area variables
#         area_group - Area level by which to aggregate predictions
#         ltc_predict- LTC(s) to predict on
#         age_factor - binary flag to convert age to five-year age bins
#
# Output: patient_df - patient-level dataframe with mutated variables

format_glm_data <- function(
  patient_df, area_group, response_predict, age_factor
) {
  # Add flag for patients with no long-term conditions, change logical LTCs to
  #   binary, combine CCG and locality, and filter out GP Practices with codes
  #   starting with Y
  patient_df <- patient_df %>%
    mutate_if(is.logical, as.integer) %>%
    mutate(
      area_var = !! sym(area_group)
    )

  # Create "predict_var" to use in GLM, depending on response variable provided
  # If response variable = LTC(s) then change to binary
  # Combine LTCs if two are used
  patient_df <- patient_df %>%
    rowwise() %>%
    mutate(predict_var = if_else(
      prod(!!! syms(response_predict)) == 1, 'Y', 'N'
    )) %>%
    ungroup() %>%
    select(-!! response_predict, -c(area_group))

  # Remove response variable, calculate count of other LTCs per patient,
  #   logarithmically re-scale IP/OP/AE appointments, and remove bad values
  patient_df <- patient_df %>%
    mutate_if(is.character, as.factor) %>%
    filter(!is.na(predict_var) & !is.na(area_var)) %>%
    drop_na()

  # Add age banding if desired
  if(age_factor == 'Y') {
    patient_df <- patient_df %>%
      mutate(
        age = pmax(
          pmin(age, config$max_age),
          config$min_age
        ),
        age = as.factor(
          config$age_band_width * ceiling(age / config$age_band_width)
        )
      )
  }

  # Return dataframe
  patient_df
}
