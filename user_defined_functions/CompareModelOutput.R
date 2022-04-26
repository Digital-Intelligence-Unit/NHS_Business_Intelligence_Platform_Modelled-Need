# A function to compare output LTC probabilities with observed LTC occurences,
#   calculating chi-square statistic, 95% confidence interval, and significance.
#
# Inputs: real_df - patient-level dataframe containing LTC boolean
#         predict_vector - dependant upon model chosen, one of
#           ~ patient-level LTC probabilities (for logistic model)
#           ~ glm.nb model for count predictions (for count model)
#         n_predict - number of predictor variables
#         model_type - model used to predict response ~ 'logistic' or 'count'
#
# Output: dataframe containing expected/observed occurences, ratio, confidence
#           interval, chi-square statistic, and significance.

compare_model_output <- function(
  real_df, predict_vector, n_predict, model_type, xg_pred = NULL
) {

  if (model_type == 'logistic') {
    # Combine patient data with LTC probability and re-format predict_var from
    #   binary factor (1, 2) to simple binary (0, 1)
    real_df <- real_df %>%
      bind_cols(
        data.frame(predicted = predict_vector)
      ) %>%
      mutate(actual = as.numeric(predict_var) - 1)
  } else {
    # Combine patient data with inpatient, outpatient, and A&E predictions and
    #   calculate total predicted hospital attendance per patient
    xg_train <- real_df %>%
      select(xg_pred) %>%
      mutate_if(is.factor, as.numeric) %>%
      as.matrix()

    real_df <- real_df %>%
      bind_cols(
        data.frame(
          predicted = predict(predict_vector, newdata = xg_train)
        )
      ) %>%
      rename(actual = predict_var)
  }

  # Group by area, filter out low-count areas, and calculate expected occurence,
  #   observed occurence, chi-square, and p-value
  real_df <- real_df %>%
    group_by(area_var) %>%
    filter(n() > 10) %>%
    summarise(
      expected = sum(as.numeric(predicted), na.rm = TRUE),
      observed = sum(as.numeric(actual), na.rm = TRUE),
      match_ratio = (observed / expected),
      chi_square = (observed - expected)^2 / expected,
      p_value = pchisq(
        chi_square, df = (n() - 1)*(n_predict - 1), lower.tail = FALSE
      )
    ) %>%
    ungroup()

  # Calculate confidence interval, significance, and add a significant Y/N flag
  real_df %>%
    mutate(
      match_lower = qchisq(0.05, 2 * observed) / (2 * expected),
      match_higher = qchisq(0.95, 2 * observed + 2) / (2 * expected),
      expected_lower = observed / match_lower,
      expected_higher = observed / match_higher,
      significant = if_else(
        (sign((match_higher - 1) / (match_ratio - 1)) == 1) &
          (sign((match_lower - 1) / (match_ratio - 1)) == 1),
        'Yes',
        'No'
      ),
      significance = if_else(
        significant == 'No',
        0,
        pmin(
          abs(match_higher - 1),
          abs(match_lower - 1)
        )
      )
    )
}
