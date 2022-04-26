# A function to compare output LTC probabilities with observed LTC occurences,
#   calculating chi-square statistic, 95% confidence interval, and significance.
#
# Inputs: real_df - patient-level dataframe containing LTC boolean
#         predict_ip, predict_op, predict_ae - patient-level glm.nb models to
#           predict inpatient/outpatient/A&E attendances
#         n_predict - number of predictor variables
#         cost_histogram - histogram of hospital spends,containing cost [£],
#           count, and hospital type (IP/OP/AE)
#
# Output: dataframe containing expected/observed occurences, ratio, confidence
#           interval, chi-square statistic, and significance.

compare_cost_model_output <- function(
  real_df,
  ip_count,
  op_count,
  ae_count,
  cost_histogram,
  xg_pred
) {

  xg_test <- real_df %>%
    select(xg_pred) %>%
    mutate_if(is.factor, as.numeric) %>%
    as.matrix()

  # Combine patient data with inpatient, outpatient, and A&E predictions and
  #   calculate total predicted hospital attendance per patient
    real_df <- real_df %>%
      bind_cols(
        data.frame(
          op_predict = predict(op_count, newdata = xg_test),
          ip_predict = predict(ip_count, newdata = xg_test),
          ae_predict = predict(ae_count, newdata = xg_test)
        )
      ) %>%
#      filter(total_cost < quantile(total_cost, 0.95)) %>%
      mutate(total_predict = op_predict + ip_predict + ae_predict)

  # Group by area, filter out low-count areas, and calculate expected occurence,
  #   observed occurence, chi-square, and p-value
  real_df <- real_df %>%
    group_by(area_var) %>%
    filter(n() > 10) %>%
    summarise(
      op_observed = sum(as.numeric(op_appointments)),
      op_expected = sum(as.numeric(op_predict)),
      ip_observed = sum(as.numeric(ip_elective)),
      ip_expected = sum(as.numeric(ip_predict)),
      ae_observed = sum(as.numeric(ae_attendances)),
      ae_expected = sum(as.numeric(ae_predict)),
      total_expected = sum(as.numeric(total_predict)),
      observed = sum(as.numeric(total_cost)) / 1e6
    ) %>%
    ungroup()

  # Run Monte Carlo simulation to calculate confidence interval, and add a
  #   significant Y/N flag for different costs
  cat('Running Monte Carlo Cost Estimation Model...\n')

  ae_dist <- cost_histogram %>% get_distribution(type = 'AE')
  op_dist <- cost_histogram %>% get_distribution(type = 'OP')
  ip_dist <- cost_histogram %>% get_distribution(type = 'IP')

  ae_vec <- cost_histogram %>%
    get_distribution(type = 'AE', return = 'vector')
  op_vec <- cost_histogram %>%
    get_distribution(type = 'OP', return = 'vector')
  ip_vec <- cost_histogram %>%
    get_distribution(type = 'IP', return = 'vector')

  real_df <- real_df %>%
    group_by(area_var) %>%
    mutate(
      ae_mc = list(get_cost_monte_carlo(1e2, ae_expected, ae_dist, ae_vec)),
      ip_mc = list(get_cost_monte_carlo(1e2, ip_expected, ip_dist, ip_vec)),
      op_mc = list(get_cost_monte_carlo(1e2, op_expected, op_dist, op_vec)),
      expected = mean(unlist(ip_mc) + unlist(ae_mc) + unlist(op_mc)) / 1e6,
      expected_lower = quantile(
        unlist(ip_mc) + unlist(ae_mc) + unlist(op_mc), 0.05
      ) / 1e6,
      expected_higher = quantile(
        unlist(ip_mc) + unlist(ae_mc) + unlist(op_mc), 0.95
      ) / 1e6
    ) %>%
    select(-ae_mc, -ip_mc, -op_mc)

  real_df %>%
    mutate(
      match_ratio = observed / expected,
      match_higher = observed / expected_higher,
      match_lower = observed / expected_lower,
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
