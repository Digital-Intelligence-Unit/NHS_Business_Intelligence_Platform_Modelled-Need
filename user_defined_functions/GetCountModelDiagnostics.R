get_count_model_diagnostics <- function(patient_df, prediction) {
  cutoff <- 50
  patient_df <- patient_df %>% filter(predict_var <= cutoff)
  prediction <- prediction[prediction <= cutoff]

  binwidth <- 2
  n_breaks <- seq(0, cutoff + binwidth, by = binwidth)

  count_histogram <- hist(
    patient_df$predict_var, breaks = n_breaks, plot = FALSE
  )
  predicted_histogram <- hist(
    prediction, breaks = n_breaks, plot = FALSE
  )

  data.frame(
    count_x = count_histogram$mids,
    count_y = count_histogram$counts,
    predict_x = predicted_histogram$mids,
    predict_y = predicted_histogram$counts
  )
}
