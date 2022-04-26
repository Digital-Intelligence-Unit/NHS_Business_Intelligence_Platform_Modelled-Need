get_logistic_model_diagnostics <- function(patient_df, probability_vector) {
  positive_class <- probability_vector[patient_df$predict_var == 'Y']
  negative_class <- probability_vector[patient_df$predict_var == 'N']

  roc <- roc.curve(
    scores.class0 = positive_class,
    scores.class1 = negative_class,
    curve = TRUE
  )

  precision_recall <- pr.curve(
    scores.class0 = positive_class,
    scores.class1 = negative_class,
    curve = TRUE
  )

  roc_auc <- roc$auc
  pr_auc <- precision_recall$auc.integral

  roc <- roc$curve %>%
    as.data.frame() %>%
    transmute(
      type = 'ROC',
      auc = roc_auc,
      false_positive = V1,
      true_positive = V2,
      cutoff = V3
    ) %>%
    distinct()

  precision_recall <- precision_recall$curve %>%
    as.data.frame() %>%
    transmute(
      type = 'PR',
      auc = pr_auc,
      recall = V1,
      precision = V2,
      cutoff = V3
    ) %>%
    distinct()

  list("pr" = precision_recall, "roc" = roc)
}
