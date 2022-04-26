get_distribution <- function(hist_df, type, dist = 'kernel', return = '') {
  hist_df <- hist_df %>%
    filter(type == type & cost > 0) %>%
    select(cost, count)

  cost_df <- rep(hist_df[, 1], hist_df[, 2])

  if (return == 'vector') {
    cost_df
  } else if (dist != 'kernel') {
    fitdistr(cost_df, dist)
  } else {
    density(cost_df, from = 1, to = max(cost_df) + 1)
  }
}
