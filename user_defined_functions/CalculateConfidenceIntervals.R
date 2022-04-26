calculate_confidence_intervals <- function(index_of_inequality, index_name) {
  # Calculate Index-of-Inequality confidence intervals and p-value
  avg_ii <- mean(index_of_inequality, na.rm = TRUE)
  sd_ii <- sd(index_of_inequality, na.rm = TRUE)
  
  ii_lower <- avg_ii - sd_ii * 1.96
  ii_upper <- avg_ii + sd_ii * 1.96
  
  ii_z_val <- if_else(
    index_name == "SII", 
    abs(avg_ii / sd_ii),
    abs(log(avg_ii) / log(sd_ii))
  )
  ii_p_val <- pmin(
    exp(-ii_z_val * (0.717 + ii_z_val * 0.416)),
    1
  )

  # Return confidence intervals and p-values
  list(c(
    ii_lower, 
    ii_upper,
    ii_p_val
  ))  
}
