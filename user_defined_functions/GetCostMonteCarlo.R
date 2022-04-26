get_cost_monte_carlo <- function(
  rounds, n_patients, kernel_distribution, kernel_base
) {
  lapply(
    1 : rounds,
    function(i, n, real_vector, sd) {
      sum(pmax(
        rnorm(
          n,
          mean = sample(real_vector, n_patients, replace = TRUE),
          sd = sd
        ),
        0))
    },
    n = n_patients,
    real_vector = kernel_base,
    sd = kernel_distribution$bw
  )
}
