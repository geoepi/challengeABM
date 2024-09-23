left_skewed_gamma <- function(desired_mean, shape, size = 1) {

  scale <- desired_mean / shape

  gamma_sample <- rgamma(size, shape, scale)

  left_skewed_sample <- (desired_mean) - gamma_sample

  return(left_skewed_sample)
}
