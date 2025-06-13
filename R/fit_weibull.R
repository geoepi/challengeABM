#' Fit a Weibull distribution from quantiles
#'
#' Utility for deriving shape and scale parameters given a median and
#' confidence interval.
#'
#' @param median Median value of the distribution.
#' @param ci Numeric vector of length two with lower and upper quantiles.
#' @return List containing `shape` and `scale`.
fit_weibull <- function(median, ci) {
  obj_fn <- function(par) {
    shape <- par[1]; scale <- par[2]
    q25  <- qweibull(0.025, shape, scale)
    q50  <- qweibull(0.5,   shape, scale)
    q975 <- qweibull(0.975, shape, scale)
    (q50 - median)^2 + (q25 - ci[1])^2 + (q975 - ci[2])^2
  }
  opt <- optim(c(2, median), function(p) sum(obj_fn(p)), method = "L-BFGS-B", lower = c(0.1, 0.1))
  list(shape = opt$par[1], scale = opt$par[2])
}
