#' Generate state transition probabilities
#'
#' Uses Weibull distributions specified in `config` to build a table of hourly
#' probabilities for noninfectious, preclinical and clinical states.
#'
#' @param config Named list of parameter values.
#' @return Data frame of state probabilities over time.
generate_state_probability_table <- function(config) {

  # maximum time span in days
  time_max_days <- config$num_hours / 24

  # extract Weibull median and CI parameters from config
  preclin_onset_median      <- config$preclin_onset_median
  preclin_onset_ci          <- config$preclin_onset_ci
  preclin_duration_median   <- config$preclin_duration_median
  preclin_duration_ci       <- config$preclin_duration_ci
  clinical_duration_median  <- config$clinical_duration_median
  clinical_duration_ci      <- config$clinical_duration_ci

  # Weibull quantile-fitting
  fit_weibull <- function(median, ci) {
    obj_fn <- function(par) {
      shape <- par[1]
      scale <- par[2]
      q25 <- qweibull(0.025, shape, scale)
      q50 <- qweibull(0.5, shape, scale)
      q975 <- qweibull(0.975, shape, scale)
      sum((q50 - median)^2 + (q25 - ci[1])^2 + (q975 - ci[2])^2)
    }
    opt <- optim(par = c(2, median), fn = obj_fn, method = "L-BFGS-B", lower = c(0.1, 0.1))
    list(shape = opt$par[1], scale = opt$par[2])
  }

  # Weibull parameters and draw one value for each
  onset_preclin_params <- fit_weibull(preclin_onset_median, preclin_onset_ci)
  dur_preclin_params   <- fit_weibull(preclin_duration_median, preclin_duration_ci)
  dur_clin_params      <- fit_weibull(clinical_duration_median, clinical_duration_ci)

  preclin_onset_day     <- rweibull(1, onset_preclin_params$shape, onset_preclin_params$scale)
  preclin_duration_day  <- rweibull(1, dur_preclin_params$shape, dur_preclin_params$scale)
  clinical_onset_day    <- preclin_onset_day + preclin_duration_day
  clinical_duration_day <- rweibull(1, dur_clin_params$shape, dur_clin_params$scale)

  # time sequence (1-hour steps)
  time_seq <- seq(0, time_max_days * 24, by = 1)

  # Sigmoid curve
  sigmoid <- function(t, onset, duration) {
    steepness <- 4 / (duration * 24)
    1 / (1 + exp(-steepness * (t - onset * 24)))
  }

  # cumulative probabilities
  F_preclin  <- sigmoid(time_seq, preclin_onset_day, preclin_duration_day)
  F_clinical <- sigmoid(time_seq, clinical_onset_day, clinical_duration_day)

  # state probabilities
  P_noninfectious <- 1 - F_preclin
  P_preclinical   <- pmax(0, F_preclin - F_clinical)
  P_clinical      <- F_clinical

  # return transition table
  data.frame(
    time_hours = time_seq,
    P_noninfectious = P_noninfectious,
    P_preclinical   = P_preclinical,
    P_clinical      = P_clinical
  )
}
