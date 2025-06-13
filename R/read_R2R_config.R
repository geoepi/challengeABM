#' Read room-to-room configuration
#'
#' Loads a YAML configuration file and allows overriding parameters via
#' additional arguments.
#'
#' @param config_file Path to a configuration YAML file.
#' @param ... Named values to override in the configuration.
#' @return A list of configuration parameters.
read_R2R_config <- function(config_file = NULL, ...) {

  if (!is.null(config_file) && is.character(config_file)) {
    config <- yaml::read_yaml(config_file)
  } else {
    config <- list()
  }

  args <- list(...)
  config <- modifyList(config, args)

  # get parameters from the configuration file
  num_infected <- config$num_infected
  num_non_infected_per_room <- config$num_non_infected_per_room
  num_hours <- config$num_hours
  delta_t <- config$delta_t
  growth_rate_nasal_mean <- config$growth_rate_nasal_mean
  growth_rate_nasal_sd <- config$growth_rate_nasal_sd
  growth_rate_serum_mean <- config$growth_rate_serum_mean
  growth_rate_serum_sd <- config$growth_rate_serum_sd
  clearance_rate_mean <- config$clearance_rate_mean
  clearance_rate_sd <- config$clearance_rate_sd
  nasal_threshold_mean <- config$nasal_threshold_mean
  nasal_threshold_sd <- config$nasal_threshold_sd
  serum_threshold_mean <- config$serum_threshold_mean
  serum_threshold_sd <- config$serum_threshold_sd
  infect_threshold_mean <- config$infect_threshold_mean
  infect_threshold_sd <- config$infect_threshold_sd
  stochastic_noise_mean <- config$stochastic_noise_mean
  stochastic_noise_sd <- config$stochastic_noise_sd
  exponential_factor_mean <- config$exponential_factor_mean
  exponential_factor_sd <- config$exponential_factor_sd
  inflection_point_mean <- config$inflection_point_mean
  inflection_point_sd <- config$inflection_point_sd
  growth_cease_mean <- config$growth_cease_mean
  growth_cease_sd <- config$growth_cease_sd
  nasal_ccap_mean <- config$nasal_ccap_mean
  nasal_ccap_sd <- config$nasal_ccap_sd
  serum_ccap_mean <- config$serum_ccap_mean
  serum_ccap_sd <- config$serum_ccap_sd
  num_rooms <- config$num_rooms
  donor_move_interval <- config$donor_move_interval
  dose_scaling_factor <- config$dose_scaling_factor
  dose_max_efficiency <- config$dose_max_efficiency
  dose_efficiency_at_threshold <- config$dose_efficiency_at_threshold
  dose_threshold <- config$dose_threshold

  return(config)
}
