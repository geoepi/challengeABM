read_config <- function(config_file = NULL, ...) {

    if (!is.null(config_file) && is.character(config_file)) {

      config <- yaml::read_yaml(config_file)
    } else {
      config <- list()
    }

    args <- list(...)
    config <- modifyList(config, args)

    num_infected <- config$num_infected
    num_non_infected_per_room <- config$num_non_infected_per_room
    num_hours <- config$num_hours
    delta_t <- config$delta_t
    dose_shape1 <- config$dose_shape1
    dose_shape2 <- config$dose_shape2
    initial_virus_nasal <- config$initial_virus_nasal
    initial_virus_serum <- config$initial_virus_serum
    k_dose <- config$k_dose
    nasal_threshold_mean <- config$nasal_threshold_mean
    nasal_shape <- config$nasal_shape
    serum_threshold_mean <- config$serum_threshold_mean
    serum_threshold_sd <- config$serum_threshold_sd
    infect_threshold_mean <- config$infect_threshold_mean
    infect_threshold_sd <- config$infect_threshold_sd
    growth_rate_nasal_mean <- config$growth_rate_nasal_mean
    growth_rate_nasal_sd <- config$growth_rate_nasal_sd
    growth_rate_serum_mean <- config$growth_rate_serum_mean
    growth_rate_serum_sd <- config$growth_rate_serum_sd
    clearance_rate_mean <- config$clearance_rate_mean
    clearance_rate_sd <- config$clearance_rate_sd
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
  }
