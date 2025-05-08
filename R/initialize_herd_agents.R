initialize_herd_agents <- function(num_donors, total_herd_size,
                                   nasal_threshold_mean, serum_threshold_mean, nasal_threshold_sd,
                                   serum_threshold_sd, infect_threshold_mean, infect_threshold_sd,
                                   recovery_time_mean, recovery_time_sd) {
  num_non_infected <- total_herd_size - num_donors

  # generate thresholds and recovery times
  nasal_threshold <- rnorm(total_herd_size, nasal_threshold_mean, nasal_threshold_sd)
  serum_threshold <- rnorm(total_herd_size, serum_threshold_mean, serum_threshold_sd)
  infect_threshold <- rnorm(total_herd_size, infect_threshold_mean, infect_threshold_sd)
  recovery_time <- rnorm(total_herd_size, recovery_time_mean, recovery_time_sd)

  # recovery_time must be positive
  recovery_time <- pmax(recovery_time, 0)

  # initialize agents data frame
  agents <- data.frame(
    id = 1:total_herd_size,
    infection_status = rep("non_infected", total_herd_size),
    infect_agent = rep(FALSE, total_herd_size),
    is_donor = rep(FALSE, total_herd_size),
    virus_nasal = rep(0, total_herd_size),
    virus_serum = rep(0, total_herd_size),
    score_t = rep(0, total_herd_size),
    nasal_threshold = nasal_threshold,
    serum_threshold = serum_threshold,
    infect_threshold = infect_threshold,
    dose = rep(NA, total_herd_size),
    infectious_t = rep(NA, total_herd_size),
    growth_rate_nasal = rep(NA, total_herd_size),
    growth_rate_serum = rep(NA, total_herd_size),
    clearance_rate = rep(NA, total_herd_size),
    stochastic_noise = rep(NA, total_herd_size),
    exponential_factor = rep(NA, total_herd_size),
    inflection_point_nasal = rep(NA, total_herd_size),
    inflection_point_serum = rep(NA, total_herd_size),
    inflection_point_absolute_nasal = rep(NA, total_herd_size),
    inflection_point_absolute_serum = rep(NA, total_herd_size),
    growth_cease = rep(NA, total_herd_size),
    nasal_ccap = rep(NA, total_herd_size),
    serum_ccap = rep(NA, total_herd_size),
    infection_time = rep(NA, total_herd_size),
    score = rep(0, total_herd_size),
    recovery_time = recovery_time,
    has_recovered = rep(FALSE, total_herd_size),
    infector_id = rep(NA, total_herd_size)
  )

  if (num_donors > 0) {
    agents$infection_status[1:num_donors] <- "infected"
    agents$infect_agent[1:num_donors] <- TRUE
    agents$is_donor[1:num_donors] <- TRUE
    agents$infection_time[1:num_donors] <- 0
  }

  return(agents)
}
