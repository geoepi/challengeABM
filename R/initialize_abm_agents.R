#' Initialise room-to-room agents
#'
#' Creates donor and contact agents for a room-to-room experiment with
#' random thresholds and parameters.
#'
#' @param num_infected Number of initially infected donors.
#' @param num_non_infected_per_room Number of naive contacts per room.
#' @param num_rooms Total rooms in the experiment.
#' @param nasal_threshold_mean,serum_threshold_mean Means for detection thresholds.
#' @param nasal_threshold_sd,serum_threshold_sd Standard deviations for thresholds.
#' @param infect_threshold_mean,infect_threshold_sd Parameters for infectious threshold.
#' @return Data frame of initialized agents.
initialize_abm_agents <- function(num_infected, num_non_infected_per_room, num_rooms,
                                  nasal_threshold_mean, serum_threshold_mean, nasal_threshold_sd,
                                  serum_threshold_sd, infect_threshold_mean, infect_threshold_sd) {
  # random draws
  nasal_threshold = rnorm(num_infected, nasal_threshold_mean, nasal_threshold_sd)
  serum_threshold = rnorm(num_infected, mean = serum_threshold_mean, sd = serum_threshold_sd)
  infect_threshold = rnorm(num_infected, mean = infect_threshold_mean, sd = infect_threshold_sd)

  # initial infected agents
  infected_agents <- data.frame(
    id = 1:num_infected,
    infection_status = "infected",
    infect_agent = TRUE,
    is_donor = TRUE,
    room = 1,
    time_in_room = 0,
    virus_nasal = 0,
    virus_serum = 0,
    score_t = 0,
    nasal_threshold = nasal_threshold,
    serum_threshold = serum_threshold,
    infect_threshold = infect_threshold,
    dose = NA,
    infectious_t = NA,
    growth_rate_nasal = NA,
    growth_rate_serum = NA,
    clearance_rate = NA,
    stochastic_noise = NA,
    exponential_factor = NA,
    inflection_point_nasal = NA,
    inflection_point_serum = NA,
    inflection_point_absolute_nasal = NA,
    inflection_point_absolute_serum = NA,
    growth_cease = NA,
    nasal_ccap = NA,
    serum_ccap = NA,
    infection_time = 0
  )

  num_non_infected_agents <- num_non_infected_per_room * (num_rooms - 1)
  non_inf_nasal_threshold = rnorm(num_non_infected_agents, nasal_threshold_mean, nasal_threshold_sd)
  non_inf_serum_threshold = rnorm(num_non_infected_agents, mean = serum_threshold_mean, sd = serum_threshold_sd)
  non_inf_infect_threshold = rnorm(num_non_infected_agents, mean = infect_threshold_mean, sd = infect_threshold_sd)

  # non-infected agents
  non_infected_agents <- data.frame(
    id = (num_infected + 1):(num_infected + num_non_infected_agents),
    infection_status = "non_infected",
    infect_agent = FALSE,
    is_donor = FALSE,
    room = rep(2:num_rooms, each = num_non_infected_per_room),
    time_in_room = 0,
    virus_nasal = 0,
    virus_serum = 0,
    score_t = 0,
    nasal_threshold = non_inf_nasal_threshold,
    serum_threshold = non_inf_serum_threshold,
    infect_threshold = non_inf_infect_threshold,
    dose = NA,
    infectious_t = NA,
    growth_rate_nasal = NA,
    growth_rate_serum = NA,
    clearance_rate = NA,
    stochastic_noise = NA,
    exponential_factor = NA,
    inflection_point_nasal = NA,
    inflection_point_serum = NA,
    inflection_point_absolute_nasal = NA,
    inflection_point_absolute_serum = NA,
    growth_cease = NA,
    nasal_ccap = NA,
    serum_ccap = NA,
    infection_time = NA
  )

  agents <- rbind(infected_agents, non_infected_agents)
  return(agents)
}
