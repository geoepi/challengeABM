#' Simulate room-to-room transmission experiment
#'
#' Runs the within-room model for multiple rooms with donor movement between
#' rooms according to the configuration parameters.
#'
#' @param config_file YAML configuration file.
#' @param ... Optional overrides for configuration values.
#' @return List containing agent trajectories and a final results table.
simulate_room_to_room <- function(config_file = NULL, ...) {

  # read configuration file
  config <- read_R2R_config(config_file, ...)

  # get variables from configuration file
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

  # initialize agents
  agents <- initialize_abm_agents(
    num_infected = num_infected,
    num_non_infected_per_room = num_non_infected_per_room,
    num_rooms = num_rooms,
    nasal_threshold_sd = nasal_threshold_sd,
    nasal_threshold_mean = nasal_threshold_mean,
    serum_threshold_mean = serum_threshold_mean,
    serum_threshold_sd = serum_threshold_sd,
    infect_threshold_mean = infect_threshold_mean,
    infect_threshold_sd = infect_threshold_sd
  )

  # initialize final_results dataframe
  final_results <- expand.grid(time = 0:(num_hours - 1), id = agents$id)
  final_results$infection_status <- "non_infected"
  final_results$virus_nasal <- 0
  final_results$virus_serum <- 0
  final_results$room <- NA
  final_results$is_donor <- FALSE
  final_results$score <- 0
  final_results$infectious_t <- NA

  # infect initial agents (donors)
  for (agent_id in 1:num_infected) {
    agents <- infect_agent(
      agent_id = agent_id,
      agents = agents,
      current_time = 0,
      initial_infection = TRUE,
      delta_t = delta_t,
      growth_rate_nasal_mean = growth_rate_nasal_mean,
      growth_rate_nasal_sd = growth_rate_nasal_sd,
      growth_rate_serum_mean = growth_rate_serum_mean,
      growth_rate_serum_sd = growth_rate_serum_sd,
      clearance_rate_mean = clearance_rate_mean,
      clearance_rate_sd = clearance_rate_sd,
      stochastic_noise_mean = stochastic_noise_mean,
      stochastic_noise_sd = stochastic_noise_sd,
      exponential_factor_mean = exponential_factor_mean,
      exponential_factor_sd = exponential_factor_sd,
      inflection_point_mean = inflection_point_mean,
      inflection_point_sd = inflection_point_sd,
      growth_cease_mean = growth_cease_mean,
      growth_cease_sd = growth_cease_sd,
      nasal_ccap_mean = nasal_ccap_mean,
      nasal_ccap_sd = nasal_ccap_sd,
      serum_ccap_mean = serum_ccap_mean,
      serum_ccap_sd = serum_ccap_sd
    )
  }

  # main simulation loop
  for (current_time in 0:(num_hours - 1)) {

    # move donors if stay completed
    donor_ids <- which(agents$is_donor)
    for (donor_id in donor_ids) {
      if (agents$time_in_room[donor_id] >= donor_move_interval && agents$room[donor_id] < num_rooms) {
        # move donor to next room
        agents$room[donor_id] <- agents$room[donor_id] + 1
        agents$time_in_room[donor_id] <- 0  # time_in_room
      }
    }

    # increment time_in_room
    agents$time_in_room <- agents$time_in_room + 1

    # virus dynamics for infected
    for (agent_id in which(agents$infect_agent)) {
      agent <- agents[agent_id, , drop = FALSE]  # Ensure agent is a data frame
      agent <- update_agent_virus_dynamics(agent, current_time, delta_t)
      agents[agent_id, ] <- agent

      # update
      idx <- final_results$id == agent_id & final_results$time == current_time
      final_results$virus_nasal[idx] <- agent$virus_nasal
      final_results$virus_serum[idx] <- agent$virus_serum
      final_results$infection_status[idx] <- "infected"

      # record infectious time, if infectious
      if (is.na(agents$infectious_t[agent_id]) && agent$virus_nasal >= agent$infect_threshold) {
        agents$infectious_t[agent_id] <- current_time
      }

      # update infectious time
      if (!is.na(agents$infectious_t[agent_id])) {
        final_results$infectious_t[idx] <- agents$infectious_t[agent_id]
      }

      # check for symptoms and update score
      if (agents$score_t[agent_id] == 0 && (agent$virus_nasal >= agent$nasal_threshold && agent$virus_serum >= agent$serum_threshold)) {

        agents$score_t[agent_id] <- current_time  # time symptoms appear
        final_results$score[final_results$id == agent_id & final_results$time >= current_time] <- 1
      }
    }

    # add room numbers and donor status
    for (agent_id in agents$id) {
      idx <- final_results$id == agent_id & final_results$time == current_time
      final_results$room[idx] <- agents$room[agent_id]
      final_results$is_donor[idx] <- agents$is_donor[agent_id]
    }

    # transmission within each room
    for (room in unique(agents$room)) {
      agents <- check_and_transmit_infection(
        agents = agents,
        room = room,
        current_time = current_time,
        delta_t = delta_t,
        dose_scaling_factor = dose_scaling_factor,
        dose_max_efficiency = dose_max_efficiency,
        dose_efficiency_at_threshold = dose_efficiency_at_threshold,
        dose_threshold = dose_threshold,
        growth_rate_nasal_mean = growth_rate_nasal_mean,
        growth_rate_nasal_sd = growth_rate_nasal_sd,
        growth_rate_serum_mean = growth_rate_serum_mean,
        growth_rate_serum_sd = growth_rate_serum_sd,
        clearance_rate_mean = clearance_rate_mean,
        clearance_rate_sd = clearance_rate_sd,
        stochastic_noise_mean = stochastic_noise_mean,
        stochastic_noise_sd = stochastic_noise_sd,
        exponential_factor_mean = exponential_factor_mean,
        exponential_factor_sd = exponential_factor_sd,
        inflection_point_mean = inflection_point_mean,
        inflection_point_sd = inflection_point_sd,
        growth_cease_mean = growth_cease_mean,
        growth_cease_sd = growth_cease_sd,
        nasal_ccap_mean = nasal_ccap_mean,
        nasal_ccap_sd = nasal_ccap_sd,
        serum_ccap_mean = serum_ccap_mean,
        serum_ccap_sd = serum_ccap_sd
      )
    }
  }

  # summary stats
  agents <- process_final_results(agents)

  return(list(agents = agents, final_results = final_results))
}
