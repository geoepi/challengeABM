simulate_within_herd <- function(config_file = NULL, ...) {

  # read configuration file
  config <- read_herd_config(config_file, ...)

  # get variables from config
  num_donors <- config$num_donors
  total_herd_size <- config$total_herd_size
  num_hours <- config$num_hours
  delta_t <- config$delta_t
  contact_rate <- config$contact_rate
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
  dose_scaling_factor <- config$dose_scaling_factor
  dose_max_efficiency <- config$dose_max_efficiency
  dose_efficiency_at_threshold <- config$dose_efficiency_at_threshold
  dose_threshold <- config$dose_threshold
  preclin_infect <- config$preclin_infect
  recovery_time_mean <- config$recovery_time_mean
  recovery_time_sd <- config$recovery_time_sd

  # initialize agents
  agents <- initialize_herd_agents(
    num_donors = num_donors,
    total_herd_size = total_herd_size,
    nasal_threshold_sd = nasal_threshold_sd,
    nasal_threshold_mean = nasal_threshold_mean,
    serum_threshold_mean = serum_threshold_mean,
    serum_threshold_sd = serum_threshold_sd,
    infect_threshold_mean = infect_threshold_mean,
    infect_threshold_sd = infect_threshold_sd,
    recovery_time_mean = recovery_time_mean*24, # to hours
    recovery_time_sd = recovery_time_sd*24 # to hours
  )

  # initialize final_results
  final_results <- expand.grid(time = 0:(num_hours - 1), id = agents$id)
  final_results$infection_status <- "non_infected"
  final_results$virus_nasal <- 0
  final_results$virus_serum <- 0
  final_results$is_donor <- FALSE
  final_results$score <- 0
  final_results$infectious_t <- NA
  final_results$infector_id <- NA

  # infect initial donor agents
  for (agent_id in 1:num_donors) {
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

    # 1. update virus for infected agents
    for (agent_id in which(agents$infect_agent)) {
      agent <- agents[agent_id, , drop = FALSE]
      agent <- update_agent_virus_dynamics(agent, current_time, delta_t)
      agents[agent_id, ] <- agent

      # record to final_results
      idx <- final_results$id == agent_id & final_results$time == current_time
      final_results$virus_nasal[idx] <- agent$virus_nasal
      final_results$virus_serum[idx] <- agent$virus_serum
      final_results$infection_status[idx] <- agents$infection_status[agent_id]
      final_results$infector_id[idx] <- agents$infector_id[agent_id]

      # record infectious time
      if (preclin_infect && is.na(agents$infectious_t[agent_id]) && agent$virus_nasal >= agent$infect_threshold) {
        agents$infectious_t[agent_id] <- current_time
      }

      # update infectious time
      if (!is.na(agents$infectious_t[agent_id])) {
        final_results$infectious_t[idx] <- agents$infectious_t[agent_id]
      }

      # check for symptoms and update score
      if (agents$score_t[agent_id] == 0 && (agent$virus_nasal >= agent$nasal_threshold && agent$virus_serum >= agent$serum_threshold)) {

        agents$score_t[agent_id] <- current_time  # time symptoms appear
        agents$score[agent_id] <- 1  # agent's score to 1 (clinical symptoms)
        final_results$score[final_results$id == agent_id & final_results$time >= current_time] <- 1

        # if preclin_infect is FALSE, agents become infectious when symptoms appear
        if (!preclin_infect && is.na(agents$infectious_t[agent_id])) {
          agents$infectious_t[agent_id] <- current_time
        }
      }
    }

    # 2. check for recovery
    for (agent_id in which(agents$infect_agent)) {
      agent <- agents[agent_id, , drop = FALSE]

      time_since_infection <- current_time - agent$infection_time

      if (time_since_infection >= agent$recovery_time &&
          agent$virus_nasal < agent$infect_threshold &&
          agent$virus_serum < agent$serum_threshold) {

        agents$infection_status[agent_id] <- "recovered"
        agents$score[agent_id] <- 0
        agents$score_t[agent_id] <- NA
        agents$infect_agent[agent_id] <- FALSE
        agents$has_recovered[agent_id] <- TRUE
      }
    }

    # 3. update donor status
    for (agent_id in agents$id) {
      idx <- final_results$id == agent_id & final_results$time == current_time
      final_results$is_donor[idx] <- agents$is_donor[agent_id]
      final_results$infection_status[idx] <- agents$infection_status[agent_id]
    }

    # 4. simulate contacts and transmissions
    agents <- simulate_herd_contacts_and_transmissions(
      agents = agents,
      current_time = current_time,
      contact_rate = contact_rate,
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
      serum_ccap_sd = serum_ccap_sd,
      preclin_infect = preclin_infect
    )
  }

  # postprocess final results
  agents <- process_final_results(agents)

  return(list(agents = agents, final_results = final_results))
}
