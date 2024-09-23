# main workflow
simulate_abm_with_transmission <- function(num_infected, num_non_infected_per_room, num_hours,
                                           infect_threshold_mean, infect_threshold_sd,
                                           delta_t = 1, dose_shape1, dose_shape2,
                                           initial_virus_nasal, initial_virus_serum, k_dose,
                                           growth_rate_nasal_mean, growth_rate_nasal_sd,
                                           growth_rate_serum_mean, growth_rate_serum_sd,
                                           clearance_rate_mean, clearance_rate_sd, nasal_shape,
                                           stochastic_noise_mean, stochastic_noise_sd,
                                           exponential_factor_mean, exponential_factor_sd,
                                           inflection_point_mean = 4, inflection_point_sd = 0.5,
                                           growth_cease_mean = 240, growth_cease_sd = 10,
                                           nasal_ccap_mean = 10, nasal_ccap_sd = 0.5,
                                           serum_ccap_mean = 10, serum_ccap_sd = 0.5,
                                           num_rooms = 5, donor_move_interval = 24,
                                           nasal_threshold_mean,
                                           serum_threshold_mean, serum_threshold_sd) {

  # initialize agents
  agents <- initialize_abm_agents(num_infected = num_infected,
                                  num_non_infected_per_room = num_non_infected_per_room,
                                  num_rooms = num_rooms, nasal_shape = nasal_shape,
                                  initial_infected_virus_nasal = initial_virus_nasal,
                                  initial_infected_virus_serum = initial_virus_serum,
                                  nasal_threshold_mean = nasal_threshold_mean,
                                  serum_threshold_mean = serum_threshold_mean,
                                  serum_threshold_sd = serum_threshold_sd,
                                  infect_threshold_mean = infect_threshold_mean,
                                  infect_threshold_sd = infect_threshold_sd)

  # initialize final_results
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
      initial_virus_nasal = initial_virus_nasal,
      initial_virus_serum = initial_virus_serum,
      delta_t = delta_t,
      dose_shape1 = dose_shape1,
      dose_shape2 = dose_shape2,
      k_dose=k_dose,
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

  # simulate each hour
  for (current_time in 0:(num_hours - 1)) {

    agents$time_in_room <- agents$time_in_room + 1
    # update virus dynamics for infected agents
    for (agent_id in which(agents$infect_agent)) {
      agent <- agents[agent_id, ]
      agent <- update_agent_virus_dynamics(agent, current_time, delta_t)
      agents[agent_id, ] <- agent

      # update final_results
      idx <- final_results$id == agent_id & final_results$time == current_time
      final_results$virus_nasal[idx] <- agent$virus_nasal
      final_results$virus_serum[idx] <- agent$virus_serum
      final_results$infection_status[idx] <- "infected"

      if (is.na(agent$infectious_t) && agent$virus_nasal >= agent$infect_threshold) {
        agent$infectious_t <- current_time  # time agent becomes infectious
        agents$infectious_t[agent_id] <- agent$infectious_t
      }

      # update final_results with infectious_t
      idx <- final_results$id == agent_id & final_results$time == current_time
      if (!is.na(agents$infectious_t[agent_id])) {
        final_results$infectious_t[idx] <- agents$infectious_t[agent_id]
      }

      # check if clinical symptoms occur and update score if needed
      if (agent$score_t == 0) {
        if (agent$virus_nasal >= agent$nasal_threshold || agent$virus_serum >= agent$serum_threshold) {
          # symptoms detected
          agent$score_t <- current_time  # Record the time symptoms appear
          agents$score_t[agent_id] <- agent$score_t

          # update final_results with score = 1
          final_results$score[final_results$id == agent_id & final_results$time >= current_time] <- 1
        }
      }

      # check if the agent becomes infectious
      if (is.na(agent$infectious_t) && agent$virus_nasal >= agent$infect_threshold) {
        agent$infectious_t <- current_time  # record time the agent becomes infectious
        agents$infectious_t[agent_id] <- agent$infectious_t
      }
    }

    # move donors every donor_move_interval hours
    if ((current_time + 1) %% donor_move_interval == 0 && agents$room[1] < num_rooms) {
      agents$room[agents$id <= num_infected] <- agents$room[agents$id <= num_infected] + 1
      agents$time_in_room[agents$id <= num_infected] <- 0
    }

    # record room numbers in final_results
    for (agent_id in agents$id) {
      idx <- final_results$id == agent_id & final_results$time == current_time
      final_results$room[idx] <- agents$room[agent_id]
      final_results$is_donor[idx] <- agents$is_donor[agent_id]
    }

    # perform transmission within each room
    for (room in unique(agents$room)) {
      agents <- check_and_transmit_infection(
        agents = agents,
        room = room,
        current_time = current_time,
        initial_virus_nasal = initial_virus_nasal,
        initial_virus_serum = initial_virus_serum,
        delta_t = delta_t,
        dose_shape1 = dose_shape1,
        dose_shape2 = dose_shape2,
        k_dose = k_dose,
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

  agents <- process_final_results(agents)

  return(list(agents = agents, final_results = final_results))
}
