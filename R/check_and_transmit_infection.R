# check for transmission in the current room

check_and_transmit_infection <- function(agents, room, current_time,k_dose,
                                         initial_virus_nasal, initial_virus_serum,
                                         delta_t, dose_shape1, dose_shape2,
                                         growth_rate_nasal_mean, growth_rate_nasal_sd,
                                         growth_rate_serum_mean, growth_rate_serum_sd,
                                         clearance_rate_mean, clearance_rate_sd,
                                         stochastic_noise_mean, stochastic_noise_sd,
                                         exponential_factor_mean, exponential_factor_sd,
                                         inflection_point_mean, inflection_point_sd,
                                         growth_cease_mean, growth_cease_sd,
                                         nasal_ccap_mean, nasal_ccap_sd,
                                         serum_ccap_mean, serum_ccap_sd) {
  # agents in the current room
  room_agents <- agents[agents$room == room, ]

  # ID transmitting agents
  transmitting_agents <- room_agents[room_agents$infect_agent & room_agents$virus_nasal >= room_agents$infect_threshold, ]

  if (nrow(transmitting_agents) > 0) {
    # get non-infected agents
    non_infected_agents <- room_agents[!room_agents$infect_agent, ]

    for (non_inf_agent_id in non_infected_agents$id) {
      non_inf_agent <- agents[agents$id == non_inf_agent_id, ]

      for (transmitting_agent_id in transmitting_agents$id) {
        transmitting_agent <- agents[agents$id == transmitting_agent_id, ]
        # check if transmitting agent's virus_nasal >= infect_threshold of non-infected agent
        if (transmitting_agent$virus_nasal >= non_inf_agent$infect_threshold) {

          agents$dose[agents$id == non_inf_agent_id] <- transmitting_agent$virus_nasal
          # infect the non-infected agent
          agents <- infect_agent(
            agent_id = non_inf_agent_id,
            agents = agents,
            current_time = current_time,
            initial_infection = FALSE,
            initial_virus_nasal = NULL,
            initial_virus_serum = NULL,  # not used here, earlier version
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

          break
        }
      }
    }
  }

  return(agents)
}
