check_and_transmit_infection <- function(
    agents, room, current_time,
    delta_t,
    growth_rate_nasal_mean, growth_rate_nasal_sd,
    growth_rate_serum_mean, growth_rate_serum_sd,
    clearance_rate_mean, clearance_rate_sd,
    stochastic_noise_mean, stochastic_noise_sd,
    exponential_factor_mean, exponential_factor_sd,
    inflection_point_mean, inflection_point_sd,
    growth_cease_mean, growth_cease_sd,
    nasal_ccap_mean, nasal_ccap_sd,
    serum_ccap_mean, serum_ccap_sd,
    dose_scaling_factor,
    dose_max_efficiency,
    dose_efficiency_at_threshold,  #
    dose_threshold
) {
  # agents in current room
  room_agents <- agents[agents$room == room, ]

  # transmitting agents
  transmitting_agents <- room_agents[
    room_agents$infect_agent &
      room_agents$virus_nasal >= room_agents$infect_threshold,
  ]

  if (nrow(transmitting_agents) > 0) {
    # non-infected agents in room
    non_infected_agents <- room_agents[!room_agents$infect_agent, ]

    # average nasal virus concentration among transmitting agents
    avg_virus_nasal <- mean(transmitting_agents$virus_nasal)

    # average nasal carrying capacity among transmitting agents
    avg_nasal_ccap <- mean(transmitting_agents$nasal_ccap)

    # calculate virus ratio (proportion)
    virus_ratio <- avg_virus_nasal / avg_nasal_ccap

    # adjust dose_threshold to maintain dose_eff at dose_threshold
    threshold_adj <- dose_threshold + (1 / dose_scaling_factor) * log((dose_max_efficiency / dose_efficiency_at_threshold) - 1)

    # adjusted sigmoid function
    dose_eff <- dose_max_efficiency / (1 + exp(-dose_scaling_factor * (virus_ratio - threshold_adj)))

    dose_eff <- min(max(dose_eff, 0), dose_max_efficiency)

    # each non-infected agent
    for (non_inf_agent_id in non_infected_agents$id) {
      non_inf_agent <- agents[agents$id == non_inf_agent_id, ]

      # check if any transmitting agent can infect
      for (transmitting_agent_id in transmitting_agents$id) {
        transmitting_agent <- agents[agents$id == transmitting_agent_id, ]

        # check non-infected agent
        if (transmitting_agent$virus_nasal >= non_inf_agent$infect_threshold) {
          # dose for the non-infected agent
          agents$dose[agents$id == non_inf_agent_id] <- avg_virus_nasal  # Use average virus nasal concentration as the dose

          # infect agent with dose_eff
          agents <- infect_agent(
            agent_id = non_inf_agent_id,
            agents = agents,
            current_time = current_time,
            initial_infection = FALSE,
            delta_t = delta_t,
            dose_eff_override = dose_eff,
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
