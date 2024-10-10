simulate_herd_contacts_and_transmissions <- function(
    agents, current_time, contact_rate, delta_t,
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
    dose_efficiency_at_threshold,
    dose_threshold,
    preclin_infect
) {

  # id infectiousness based on preclinical toggle
  if (preclin_infect) {
    # infectious once virus_nasal >= infect_threshold
    infectious_agents <- agents[
      agents$infect_agent &
        agents$infection_status == "infected" &
        agents$virus_nasal >= agents$infect_threshold,
    ]
  } else {
    # infectious when clinical symptoms appear (score == 1)
    infectious_agents <- agents[
      agents$infect_agent &
        agents$infection_status == "infected" &
        agents$score == 1,
    ]
  }

  if (nrow(infectious_agents) > 0) {

    for (infectious_agent_id in infectious_agents$id) {

      # contacts for this infectious agent
      num_contacts <- rpois(1, lambda = contact_rate)

      if (num_contacts > 0) {
        # possible contacts
        possible_contacts <- setdiff(agents$id, infectious_agent_id)

        # randomly select contacts
        contact_ids <- sample(possible_contacts, num_contacts, replace = TRUE)

        for (contact_id in contact_ids) {
          # check if contacted agent is already infected or has recovered
          if (!agents$infect_agent[contact_id] && !agents$has_recovered[contact_id]) {
            # attempt transmission

            # check if infectious agent's virus_nasal >= infect_threshold of the contacted agent
            if (agents$virus_nasal[infectious_agent_id] >= agents$infect_threshold[contact_id]) {

              # calculate dose_eff
              virus_ratio <- agents$virus_nasal[infectious_agent_id] / agents$nasal_ccap[infectious_agent_id]

              # adjust dose_threshold to maintain dose_eff at the dose_threshold
              threshold_adj <- dose_threshold + (1 / dose_scaling_factor) * log((dose_max_efficiency / dose_efficiency_at_threshold) - 1)

              # sigmoid function
              dose_eff <- dose_max_efficiency / (1 + exp(-dose_scaling_factor * (virus_ratio - threshold_adj)))

              # ensure dose_eff is between 0 and dose_max_efficiency
              dose_eff <- min(max(dose_eff, 0), dose_max_efficiency)

              # set the dose
              agents$dose[contact_id] <- agents$virus_nasal[infectious_agent_id]

              # infect the agent
              agents <- infect_agent(
                agent_id = contact_id,
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
            }
          }
        }
      }
    }
  }

  return(agents)
}
