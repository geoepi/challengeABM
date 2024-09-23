# infect an agent
infect_agent <- function(agent_id, agents, current_time,
                         initial_infection = FALSE, k_dose,
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

  # if already infected and not a donor, return early
  if (agents$infect_agent[agent_id] && !initial_infection) {
    return(agents)
  }

  # infect the agent
  agents$infect_agent[agent_id] <- TRUE
  agents$infection_status[agent_id] <- "infected"

  # infection_time
  agents$infection_time[agent_id] <- current_time

  # initial virus concentrations
  if (initial_infection) {
    # donors: initial virus levels based on some default values
    agents$virus_nasal[agent_id] <- rnorm(1, mean = initial_virus_nasal, sd = 0.1)
    agents$virus_serum[agent_id] <- rnorm(1, mean = initial_virus_serum, sd = 0.1)

    # assign individual parameters for virus growth
    agents$growth_rate_nasal[agent_id] <- rnorm(1, mean = growth_rate_nasal_mean, sd = growth_rate_nasal_sd) / 24
    agents$growth_rate_serum[agent_id] <- rnorm(1, mean = growth_rate_serum_mean, sd = growth_rate_serum_sd) / 24
  } else if (!is.na(agents$dose[agent_id])) {
    # non-donors: sample virus based on dose received
    dose_eff <- rbeta(1, shape1 = dose_shape1, shape2 = dose_shape2)
    agents$virus_nasal[agent_id] <- dose_eff * agents$dose[agent_id]
    agents$virus_serum[agent_id] <- agents$virus_nasal[agent_id] #* 0.1  # for serum?

    # adjust growth rates based on dose_eff
    base_growth_rate_nasal <- rnorm(1, mean = growth_rate_nasal_mean, sd = growth_rate_nasal_sd) / 24
    base_growth_rate_serum <- rnorm(1, mean = growth_rate_serum_mean, sd = growth_rate_serum_sd) / 24

    # scaling factor 'k' to control influence
    k_dose <- k_dose

    # adjust growth rates
    agents$growth_rate_nasal[agent_id] <- base_growth_rate_nasal * (1 + k_dose * dose_eff)
    agents$growth_rate_serum[agent_id] <- base_growth_rate_serum * (1 + k_dose * dose_eff)
  } else {
    # default for non-infected agents receiving no dose
    agents$virus_nasal[agent_id] <- 0
    agents$virus_serum[agent_id] <- 0

    # base growth rates without dose adjustment
    agents$growth_rate_nasal[agent_id] <- rnorm(1, mean = growth_rate_nasal_mean, sd = growth_rate_nasal_sd) / 24
    agents$growth_rate_serum[agent_id] <- rnorm(1, mean = growth_rate_serum_mean, sd = growth_rate_serum_sd) / 24
  }

  # individual virus growth
  agents$clearance_rate[agent_id] <- rnorm(1, mean = clearance_rate_mean, sd = clearance_rate_sd) / 24
  agents$stochastic_noise[agent_id] <- rnorm(1, mean = stochastic_noise_mean, sd = stochastic_noise_sd)
  agents$exponential_factor[agent_id] <- rnorm(1, mean = exponential_factor_mean, sd = exponential_factor_sd)

  # sample inflection point relative to infection time, in hours
  sampled_inflection_point_hours <- max(0, rnorm(1, mean = inflection_point_mean, sd = inflection_point_sd)) * 24
  agents$inflection_point[agent_id] <- sampled_inflection_point_hours  # Relative inflection point in hours
  agents$inflection_point_absolute[agent_id] <- current_time + sampled_inflection_point_hours  # Absolute time

  sampled_growth_cease <- rnorm(1, mean = growth_cease_mean, sd = growth_cease_sd)
  agents$growth_cease[agent_id] <- current_time + sampled_growth_cease

  # virus caps (carrying capacity)
  agents$nasal_ccap[agent_id] <- rnorm(1, mean = nasal_ccap_mean, sd = nasal_ccap_sd)
  agents$serum_ccap[agent_id] <- rnorm(1, mean = serum_ccap_mean, sd = serum_ccap_sd)

  return(agents)
}
