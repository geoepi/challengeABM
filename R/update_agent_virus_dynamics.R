update_agent_virus_dynamics <- function(agent, current_time, delta_t) {
  # determine the agent's inflection point
  animal_inflection_point <- agent$inflection_point_absolute

  # dynamic growth rate: completely cease growth at t
  growth_cease <- agent$growth_cease

  if (current_time > growth_cease) {
    effective_growth_rate_nasal <- 0
    effective_growth_rate_serum <- 0
  } else {
    # reduced growth after the inflection
    if (current_time <= animal_inflection_point) {
      effective_growth_rate_nasal <- agent$growth_rate_nasal  # Growth phas
      effective_growth_rate_serum <- agent$growth_rate_serum
    } else {
      effective_growth_rate_nasal <- agent$growth_rate_nasal * exp(-(current_time - animal_inflection_point) / (24 * agent$exponential_factor))
      effective_growth_rate_serum <- agent$growth_rate_serum * exp(-(current_time - animal_inflection_point) / (24 * agent$exponential_factor))
    }
  }

  # clearance
  dynamic_clearance_nasal <- agent$clearance_rate
  dynamic_clearance_serum <- agent$clearance_rate

  # stochastic noise
  stochastic_value_nasal <- rnorm(1, mean = 0, sd = agent$stochastic_noise)
  stochastic_value_serum <- rnorm(1, mean = 0, sd = agent$stochastic_noise)

  # update virus_nasal
  virus_nasal <- max(0,
                     agent$virus_nasal +
                       effective_growth_rate_nasal * agent$virus_nasal * (1 - agent$virus_nasal / agent$nasal_ccap) * delta_t -
                       dynamic_clearance_nasal * agent$virus_nasal * delta_t +
                       stochastic_value_nasal)

  # update virus_serum
  virus_serum <- max(0,
                     agent$virus_serum +
                       effective_growth_rate_serum * agent$virus_serum * (1 - agent$virus_serum / agent$serum_ccap) * delta_t -
                       dynamic_clearance_serum * agent$virus_serum * delta_t +
                       stochastic_value_serum)

  agent$virus_nasal <- virus_nasal
  agent$virus_serum <- virus_serum

  return(agent)
}
