update_agent_virus_dynamics <- function(agent, current_time, delta_t) {

  infection_time <- agent$infection_time
  is_donor <- agent$is_donor
  time_since_infection <- current_time - infection_time

  # donors
  if (is_donor && time_since_infection < 2 * delta_t) {
    return(agent)
  }

  # non-donors: iteration after infection
  if (!is_donor && time_since_infection < delta_t) {
    return(agent)
  }

  virus_nasal <- agent$virus_nasal
  virus_serum <- agent$virus_serum
  inflection_point_absolute_nasal <- agent$inflection_point_absolute_nasal
  inflection_point_absolute_serum <- agent$inflection_point_absolute_serum
  growth_cease <- agent$growth_cease
  growth_rate_nasal <- agent$growth_rate_nasal
  growth_rate_serum <- agent$growth_rate_serum
  exponential_factor <- agent$exponential_factor
  clearance_rate <- agent$clearance_rate
  stochastic_noise <- agent$stochastic_noise
  nasal_ccap <- agent$nasal_ccap
  serum_ccap <- agent$serum_ccap

  # growth rate for nasal virus
  if (current_time > growth_cease) {
    effective_growth_rate_nasal <- 0
  } else {
    if (current_time <= inflection_point_absolute_nasal) {
      effective_growth_rate_nasal <- growth_rate_nasal
    } else {
      effective_growth_rate_nasal <- growth_rate_nasal *
        exp(-(current_time - inflection_point_absolute_nasal) / (24 * exponential_factor))
    }
  }

  # growth rate for serum virus
  if (current_time > growth_cease) {
    effective_growth_rate_serum <- 0
  } else {
    if (current_time <= inflection_point_absolute_serum) {
      effective_growth_rate_serum <- growth_rate_serum
    } else {
      effective_growth_rate_serum <- growth_rate_serum *
        exp(-(current_time - inflection_point_absolute_serum) / (24 * exponential_factor))
    }
  }

  # clearance
  dynamic_clearance_nasal <- clearance_rate
  dynamic_clearance_serum <- clearance_rate

  # stochastic noise
  stochastic_value_nasal <- rnorm(1, mean = 0, sd = stochastic_noise)
  stochastic_value_serum <- rnorm(1, mean = 0, sd = stochastic_noise)

  # virus_nasal
  virus_nasal <- max(0,
                     virus_nasal +
                       effective_growth_rate_nasal * virus_nasal * (1 - virus_nasal / nasal_ccap) * delta_t -
                       dynamic_clearance_nasal * virus_nasal * delta_t +
                       stochastic_value_nasal)

  # virus_serum
  virus_serum <- max(0,
                     virus_serum +
                       effective_growth_rate_serum * virus_serum * (1 - virus_serum / serum_ccap) * delta_t -
                       dynamic_clearance_serum * virus_serum * delta_t +
                       stochastic_value_serum)

  # update
  agent$virus_nasal <- virus_nasal
  agent$virus_serum <- virus_serum

  return(agent)
}
