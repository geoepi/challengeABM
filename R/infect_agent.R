#' Infect an individual agent
#'
#' Sets initial infection parameters and virus dynamics for a given agent. When
#' not an initial infection the dose efficiency override is applied.
#'
#' @param agent_id Index of the agent to infect.
#' @param agents Data frame of all agents.
#' @param current_time Current simulation time.
#' @param initial_infection Logical indicating donor introduction.
#' @param delta_t Simulation time step.
#' @param dose_eff_override Override value for dose efficiency when infected by contact.
#' @param growth_rate_nasal_mean,growth_rate_nasal_sd Mean and SD of nasal growth rate.
#' @param growth_rate_serum_mean,growth_rate_serum_sd Mean and SD of serum growth rate.
#' @param clearance_rate_mean,clearance_rate_sd Parameters for clearance rate.
#' @param stochastic_noise_mean,stochastic_noise_sd Parameters controlling stochastic noise.
#' @param exponential_factor_mean,exponential_factor_sd Parameters for waning growth exponent.
#' @param inflection_point_mean,inflection_point_sd Parameters for growth inflection point.
#' @param growth_cease_mean,growth_cease_sd Mean and SD for growth cessation time.
#' @param nasal_ccap_mean,nasal_ccap_sd Mean and SD of nasal carrying capacity.
#' @param serum_ccap_mean,serum_ccap_sd Mean and SD of serum carrying capacity.
#' @param infector_id ID of the infecting agent if applicable.
#' @return Updated agent data frame.
infect_agent <- function(agent_id, agents, current_time,
                         initial_infection = FALSE,
                         delta_t, dose_eff_override = NULL,
                         growth_rate_nasal_mean, growth_rate_nasal_sd,
                         growth_rate_serum_mean, growth_rate_serum_sd,
                         clearance_rate_mean, clearance_rate_sd,
                         stochastic_noise_mean, stochastic_noise_sd,
                         exponential_factor_mean, exponential_factor_sd,
                         inflection_point_mean, inflection_point_sd,
                         growth_cease_mean, growth_cease_sd,
                         nasal_ccap_mean, nasal_ccap_sd,
                         serum_ccap_mean, serum_ccap_sd,
                         infector_id = NULL) {

  # if already infected and this is not the initial infection, return
  if (agents$infect_agent[agent_id] && !initial_infection) {
    return(agents)
  }

  # infect agent
  agents$infect_agent[agent_id] <- TRUE
  agents$infection_status[agent_id] <- "infected"

  # record infector id
  if (!is.null(infector_id) && is.na(agents$infector_id[agent_id])) {
    agents$infector_id[agent_id] <- infector_id
  }

  if (initial_infection) {

    agents$virus_nasal[agent_id] <- 0
    agents$virus_serum[agent_id] <- 0

    agents$growth_rate_nasal[agent_id] <- rnorm(1, mean = growth_rate_nasal_mean, sd = growth_rate_nasal_sd) / 24
    agents$growth_rate_serum[agent_id] <- rnorm(1, mean = growth_rate_serum_mean, sd = growth_rate_serum_sd) / 24

    agents$infection_time[agent_id] <- -delta_t
  } else {

    # check dose_eff_override
    if (is.null(dose_eff_override)) {
      stop("Error: dose_eff_override not found.")
    }

    # initial virus quantity
    agents$virus_nasal[agent_id] <- dose_eff_override * agents$dose[agent_id]
    agents$virus_serum[agent_id] <- agents$virus_nasal[agent_id]

    # growth rates for contact animals
    agents$growth_rate_nasal[agent_id] <- rnorm(1, mean = growth_rate_nasal_mean, sd = growth_rate_nasal_sd) / 24
    agents$growth_rate_serum[agent_id] <- rnorm(1, mean = growth_rate_serum_mean, sd = growth_rate_serum_sd) / 24

    # infection time
    agents$infection_time[agent_id] <- current_time
  }

  # parameters for virus growth
  agents$clearance_rate[agent_id] <- rlnorm(1, meanlog = log(clearance_rate_mean), sdlog = clearance_rate_sd) / 24
  agents$stochastic_noise[agent_id] <- rlnorm(1, meanlog = log(stochastic_noise_mean), sdlog = stochastic_noise_sd)
  agents$exponential_factor[agent_id] <- rlnorm(1, meanlog = log(exponential_factor_mean), sdlog = exponential_factor_sd)

  # inflection point for nasal virus, in hours
  sampled_inflection_point_nasal <- max(0, rnorm(1, mean = inflection_point_mean, sd = inflection_point_sd)) * 24
  agents$inflection_point_nasal[agent_id] <- sampled_inflection_point_nasal
  agents$inflection_point_absolute_nasal[agent_id] <- agents$infection_time[agent_id] + sampled_inflection_point_nasal

  # inflection point for serum virus, 2 days (48 hours) earlier than nasal
  sampled_inflection_point_serum <- max(0, sampled_inflection_point_nasal - 48)
  agents$inflection_point_serum[agent_id] <- sampled_inflection_point_serum
  agents$inflection_point_absolute_serum[agent_id] <- agents$infection_time[agent_id] + sampled_inflection_point_serum

  # growth cease time after infection time
  sampled_growth_cease <- rnorm(1, mean = growth_cease_mean, sd = growth_cease_sd)
  agents$growth_cease[agent_id] <- agents$infection_time[agent_id] + sampled_growth_cease

  # carrying capacities
  agents$nasal_ccap[agent_id] <- rnorm(1, mean = nasal_ccap_mean, sd = nasal_ccap_sd)
  agents$serum_ccap[agent_id] <- rnorm(1, mean = serum_ccap_mean, sd = serum_ccap_sd)

  return(agents)
}
