initialize_abm_agents <- function(num_infected, num_non_infected_per_room, num_rooms,
                                  initial_infected_virus_nasal, initial_infected_virus_serum,
                                  nasal_threshold_mean, serum_threshold_mean, nasal_shape,
                                  serum_threshold_sd, infect_threshold_mean, infect_threshold_sd) {
  #random draws
  nasal_threshold = left_skewed_gamma(nasal_threshold_mean, nasal_shape)
  serum_threshold = rnorm(num_infected, mean = serum_threshold_mean, sd = serum_threshold_sd)
  infect_threshold = rnorm(num_infected, mean = infect_threshold_mean, sd = infect_threshold_sd)

  # initial infected agents
  infected_agents <- data.frame(
    id = 1:num_infected,
    infection_status = "infected",
    infect_agent = TRUE,
    is_donor = TRUE,
    room = 1,
    time_in_room = 0,
    virus_nasal = initial_infected_virus_nasal,
    virus_serum = initial_infected_virus_serum,
    score_t = 0,
    nasal_threshold = nasal_threshold,
    serum_threshold = serum_threshold,
    infect_threshold = infect_threshold,
    dose = NA,
    infectious_t = NA,
    growth_rate_nasal = NA,
    growth_rate_serum = NA,
    clearance_rate = NA,
    stochastic_noise = NA,
    exponential_factor = NA,
    inflection_point = NA,
    inflection_point_absolute = NA,
    growth_cease = NA,
    nasal_ccap = NA,
    serum_ccap = NA,
    infection_time = 0
  )

  num_non_infected_agents <- num_non_infected_per_room * (num_rooms - 1)
  non_inf_nasal_threshold = left_skewed_gamma(nasal_threshold_mean, nasal_shape)
  non_inf_serum_threshold = rnorm(num_non_infected_agents, mean = serum_threshold_mean, sd = serum_threshold_sd)
  non_inf_infect_threshold = rnorm(num_non_infected_agents, mean = infect_threshold_mean, sd = infect_threshold_sd)

  # non-infected agents for all rooms
  non_infected_agents <- data.frame(
    id = (num_infected + 1):(num_infected + num_non_infected_agents),
    infection_status = "non_infected",
    infect_agent = FALSE,
    is_donor = FALSE,
    room = rep(2:num_rooms, each = num_non_infected_per_room),
    time_in_room = 0,
    virus_nasal = 0,
    virus_serum = 0,
    score_t = 0,
    nasal_threshold = non_inf_nasal_threshold,
    serum_threshold = non_inf_serum_threshold,
    infect_threshold = non_inf_infect_threshold,
    dose = NA,
    infectious_t = NA,
    growth_rate_nasal = NA,
    growth_rate_serum = NA,
    clearance_rate = NA,
    stochastic_noise = NA,
    exponential_factor = NA,
    inflection_point = NA,
    inflection_point_absolute = NA,
    growth_cease = NA,
    nasal_ccap = NA,
    serum_ccap = NA,
    infection_time = NA
  )

  # join
  agents <- rbind(infected_agents, non_infected_agents)
  return(agents)
}
