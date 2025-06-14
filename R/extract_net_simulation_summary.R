#' Extract Summary Statistics from a Single Simulation Result
#'
#' Given a model result from `simulate_net_model()`, compute key summary metrics
#' including R_farm, outbreak size, time of first clinical case, and number infected.
#'
#' @param result A list object returned by `simulate_net_model()`.
#'
#' @return A tibble containing:
#' - R_farm
#' - outbreak_farms
#' - first_infection_time
#' - total_infected_at_first
#' - total_infected_animals
#'
#' @export
extract_net_simulation_summary <- function(result) {
  library(purrr)
  library(dplyr)

  farm_states <- result$farm_status$herd_states
  move_df     <- bind_rows(result$movement_log)

  infected_flags <- map_lgl(farm_states, ~ any(.x$infection_status %in% c("infected", "recovered")))
  srcs <- names(infected_flags)[infected_flags]

  sec_counts <- sapply(srcs, function(s) {
    dests <- unique(move_df$to[move_df$from == s])
    sum(infected_flags[dests], na.rm = TRUE)
  })
  R_farm <- if (length(sec_counts)) mean(sec_counts) else 0

  outbreak_farms <- sum(infected_flags)

  clinical_times <- map_dbl(farm_states, ~ {
    df <- .x
    times <- df$time[df$state == "clinical" & df$time > 0 & df$is_donor == FALSE]
    if (length(times)) min(times) else NA_real_
  })
  first_inf_time <- min(clinical_times, na.rm = TRUE)

  total_at_first <- sum(
    map_int(farm_states, ~ sum(
      .x$time == first_inf_time &
        .x$infection_status == "infected" &
        .x$is_donor == FALSE
    ))
  )

  total_animals <- sum(
    map_int(result$farm_status$herd_agents, ~ sum(
      .x$infection_status %in% c("infected", "recovered") &
        .x$is_donor == FALSE
    ))
  )

  tibble(
    R_farm                  = R_farm,
    outbreak_farms          = outbreak_farms,
    first_infection_time    = first_inf_time,
    total_infected_at_first = total_at_first,
    total_infected_animals  = total_animals
  )
}
