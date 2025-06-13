#' Simulate one within herd time step
#'
#' Updates infection states for all animals in a herd and records the results in
#' the state data frame.
#'
#' @param agents Data frame of herd agents.
#' @param state_df State history data frame.
#' @param config List of parameters for transmission and recovery.
#' @param time Current simulation time.
#' @return List containing updated `agents` and `state_df`.
update_within_herd_step <- function(agents, state_df, config, time) {

  if (nrow(agents) == 0) {
    return(list(agents = agents, state_df = state_df))
  }
  agents$time_since_infection <- ifelse(is.na(agents$infection_time), NA, time - agents$infection_time)
  agents$state <- "noninfectious"

  # update infected states
  idx_time <- which(!is.na(agents$time_since_infection))
  for (i in idx_time) {
    ts    <- agents$time_since_infection[i]
    F_pre <- aesigmoid(ts, agents$preclin_onset[i], agents$preclin_dur[i])
    F_cli <- aesigmoid(ts, agents$preclin_onset[i] + agents$preclin_dur[i], agents$clinical_dur[i])
    P_pre <- max(0, F_pre - F_cli)
    P_cli <- F_cli
    r     <- runif(1)
    agents$state[i] <- if (r < P_cli) "clinical"
    else if (r < P_cli + P_pre) "preclinical"
    else "noninfectious"
  }

  new_inf <- which(agents$infection_status == "infected" & is.na(agents$recovery_time))
  if (length(new_inf) > 0) {
    agents$recovery_time[new_inf] <- round(
      rnorm(length(new_inf), config$recovery_time_mean * 24, config$recovery_time_sd * 24)
    )
  }

  recov_idx <- which(
    agents$infection_status == "infected" &
      !is.na(agents$infection_time) &
      (time - agents$infection_time) >= agents$recovery_time
  )
  if (length(recov_idx) > 0) {
    agents$infection_status[recov_idx] <- "recovered"
    agents$has_recovered[recov_idx]    <- TRUE
  }

  inf_idx <- if (config$preclin_infect) {
    which(agents$state %in% c("preclinical", "clinical"))
  } else {
    which(agents$state == "clinical")
  }
  if (length(inf_idx) > 0) {
    for (i in inf_idx) {
      contacts <- sample(
        which(agents$infection_status == "non_infected"),
        min(rpois(1, config$contact_rate), sum(agents$infection_status == "non_infected"))
      )
      for (j in contacts) {
        if (runif(1) < config$transmission_prob) {
          agents$infection_status[j]  <- "infected"
          agents$infect_agent[j]      <- TRUE
          agents$infection_time[j]    <- time
          agents$infector_id[j]       <- agents$id[i]
          agents$transmitter_state[j] <- agents$state[i]
        }
      }
    }
  }

  new_row <- data.frame(
    time             = time,
    id               = agents$id,
    infection_status = agents$infection_status,
    is_donor         = agents$is_donor,
    score            = as.integer(agents$state == "clinical"),
    infectious_t     = agents$infection_time,
    infector_id      = agents$infector_id,
    state            = agents$state,
    stringsAsFactors = FALSE
  )
  state_df <- dplyr::bind_rows(state_df, new_row)

  list(agents = agents, state_df = state_df)
}
