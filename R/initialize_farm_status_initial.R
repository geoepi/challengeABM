#' Initialize farm status lists for network model
#'
#' Creates per-farm agent populations and state tracking data frames used by the
#' network simulation.
#'
#' @param net Output from `generate_farm_network()`.
#' @param config List of simulation parameters.
#' @return List with `herd_agents` and `herd_states` per farm.
initialize_farm_status_initial <- function(net, config) {
  graph   <- net$graph
  farm_df <- net$farm_df
  farm_ids <- farm_df$farm_id

  onset_pars  <- fit_weibull(config$preclin_onset_median,  config$preclin_onset_ci)
  dur_pre_par <- fit_weibull(config$preclin_duration_median, config$preclin_duration_ci)
  dur_cli_par <- fit_weibull(config$clinical_duration_median, config$clinical_duration_ci)

  herd_agents <- vector("list", length(farm_ids))
  herd_states <- vector("list", length(farm_ids))
  names(herd_agents) <- farm_ids
  names(herd_states) <- farm_ids

  for (i in seq_along(farm_ids)) {
    fid <- farm_ids[i]
    herd_size <- farm_df$herd_size[i]
    num_donors <- if (!is.null(config$initial_donors) && fid %in% names(config$initial_donors)) {
      config$initial_donors[[fid]]
    } else {
      config$num_donors
    }

    agents <- initialize_herd_agents(
      num_donors         = num_donors,
      total_herd_size    = herd_size,
      nasal_threshold_mean  = config$nasal_threshold_mean,
      nasal_threshold_sd    = config$nasal_threshold_sd,
      serum_threshold_mean  = config$serum_threshold_mean,
      serum_threshold_sd    = config$serum_threshold_sd,
      infect_threshold_mean = config$infect_threshold_mean,
      infect_threshold_sd   = config$infect_threshold_sd,
      recovery_time_mean    = config$recovery_time_mean * 24,
      recovery_time_sd      = config$recovery_time_sd * 24
    )

    n <- nrow(agents)
    agents$preclin_onset     <- rweibull(n, onset_pars$shape,  onset_pars$scale)
    agents$preclin_dur       <- rweibull(n, dur_pre_par$shape, dur_pre_par$scale)
    agents$clinical_dur      <- rweibull(n, dur_cli_par$shape, dur_cli_par$scale)
    agents$transmitter_state <- NA_character_

    herd_agents[[fid]] <- agents

    herd_states[[fid]] <- data.frame(
      time             = rep(0, n),
      id               = agents$id,
      infection_status = agents$infection_status,
      is_donor         = agents$is_donor,
      score            = rep(0, n),
      infectious_t     = agents$infection_time,
      infector_id      = agents$infector_id,
      state            = rep("noninfectious", n),
      stringsAsFactors = FALSE
    )
  }

  list(herd_agents = herd_agents, herd_states = herd_states)
}
