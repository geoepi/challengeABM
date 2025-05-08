aesigmoid <- function(t, onset, duration) {
  steep <- 4 / (duration * 24)
  1 / (1 + exp(-steep * (t - onset * 24)))
}

fit_weibull <- function(median, ci) {
  obj_fn <- function(par) {
    shape <- par[1]; scale <- par[2]
    q25  <- qweibull(0.025, shape, scale)
    q50  <- qweibull(0.5,   shape, scale)
    q975 <- qweibull(0.975, shape, scale)
    (q50 - median)^2 + (q25 - ci[1])^2 + (q975 - ci[2])^2
  }
  opt <- optim(c(2, median), function(p) sum(obj_fn(p)), method = "L-BFGS-B", lower = c(0.1, 0.1))
  list(shape = opt$par[1], scale = opt$par[2])
}

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

# master function
simulate_net_model <- function(net, config) {
  graph   <- net$graph
  farm_df <- net$farm_df
  farm_ids <- farm_df$farm_id

  detection_enabled <- isTRUE(config$detection_triggers_quarantine)
  detection_delay   <- if (!is.null(config$detection_delay_hours)) config$detection_delay_hours else 0

  fs <- initialize_farm_status_initial(net, config)
  herd_agents <- fs$herd_agents
  herd_states <- fs$herd_states

  quarantined      <- setNames(rep(FALSE, length(farm_ids)), farm_ids)
  quarantine_start <- setNames(rep(NA_integer_, length(farm_ids)), farm_ids)

  movement_log   <- vector("list", config$num_hours)
  quarantine_log <- vector("list", config$num_hours)

  for (t in seq_len(config$num_hours - 1)) {

    for (fid in farm_ids) {
      step <- update_within_herd_step(
        herd_agents[[fid]], herd_states[[fid]], config, t
      )
      herd_agents[[fid]] <- step$agents
      herd_states[[fid]]  <- step$state_df
    }

    if (detection_enabled) {
      for (fid in farm_ids) {
        if (!quarantined[fid]) {
          if (any(herd_states[[fid]]$time == t & herd_states[[fid]]$state == "clinical")) {
            if (is.na(quarantine_start[fid])) {
              quarantine_start[fid] <- t + detection_delay
            } else if (t >= quarantine_start[fid]) {
              quarantined[fid] <- TRUE
            }
          }
        }
      }
    }
    quarantine_log[[t]] <- quarantined

    farm_status_obj <- list(
      herd_agents = herd_agents,
      herd_states = herd_states,
      quarantined = quarantined,
      farm_ids    = farm_ids
    )

    if (t > 0 && (t %% config$movement_interval) == 0) {
      mv <- simulate_farm_movements(graph, farm_status_obj, config, t)
      herd_agents <- mv$farm_status$herd_agents
      herd_states <- mv$farm_status$herd_states
      quarantined <- mv$farm_status$quarantined
      movement_log[[t]] <- mv$movement_events
    } else {
      movement_log[[t]] <- data.frame(
        time      = integer(0),
        from      = character(0),
        to        = character(0),
        num_moved = integer(0),
        stringsAsFactors = FALSE
      )
    }
  }

  list(
    farm_status    = list(
      herd_agents = herd_agents,
      herd_states = herd_states,
      quarantined = quarantined
    ),
    movement_log   = movement_log,
    quarantine_log = quarantine_log
  )
}
