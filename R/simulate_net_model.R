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
        if (!quarantined[fid] && any(
          herd_states[[fid]]$time == t & herd_states[[fid]]$state == "clinical"
        )) {
          if (is.na(quarantine_start[fid])) {
            quarantine_start[fid] <- t + detection_delay
          } else if (t >= quarantine_start[fid]) {
            quarantined[fid] <- TRUE
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
