simulate_farm_movements <- function(graph, farm_status, config, time) {

  farm_ids <- farm_status$farm_ids
  movement_events <- list()

  for (from_fid in farm_ids) {
    if (farm_status$quarantined[from_fid]) next

    num_to_move <- rbinom(1, nrow(farm_status$herd_agents[[from_fid]]), config$movement_prob)
    if (num_to_move == 0) next

    v_index <- which(V(graph)$farm_id == from_fid)
    neigh   <- neighbors(graph, v_index)
    if (length(neigh) == 0) next
    neighbor_ids <- V(graph)$farm_id[neigh]

    to_fid <- sample(neighbor_ids, 1)

    mover_ids <- sample(farm_status$herd_agents[[from_fid]]$id, num_to_move)

    origin_agents <- farm_status$herd_agents[[from_fid]]
    movers         <- origin_agents[origin_agents$id %in% mover_ids, ]
    farm_status$herd_agents[[from_fid]] <- origin_agents[!origin_agents$id %in% mover_ids, ]

    farm_status$herd_agents[[to_fid]] <- rbind(
      farm_status$herd_agents[[to_fid]],
      movers
    )

    movement_events[[length(movement_events) + 1]] <- data.frame(
      time      = time,
      from      = from_fid,
      to        = to_fid,
      num_moved = num_to_move,
      stringsAsFactors = FALSE
    )
  }

  movement_events_df <- if (length(movement_events) == 0) {
    data.frame(
      time      = integer(0),
      from      = character(0),
      to        = character(0),
      num_moved = integer(0),
      stringsAsFactors = FALSE
    )
  } else {
    do.call(rbind, movement_events)
  }

  list(
    farm_status     = farm_status,
    movement_events = movement_events_df
  )
}
