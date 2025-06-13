#' Animate disease spread on a farm network
#'
#' Produce a GIF showing infection status of farms over time using movement
#' events from the simulation.
#'
#' @param net Network object produced by `generate_farm_network`.
#' @param farm_states List with per-farm state tables.
#' @param move_df Data frame of animal movements between farms.
#' @param output_file Filename for the resulting GIF.
#' @param width,height Size of the animation in pixels.
#' @param fps Frames per second.
#' @param seed Random seed used for layout generation.
#' @return The gganimate object invisibly.
animate_spread <- function(net, farm_states, move_df, output_file = "spread.gif",
                           width = 800, height = 600, fps = 5, seed = 123) {
  # use functions from imported packages via explicit namespaces

  set.seed(seed)

  graph    <- net$graph
  farm_ids <- igraph::V(graph)$farm_id

  layout_mat <- igraph::layout_with_fr(graph)
  layout_df  <- tibble::tibble(farm_id = farm_ids, x = layout_mat[,1], y = layout_mat[,2])

  node_states <- dplyr::bind_rows(lapply(names(farm_states), function(fid) {
    farm_states[[fid]] %>%
      dplyr::filter(is_donor == FALSE) %>%
      dplyr::select(time, infection_status) %>%
      dplyr::mutate(farm_id = fid)
  })) %>%
    dplyr::group_by(time, farm_id) %>%
    dplyr::summarize(outbreak = any(infection_status == "infected"), .groups = "drop") %>%
    dplyr::mutate(status = ifelse(outbreak, "Outbreak", "No Outbreak")) %>%
    dplyr::left_join(layout_df, by = "farm_id") %>%
    dplyr::mutate(time = as.numeric(time))

  outbreak_start <- node_states %>%
    dplyr::filter(outbreak) %>%
    dplyr::group_by(farm_id) %>%
    dplyr::summarize(t0 = min(time), .groups = "drop")

  inst_events <- move_df %>%
    dplyr::mutate(time = as.numeric(time)) %>%
    dplyr::left_join(layout_df, by = c("from" = "farm_id")) %>%
    dplyr::rename(x_from = x, y_from = y) %>%
    dplyr::left_join(layout_df, by = c("to" = "farm_id")) %>%
    dplyr::rename(x_to = x, y_to = y) %>%
    dplyr::left_join(outbreak_start, by = c("to" = "farm_id")) %>%
    dplyr::filter(!is.na(t0) & time == t0)

  times <- sort(unique(node_states$time))
  skeleton_edges <- igraph::as_data_frame(graph, what = "edges") %>%
    dplyr::transmute(from = igraph::V(graph)$farm_id[from], to = igraph::V(graph)$farm_id[to]) %>%
    dplyr::left_join(layout_df, by = c("from" = "farm_id")) %>%
    dplyr::rename(x_from = x, y_from = y) %>%
    dplyr::left_join(layout_df, by = c("to" = "farm_id")) %>%
    dplyr::rename(x_to = x, y_to = y) %>%
    tidyr::crossing(time = times)

  p <- ggplot2::ggplot() +
    ggplot2::geom_segment(
      data = skeleton_edges,
      ggplot2::aes(x = x_from, y = y_from, xend = x_to, yend = y_to,
          group = interaction(from, to)),
      color = "grey80", alpha = 0.5, linewidth = 0.5
    )

  if (nrow(inst_events) > 0) {
      p <- p +
        ggplot2::geom_segment(
          data = inst_events,
          ggplot2::aes(x = x_from, y = y_from, xend = x_to, yend = y_to,
              group = interaction(from, to)),
          color = "red", size = 1,
          arrow = grid::arrow(length = grid::unit(4, "pt")),
          inherit.aes = FALSE
        )
  }

  p <- p +
    ggplot2::geom_point(
      data = node_states,
      ggplot2::aes(x = x, y = y, color = status), size = 5
    ) +
    ggplot2::geom_text(
      data = node_states,
      ggplot2::aes(x = x, y = y, label = farm_id),
      vjust = -1, size = 3, show.legend = FALSE
    ) +
    ggplot2::scale_color_manual(values = c(
      "No Outbreak" = "lightgreen",
      "Outbreak"    = "darkred"
    )) +
    ggplot2::theme_void() +
    ggplot2::theme(plot.margin = grid::unit(c(2,0.5,2,0.5),"cm"),
          legend.direction = "vertical",
          legend.position="right",
          strip.text = element_blank(),
          strip.background = element_blank(),
          legend.key.size = grid::unit(2,"line"),
          legend.key.width = grid::unit(3,"line"),
          legend.text = ggplot2::element_text(size=16, face="bold"),
          legend.title = ggplot2::element_text(size=18, face="bold"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          plot.title = ggplot2::element_text(size=24, face="bold")) +
    ggplot2::labs(title = "Time: {as.integer(frame_time)} h",
         color = "Status") +
    gganimate::transition_time(time) +
    gganimate::ease_aes("linear")

  gganimate::animate(
    p,
    width    = width,
    height   = height,
    fps      = fps,
    renderer = gganimate::gifski_renderer(output_file)
  )
}

