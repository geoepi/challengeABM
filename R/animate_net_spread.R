animate_spread <- function(net, farm_states, move_df, output_file = "spread.gif",
                           width = 800, height = 600, fps = 5, seed = 123) {
  library(igraph)
  library(ggraph)
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(gganimate)
  library(gifski)

  set.seed(seed)

  graph    <- net$graph
  farm_ids <- V(graph)$farm_id

  layout_mat <- layout_with_fr(graph)
  layout_df  <- tibble(farm_id = farm_ids, x = layout_mat[,1], y = layout_mat[,2])

  node_states <- bind_rows(lapply(names(farm_states), function(fid) {
    farm_states[[fid]] %>%
      filter(is_donor == FALSE) %>%
      select(time, infection_status) %>%
      mutate(farm_id = fid)
  })) %>%
    group_by(time, farm_id) %>%
    summarize(outbreak = any(infection_status == "infected"), .groups = "drop") %>%
    mutate(status = ifelse(outbreak, "Outbreak", "No Outbreak")) %>%
    left_join(layout_df, by = "farm_id") %>%
    mutate(time = as.numeric(time))

  outbreak_start <- node_states %>%
    filter(outbreak) %>%
    group_by(farm_id) %>%
    summarize(t0 = min(time), .groups = "drop")

  inst_events <- move_df %>%
    mutate(time = as.numeric(time)) %>%
    left_join(layout_df, by = c("from" = "farm_id")) %>%
    rename(x_from = x, y_from = y) %>%
    left_join(layout_df, by = c("to" = "farm_id")) %>%
    rename(x_to = x, y_to = y) %>%
    left_join(outbreak_start, by = c("to" = "farm_id")) %>%
    filter(!is.na(t0) & time == t0)

  times <- sort(unique(node_states$time))
  skeleton_edges <- as_data_frame(graph, what = "edges") %>%
    transmute(from = V(graph)$farm_id[from], to = V(graph)$farm_id[to]) %>%
    left_join(layout_df, by = c("from" = "farm_id")) %>%
    rename(x_from = x, y_from = y) %>%
    left_join(layout_df, by = c("to" = "farm_id")) %>%
    rename(x_to = x, y_to = y) %>%
    crossing(time = times)

  p <- ggplot() +
    geom_segment(
      data = skeleton_edges,
      aes(x = x_from, y = y_from, xend = x_to, yend = y_to,
          group = interaction(from, to)),
      color = "grey80", alpha = 0.5, linewidth = 0.5
    )

  if (nrow(inst_events) > 0) {
    p <- p +
      geom_segment(
        data = inst_events,
        aes(x = x_from, y = y_from, xend = x_to, yend = y_to,
            group = interaction(from, to)),
        color = "red", size = 1,
        arrow = arrow(length = unit(4, "pt")),
        inherit.aes = FALSE
      )
  }

  p <- p +
    geom_point(
      data = node_states,
      aes(x = x, y = y, color = status), size = 5
    ) +
    geom_text(
      data = node_states,
      aes(x = x, y = y, label = farm_id),
      vjust = -1, size = 3, show.legend = FALSE
    ) +
    scale_color_manual(values = c(
      "No Outbreak" = "lightgreen",
      "Outbreak"    = "darkred"
    )) +
    theme_void() +
    theme(plot.margin = unit(c(2,0.5,2,0.5),"cm"),
          legend.direction = "vertical",
          legend.position="right",
          strip.text = element_blank(),
          strip.background = element_blank(),
          legend.key.size = unit(2,"line"),
          legend.key.width = unit(3,"line"),
          legend.text = element_text(size=16, face="bold"),
          legend.title = element_text(size=18, face="bold"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          plot.title = element_text(size=24, face="bold")) +
    labs(title = "Time: {as.integer(frame_time)} h",
         color = "Status") +
    transition_time(time) +
    ease_aes("linear")

  animate(
    p,
    width    = width,
    height   = height,
    fps      = fps,
    renderer = gifski_renderer(output_file)
  )
}

