plot_transmission_network <- function(file_path, layout_type = "graphopt", seed = NULL) {

  agent_data <- read.csv(file_path)

  if (!all(c("id", "infector_id", "is_donor") %in% colnames(agent_data))) {
    stop("The CSV file must contain 'id', 'infector_id', and 'is_donor' columns.")
  }

  if (!is.null(seed)) {
    set.seed(seed)
  }

  edges <- agent_data[!is.na(agent_data$infector_id), c("infector_id", "id")]

  graph <- graph_from_data_frame(d = edges, directed = TRUE)

  V(graph)$color <- ifelse(agent_data$is_donor[match(V(graph)$name, agent_data$id)], "red", "lightblue")

  layout <- switch(
    layout_type,
    "graphopt" = layout_with_graphopt(graph),
    "tree" = layout_as_tree(graph),
    stop("Invalid layout type. Choose 'graphopt' or 'tree'.")
  )

  plot(graph,
       vertex.label = V(graph)$name,
       vertex.size = 10,
       vertex.color = V(graph)$color,
       vertex.label.cex = 0.90,
       edge.arrow.size = 0.25,
       layout = layout,
       main = "Transmission Network")
}
