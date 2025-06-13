#' Generate a random farm contact network
#'
#' Creates an igraph object with weighted edges representing contact likelihood
#' between farms and a companion data frame of node attributes.
#'
#' @param config List of network settings including type, size and random seed.
#' @return A list with `graph` and `farm_df` elements.
generate_farm_network <- function(config) {

  set.seed(config$seed)

  calculate_inverse_distance_weight <- function(distance_m, lambda) {
    distance_m <- pmax(distance_m, 0)
    round(exp(-lambda * distance_m), 4)
  }

  n_farms <- config$n_farms
  graph_type <- config$graph_type

  if (graph_type == "erdos") {
    g <- igraph::sample_gnp(n = n_farms, p = 0.2, directed = FALSE)
  } else if (graph_type == "smallworld") {
    g <- igraph::sample_smallworld(dim = 1, size = n_farms, nei = 2, p = 0.1)
  } else if (graph_type == "geometric") {
    coords <- matrix(runif(n_farms * 2), ncol = 2)
    g <- igraph::make_full_graph(n_farms)
    dists <- as.matrix(dist(coords))

    for (e in igraph::E(g)) {
      ends <- igraph::ends(g, e)
      raw_dist <- dists[ends[1], ends[2]]
      multiplier <- rgamma(1, shape = config$shape_gamma, scale = config$scale_gamma)
      edge_dist <- max(0.1, multiplier * raw_dist)
      g <- igraph::set_edge_attr(g, "distance", index = e, value = edge_dist)
    }
  } else {
    stop("Unsupported graph_type.")
  }

  if (!"distance" %in% igraph::edge_attr_names(g)) {
    for (e in igraph::E(g)) {
      edge_dist <- rgamma(1, shape = config$shape_gamma, scale = config$scale_gamma)
      g <- igraph::set_edge_attr(g, "distance", index = e, value = edge_dist)
    }
  }

  dists <- igraph::edge_attr(g, "distance")
  weights <- sapply(dists, calculate_inverse_distance_weight, lambda = config$lambda)
  g <- igraph::set_edge_attr(g, "weight", value = weights)

  herd_sizes <- sample(config$herd_size_range[1]:config$herd_size_range[2], n_farms, replace = TRUE)
  igraph::V(g)$farm_id <- paste0("farm_", seq_len(n_farms))
  igraph::V(g)$herd_size <- herd_sizes

  farm_df <- data.frame(
    farm_id = igraph::V(g)$farm_id,
    node_id = seq_len(n_farms),
    herd_size = herd_sizes
  )

  return(list(graph = g, farm_df = farm_df))
}
