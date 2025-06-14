#' Generate a Farm Network Graph Based on Configurable Parameters
#'
#' Builds an igraph network of farms using either Erdos-Renyi, small-world, or
#' geometric construction, with edge distances and weights, herd sizes, and metadata.
#' Parameters from the `config` list can be overridden by direct function arguments.
#'
#' @param config A named list of default parameters.
#' @param n_farms Optional. Number of farms to simulate.
#' @param graph_type Optional. One of `"erdos"`, `"smallworld"`, or `"geometric"`.
#' @param seed Optional. Integer seed for reproducibility.
#' @param shape_gamma Optional. Shape parameter for gamma-distributed edge distances.
#' @param scale_gamma Optional. Scale parameter for gamma-distributed edge distances.
#' @param lambda Optional. Decay rate for edge weight calculation.
#' @param herd_size_range Optional. Integer vector of length 2 specifying min and max herd sizes.
#'
#' @return A list with components: `graph` (igraph object) and `farm_df` (farm metadata).
generate_farm_network <- function(config,
                                  n_farms = NULL,
                                  graph_type = NULL,
                                  seed = NULL,
                                  shape_gamma = NULL,
                                  scale_gamma = NULL,
                                  lambda = NULL,
                                  herd_size_range = NULL) {

  n_farms         <- if (is.null(n_farms)) config$n_farms else n_farms
  graph_type      <- if (is.null(graph_type)) config$graph_type else graph_type
  seed            <- if (is.null(seed)) config$seed else seed
  shape_gamma     <- if (is.null(shape_gamma)) config$shape_gamma else shape_gamma
  scale_gamma     <- if (is.null(scale_gamma)) config$scale_gamma else scale_gamma
  lambda          <- if (is.null(lambda)) config$lambda else lambda
  herd_size_range <- if (is.null(herd_size_range)) config$herd_size_range else herd_size_range

  set.seed(seed)

  calculate_inverse_distance_weight <- function(distance_m, lambda) {
    distance_m <- pmax(distance_m, 0)
    round(exp(-lambda * distance_m), 4)
  }

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
      multiplier <- rgamma(1, shape = shape_gamma, scale = scale_gamma)
      edge_dist <- max(0.1, multiplier * raw_dist)
      g <- igraph::set_edge_attr(g, "distance", index = e, value = edge_dist)
    }
  } else {
    stop("Unsupported graph_type.")
  }

  if (!"distance" %in% igraph::edge_attr_names(g)) {
    for (e in igraph::E(g)) {
      edge_dist <- rgamma(1, shape = shape_gamma, scale = scale_gamma)
      g <- igraph::set_edge_attr(g, "distance", index = e, value = edge_dist)
    }
  }

  dists <- igraph::edge_attr(g, "distance")
  weights <- sapply(dists, calculate_inverse_distance_weight, lambda = lambda)
  g <- igraph::set_edge_attr(g, "weight", value = weights)

  herd_sizes <- sample(herd_size_range[1]:herd_size_range[2], n_farms, replace = TRUE)
  igraph::V(g)$farm_id <- paste0("farm_", seq_len(n_farms))
  igraph::V(g)$herd_size <- herd_sizes

  farm_df <- data.frame(
    farm_id = igraph::V(g)$farm_id,
    node_id = seq_len(n_farms),
    herd_size = herd_sizes
  )

  return(list(graph = g, farm_df = farm_df))
}
