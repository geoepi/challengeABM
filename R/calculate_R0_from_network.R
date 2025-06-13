#' Estimate basic reproduction number from network outputs
#'
#' Uses agent level output files to compute the mean out degree of donor
#' infections, yielding an estimate of R0.
#'
#' @param data_path Directory containing `agent_*_herd_clin.csv` files.
#' @return A data frame summarising median and quantile R0 estimates.
calculate_R0_from_network <- function(data_path) {

  file_list <- list.files(
    path = data_path,
    pattern = "agent_.*_herd_clin.csv",
    full.names = TRUE
  )

  removed_files_count <- 0 # counter for files with no infections

  process_file <- function(file_name) {
    agent_data <- fread(file_name)

    if (!all(c("id", "infector_id", "is_donor") %in% colnames(agent_data))) {
      removed_files_count <<- removed_files_count + 1
      return(NA)
    }

    edges <- agent_data[!is.na(agent_data$infector_id), .(infector_id, id)]
    if (nrow(edges) == 0) {
      removed_files_count <<- removed_files_count + 1
      return(NA)
    }

    graph <- graph_from_data_frame(d = edges, directed = TRUE)

    out_degrees <- degree(graph, mode = "out")

    donor_ids <- agent_data[is_donor == TRUE, unique(id)]
    if (length(donor_ids) == 0) {
      removed_files_count <<- removed_files_count + 1
      return(NA)
    }

    R0_values <- out_degrees[names(out_degrees) %in% donor_ids]

    return(mean(R0_values, na.rm = TRUE))
  }

  R0_estimates <- lapply(file_list, process_file)
  R0_estimates <- unlist(R0_estimates)

  if (length(R0_estimates) == 0) {
    stop("Insufficient number of infections to calculate R0. Check the data.")
  }

  R0_summary <- data.frame(
    median_R0 = median(R0_estimates, na.rm = TRUE),
    lower_R0 = quantile(R0_estimates, 0.025, na.rm = TRUE),
    upper_R0 = quantile(R0_estimates, 0.975, na.rm = TRUE),
    removed_files = removed_files_count,  # files with no infections, other than hosts
    row.names = NULL
  )

  return(R0_summary)
}
