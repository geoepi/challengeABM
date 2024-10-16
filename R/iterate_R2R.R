iterate_R2R <- function(simulate_room_to_room, config_file = NULL, num_iterations, write_interval = NULL, output_directory = getwd()) {
  # empty data frames
  agents_combined <- data.frame()
  final_results_combined <- data.frame()

  for (i in 1:num_iterations) { #iterate

    results <- simulate_room_to_room(config_file)

    results$agents$trial <- i
    results$final_results$trial <- i

    agents_combined <- rbind(agents_combined, results$agents)
    final_results_combined <- rbind(final_results_combined, results$final_results)

    # Write at specified write_interval
    if (!is.null(write_interval) && i %% write_interval == 0) {
      agents_filename <- file.path(output_directory, paste0("agents_results_iteration_", i, ".csv"))
      final_results_filename <- file.path(output_directory, paste0("final_results_iteration_", i, ".csv"))

      write.csv(agents_combined, agents_filename, row.names = FALSE)
      write.csv(final_results_combined, final_results_filename, row.names = FALSE)

      agents_combined <- data.frame()
      final_results_combined <- data.frame()
    }
  }

  # if write_interval is NULL
  if (is.null(write_interval)) {
    return(list(agents = agents_combined, final_results = final_results_combined))
  } else {
    return(NULL)
  }
}

