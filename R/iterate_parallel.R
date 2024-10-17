## Too expensive to export the environment to all workers...  this needs work.

iterate_parallel <- function(simulation_function, config_file = NULL, num_iterations,
                                 output_directory = getwd(), num_cores = parallel::detectCores()) {

  require(foreach)
  require(doParallel)

  message("Setting up parallel backend...")
  # backend with specified number of cores
  cl <- makeCluster(num_cores, outfile = file.path(output_directory, "cluster_debug_log.txt"))
  registerDoParallel(cl)

  # export to each cluster worker
  clusterExport(cl, varlist = c("simulation_function", "config_file"), envir = environment())
  clusterEvalQ(cl, {
    library(dplyr)
  })

  # iterate
  foreach(i = 1:num_iterations, .packages = c("dplyr"), .errorhandling = 'pass') %dopar% {
    tryCatch({
      cat(sprintf("Running iteration %d
", i), file = file.path(output_directory, "iteration_debug_log.txt"), append = TRUE)

      # Run the simulation function
      results <- simulation_function(config_file)

      results$agents$trial <- i
      results$final_results$trial <- i

      agents_filename <- file.path(output_directory, paste0("agents_iter_", i, ".csv"))
      final_results_filename <- file.path(output_directory, paste0("final_results_iter_", i, ".csv"))

      write.csv(results$agents, agents_filename, row.names = FALSE)
      write.csv(results$final_results, final_results_filename, row.names = FALSE)

      cat(sprintf("Successfully wrote iteration %d to files.
", i), file = file.path(output_directory, "iteration_debug_log.txt"), append = TRUE)

    }, error = function(e) {
      cat(sprintf("Error in iteration %d: %s
", i, e$message), file = file.path(output_directory, "iteration_debug_log.txt"), append = TRUE)
      return(NULL)
    })
  }

  # stop the cluster
  stopCluster(cl)
  message("Parallel backend stopped.")
}
