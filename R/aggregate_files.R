#' Aggregate simulation files
#'
#' Read a selection of simulation output CSV files and combine them into a
#' single data frame.
#'
#' @param data_path Directory containing result files.
#' @param sim_type Either `"R2R"` or `"herd"` to choose file patterns.
#' @param sample_percentage Percentage of files to randomly sample.
#' @return A combined `data.frame` of all sampled records.
aggregate_files <- function(data_path, sim_type = "R2R", sample_percentage = 100) {

  file_list <- list.files(
    path = data_path,
    pattern = if (sim_type == "R2R") "final_results_iteration_.*\\.csv" else "results_.*_herd_clin.csv",
    full.names = TRUE
  )

  num_files <- length(file_list)
  sample_size <- ceiling((sample_percentage / 100) * num_files)

  sampled_files <- sample(file_list, sample_size)

  aggregate_df <- data.frame()

  for (file in sampled_files) {
    room_file <- read.csv(file)

      aggregate_df <- bind_rows(aggregate_df, room_file)
  }

  return(aggregate_df)
}
