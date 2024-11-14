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
