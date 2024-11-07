calculate_generation_times <- function(data_path) {

  file_list <- list.files(
    path = data_path,
    pattern = "agent_.*_herd_clin.csv",
    full.names = TRUE
  )

  generation_times_list <- list()

  for (file_name in file_list) {

    data <- data.table::fread(file_name)

    # exclude agents that are donors
    non_donor_data <- data[is_donor == FALSE]

    generation_time_data <- merge(
      non_donor_data,
      non_donor_data[, .(id, inf_time = infection_time)],
      by.x = "infector_id",
      by.y = "id",
      all.x = TRUE
    )

    # generation time as the difference between infection_time and inf_time of the infector
    generation_time_data[, generation_time := infection_time - inf_time]

    valid_generation_times <- generation_time_data[!is.na(generation_time) & generation_time > 0, generation_time]

    generation_times_list[[file_name]] <- valid_generation_times
  }

  all_generation_times <- unlist(generation_times_list)

  # summarize
  summary_stats <- data.table(
    median_generation_time = median(all_generation_times, na.rm = TRUE),
    lower_generation_time = quantile(all_generation_times, 0.25, na.rm = TRUE),
    upper_generation_time = quantile(all_generation_times, 0.75, na.rm = TRUE)
  )

  return(summary_stats)
}
