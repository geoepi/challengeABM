# uses data.table
calculate_outbreak_metrics <- function(data_path, min_incidence = 5) {

  file_list <- list.files(
    path = data_path,
    pattern = "results_.*_herd_clin.csv",
    full.names = TRUE
  )

  process_file <- function(file_name) {
    data <- vroom(file_name, delim = ",", show_col_types = FALSE)
    data <- as.data.table(data)
    data[, trial := as.numeric(gsub(".*results_(\\d+)_herd_clin\\.csv", "\\1", file_name))]
    return(data)
  }

  all_data <- rbindlist(lapply(file_list, process_file))
  setDT(all_data)

  unique_infected_per_trial <- all_data[, .(
    unique_infected_animals = uniqueN(id[virus_nasal > 0 | virus_serum > 0])
  ), by = trial]

  trials_to_keep <- unique_infected_per_trial[unique_infected_animals >= min_incidence, trial]
  filtered_data <- all_data[trial %in% trials_to_keep]

  filtered_data[, infected := (virus_nasal > 0 | virus_serum > 0)]
  filtered_data[, has_symptoms := (score > 0)]

  # time of max prevalence
  peak_info <- filtered_data[infected == TRUE, .(
    num_infected = uniqueN(id)
  ), by = .(trial, time)]
  peak_info <- peak_info[, .(
    peak_t = time[which.max(num_infected)],
    max_infected = max(num_infected)
  ), by = trial]

  # earliest time at which any animal has score > 0 (first_detection)
  first_detection_info <- filtered_data[has_symptoms == TRUE, .(
    first_detection = min(time)
  ), by = trial]

  # number of infected animals at first_detection (latent_n)
  latent_info <- merge(filtered_data, first_detection_info, by = "trial")
  latent_info <- latent_info[infected == TRUE & time == first_detection, .(
    latent_n = uniqueN(id)
  ), by = trial]

  metrics <- merge(peak_info, first_detection_info, by = "trial", all = TRUE)
  metrics <- merge(metrics, latent_info, by = "trial", all = TRUE)

  summary_metrics <- metrics[, .(
    peak_t_median = median(peak_t, na.rm = TRUE),
    peak_t_lower = quantile(peak_t, 0.25, na.rm = TRUE),
    peak_t_upper = quantile(peak_t, 0.75, na.rm = TRUE),
    first_detection_median = median(first_detection, na.rm = TRUE),
    first_detection_lower = quantile(first_detection, 0.25, na.rm = TRUE),
    first_detection_upper = quantile(first_detection, 0.75, na.rm = TRUE),
    latent_n_median = median(latent_n, na.rm = TRUE),
    latent_n_lower = quantile(latent_n, 0.25, na.rm = TRUE),
    latent_n_upper = quantile(latent_n, 0.75, na.rm = TRUE)
  )]

  return(summary_metrics)
}
