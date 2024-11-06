calculate_outbreak_metrics <- function(data_path, min_incidence = 2) {
  # Ensure data.table functions are used throughout

  file_list <- list.files(
    path = data_path,
    pattern = "results_.*_herd_clin.csv",
    full.names = TRUE
  )

  process_file <- function(file_name) {
    data <- data.table::fread(file_name)
    data[, trial := as.numeric(gsub(".*results_(\\d+)_herd_clin\\.csv", "\\1", file_name))]
    return(data)
  }

  all_data <- data.table::rbindlist(lapply(file_list, process_file))

  # Identify unique infected animals per trial
  unique_infected_per_trial <- all_data[, .(
    unique_infected_animals = uniqueN(id[virus_nasal > 0 | virus_serum > 0])
  ), by = trial]

  # Filter out trials with fewer infected animals than min_incidence
  trials_to_keep <- unique_infected_per_trial[unique_infected_animals >= min_incidence, trial]
  filtered_data <- all_data[trial %in% trials_to_keep]

  # Create new columns indicating infected status and symptom status
  filtered_data[, infected := (virus_nasal > 0 | virus_serum > 0)]
  filtered_data[, has_symptoms := (score > 0)]

  # Calculate time of max prevalence
  peak_info <- filtered_data[infected == TRUE, .(
    num_infected = uniqueN(id)
  ), by = .(trial, time)]
  peak_info <- peak_info[, .(
    peak_t = time[which.max(num_infected)],
    max_infected = max(num_infected)
  ), by = trial]

  # Calculate earliest time of symptom detection
  first_detection_info <- filtered_data[has_symptoms == TRUE, .(
    first_detection = min(time)
  ), by = trial]

  # Calculate the number of infected animals at first detection
  latent_info <- merge(filtered_data, first_detection_info, by = "trial")
  latent_info <- latent_info[infected == TRUE & time == first_detection, .(
    latent_n = uniqueN(id)
  ), by = trial]

  # Calculate peak titer quantity and time for virus_nasal and virus_serum considering unique individuals
  titer_info <- filtered_data[, .(
    titer_nasal = max(virus_nasal[id == id], na.rm = TRUE),
    titer_nasal_t = time[which.max(virus_nasal[id == id])],
    titer_serum = max(virus_serum[id == id], na.rm = TRUE),
    titer_serum_t = time[which.max(virus_serum[id == id])]
  ), by = trial]

  # Merge all metric data tables
  metrics <- merge(peak_info, first_detection_info, by = "trial", all = TRUE)
  metrics <- merge(metrics, latent_info, by = "trial", all = TRUE)
  metrics <- merge(metrics, titer_info, by = "trial", all = TRUE)

  # Summarize metrics with median and quantiles
  summary_metrics <- metrics[, .(
    peak_t_median = median(peak_t, na.rm = TRUE),
    peak_t_lower = quantile(peak_t, 0.25, na.rm = TRUE),
    peak_t_upper = quantile(peak_t, 0.75, na.rm = TRUE),
    first_detection_median = median(first_detection, na.rm = TRUE),
    first_detection_lower = quantile(first_detection, 0.25, na.rm = TRUE),
    first_detection_upper = quantile(first_detection, 0.75, na.rm = TRUE),
    latent_n_median = median(latent_n, na.rm = TRUE),
    latent_n_lower = quantile(latent_n, 0.25, na.rm = TRUE),
    latent_n_upper = quantile(latent_n, 0.75, na.rm = TRUE),
    titer_nasal_median = median(titer_nasal, na.rm = TRUE),
    titer_nasal_lower = quantile(titer_nasal, 0.25, na.rm = TRUE),
    titer_nasal_upper = quantile(titer_nasal, 0.75, na.rm = TRUE),
    titer_nasal_t_median = median(titer_nasal_t, na.rm = TRUE),
    titer_nasal_t_lower = quantile(titer_nasal_t, 0.25, na.rm = TRUE),
    titer_nasal_t_upper = quantile(titer_nasal_t, 0.75, na.rm = TRUE),
    titer_serum_median = median(titer_serum, na.rm = TRUE),
    titer_serum_lower = quantile(titer_serum, 0.25, na.rm = TRUE),
    titer_serum_upper = quantile(titer_serum, 0.75, na.rm = TRUE),
    titer_serum_t_median = median(titer_serum_t, na.rm = TRUE),
    titer_serum_t_lower = quantile(titer_serum_t, 0.25, na.rm = TRUE),
    titer_serum_t_upper = quantile(titer_serum_t, 0.75, na.rm = TRUE)
  )]

  # Reshape summary metrics to have columns for median, lower, and upper quantiles
  summary_metrics_long <- data.table::melt(
    summary_metrics,
    measure.vars = names(summary_metrics),
    variable.name = "Metric",
    value.name = "Value"
  )

  # Extract the stem names for each metric
  summary_metrics_long[, `:=`(StatType = sub("_(median|lower|upper)$", "", Metric),
                              Quantile = sub("^.*_(median|lower|upper)$", "\\1", Metric))]

  # Reshape to wide format with separate columns for median, lower, and upper
  final_metrics <- dcast(
    summary_metrics_long,
    StatType ~ Quantile,
    value.var = "Value"
  )

  setnames(final_metrics, c("StatType", "median", "lower", "upper"))

  final_metrics[, `:=`(median = round(median, 1),
                       lower = round(lower, 1),
                       upper = round(upper, 1))]

  return(final_metrics)
}
