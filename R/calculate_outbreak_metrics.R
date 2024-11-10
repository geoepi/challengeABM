calculate_outbreak_metrics <- function(data_path, min_incidence = 2) {

  file_list <- list.files(path = data_path, pattern = "results_.*_herd_clin.csv", full.names = TRUE)
  process_file <- function(file_name) {
    data <- data.table::fread(file_name)
    data[, trial := as.numeric(gsub(".*results_(\\d+)_herd_clin\\.csv", "\\1", file_name))]
    return(data)
  }
  all_data <- data.table::rbindlist(lapply(file_list, process_file))

  # unique infected animals per trial
  unique_infected_per_trial <- all_data[, .(
    unique_infected_animals = uniqueN(id[virus_nasal > 0 | virus_serum > 0])
  ), by = trial]

  # filter trials based on min_incidence
  trials_to_keep <- unique_infected_per_trial[unique_infected_animals >= min_incidence, trial]
  filtered_data <- all_data[trial %in% trials_to_keep]

  # Create logical columns for infections and symptoms
  filtered_data[, `:=`(
    infected = (virus_nasal > 0 | virus_serum > 0),
    has_symptoms = (score > 0)
  )]

  # infection start time for each agent
  infection_times <- filtered_data[infected == TRUE, .(
    infection_time = min(time)
  ), by = .(trial, id)]

  filtered_data <- merge(filtered_data, infection_times, by = c("trial", "id"), all.x = TRUE)

  # elapsed time since infection
  filtered_data[, `:=`(
    time_since_infection = time - infection_time
  )]

  # herd level using global time
  peak_info <- filtered_data[infected == TRUE, .(
    num_infected = uniqueN(id)
  ), by = .(trial, time)]
  peak_info <- peak_info[, .(
    peak_t = time[which.max(num_infected)],
    max_infected = max(num_infected)
  ), by = trial]

  titer_info <- filtered_data[, .(
    titer_nasal = max(virus_nasal, na.rm = TRUE),
    titer_nasal_t = time_since_infection[which.max(virus_nasal)],
    titer_serum = max(virus_serum, na.rm = TRUE),
    titer_serum_t = time_since_infection[which.max(virus_serum)]
  ), by = trial]

  first_detection_info <- filtered_data[has_symptoms == TRUE, .(
    first_detection = min(time)
  ), by = trial]

  latent_info <- merge(filtered_data, first_detection_info, by = "trial")
  latent_info <- latent_info[infected == TRUE & time == first_detection, .(
    latent_n = uniqueN(id)
  ), by = trial]

  metrics <- Reduce(function(x, y) merge(x, y, by = "trial", all = TRUE),
                    list(peak_info, first_detection_info, latent_info, titer_info))

  # summarize
  summary_metrics <- metrics[, .(
    peak_t_median = median(peak_t, na.rm = TRUE)/24,
    peak_t_q25 = quantile(peak_t, 0.25, na.rm = TRUE)/24,
    peak_t_q75 = quantile(peak_t, 0.75, na.rm = TRUE)/24,
    first_detection_median = median(first_detection, na.rm = TRUE)/24,
    first_detection_q25 = quantile(first_detection, 0.25, na.rm = TRUE)/24,
    first_detection_q75 = quantile(first_detection, 0.75, na.rm = TRUE)/24,
    latent_n_median = median(latent_n, na.rm = TRUE),
    latent_n_q25 = quantile(latent_n, 0.25, na.rm = TRUE),
    latent_n_q75 = quantile(latent_n, 0.75, na.rm = TRUE),
    titer_nasal_median = median(titer_nasal, na.rm = TRUE),
    titer_nasal_q25 = quantile(titer_nasal, 0.25, na.rm = TRUE),
    titer_nasal_q75 = quantile(titer_nasal, 0.75, na.rm = TRUE),
    titer_nasal_t_median = median(titer_nasal_t, na.rm = TRUE)/24,
    titer_nasal_t_q25 = quantile(titer_nasal_t, 0.25, na.rm = TRUE)/24,
    titer_nasal_t_q75 = quantile(titer_nasal_t, 0.75, na.rm = TRUE)/24,
    titer_serum_median = median(titer_serum, na.rm = TRUE),
    titer_serum_q25 = quantile(titer_serum, 0.25, na.rm = TRUE),
    titer_serum_q75 = quantile(titer_serum, 0.75, na.rm = TRUE),
    titer_serum_t_median = median(titer_serum_t, na.rm = TRUE)/24,
    titer_serum_t_q25 = quantile(titer_serum_t, 0.25, na.rm = TRUE)/24,
    titer_serum_t_q75 = quantile(titer_serum_t, 0.75, na.rm = TRUE)/24
  )]

  # reshape for readability
  summary_metrics_long <- data.table::melt(
    summary_metrics,
    measure.vars = names(summary_metrics),
    variable.name = "Metric",
    value.name = "Value"
  )

  summary_metrics_long[, `:=`(
    Metric_Name = sub("_(median|q25|q75)$", "", Metric),
    Statistic = sub("^.*_(median|q25|q75)$", "\\1", Metric)
  )]

  reshaped_metrics <- dcast(
    summary_metrics_long,
    Metric_Name ~ Statistic,
    value.var = "Value",
    fun.aggregate = mean
  )

  setnames(reshaped_metrics, c("Metric_Name", "median", "q25", "q75"))

  reshaped_metrics[, `:=`(
    median = round(median, 1),
    q25 = round(q25, 1),
    q75 = round(q75, 1)
  )]

  return(reshaped_metrics)
}
