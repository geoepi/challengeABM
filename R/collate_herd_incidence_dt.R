collate_herd_incidence_dt <- function(data_path, min_incidence = 2) {
  # uses data.table

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

  data.table::setDT(all_data)

  # identify unique animals that were infected in each trial
  unique_infected_per_trial <- all_data[, .(
    unique_infected_animals = data.table::uniqueN(id[infection_status == "infected"])
  ), by = trial]

  # filter trials with fewer infected animals than min_incidence
  trials_to_keep <- unique_infected_per_trial[unique_infected_animals >= min_incidence, trial]
  filtered_data <- all_data[trial %in% trials_to_keep]

  num_trials_filtered <- length(unique(unique_infected_per_trial$trial)) - length(trials_to_keep)
  num_trials_retained <- length(trials_to_keep)

  message(sprintf("Number of trials filtered: %d", num_trials_filtered))
  message(sprintf("Number of trials retained: %d", num_trials_retained))

  infections_per_trial <- filtered_data[, .(
    infected_count = sum(infection_status == "infected"),
    recovered_count = sum(infection_status == "recovered")
  ), by = .(time, trial)]

  # stats
  infections_trend <- infections_per_trial[, .(
    median_infected = median(infected_count),
    lower_infected = quantile(infected_count, 0.025),
    upper_infected = quantile(infected_count, 0.975),
    median_recovered = median(recovered_count),
    lower_recovered = quantile(recovered_count, 0.025),
    upper_recovered = quantile(recovered_count, 0.975)
  ), by = time]

  return(infections_trend)
}
