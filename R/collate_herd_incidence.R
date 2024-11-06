# may be slow for large simulations, see collate_herd_incidence_dt
collate_herd_incidence <- function(data_path, min_incidence = 2) {

  file_list <- list.files(
    path = data_path,
    pattern = "results_.*_herd_clin.csv",
    full.names = TRUE
  )

  process_file <- function(file_name) {
    data <- read_csv(file_name)
    trial_number <- as.numeric(gsub(".*results_(\\d+)_herd_clin\\.csv", "\\1", file_name))
    data <- data %>% mutate(trial = trial_number)
    return(data)
  }

  all_data <- bind_rows(lapply(file_list, process_file))

  # identify animals that were infected in each trial
  unique_infected_per_trial <- all_data %>%
    filter(infection_status == "infected") %>%
    group_by(trial) %>%
    summarize(unique_infected_animals = n_distinct(id), .groups = "drop")

  # filter trials with fewer infected animals than min_incidence
  trials_to_keep <- unique_infected_per_trial %>%
    filter(unique_infected_animals >= min_incidence) %>%
    pull(trial)

  filtered_data <- all_data %>%
    filter(trial %in% trials_to_keep)

  num_trials_filtered <- n_distinct(all_data$trial) - length(trials_to_keep)
  num_trials_retained <- length(trials_to_keep)

  message(sprintf("Number of trials filtered: %d", num_trials_filtered))
  message(sprintf("Number of trials retained: %d", num_trials_retained))

  infections_per_trial <- filtered_data %>%
    group_by(time, trial) %>%
    summarize(
      infected_count = sum(infection_status == "infected"),
      recovered_count = sum(infection_status == "recovered"),
      .groups = "drop"
    )

  infections_trend <- infections_per_trial %>%
    group_by(time) %>%
    summarize(
      median_infected = median(infected_count),
      lower_infected = quantile(infected_count, 0.025),
      upper_infected = quantile(infected_count, 0.975),
      median_recovered = median(recovered_count),
      lower_recovered = quantile(recovered_count, 0.025),
      upper_recovered = quantile(recovered_count, 0.975),
      .groups = "drop"
    )

  return(infections_trend)
}
