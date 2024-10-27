calculate_proportion_preclinical <- function(data_path, n_iterations = 1000, seed = 123) {

  set.seed(seed)

  # CSV files
  file_list <- list.files(
    path = data_path,
    pattern = "final_results_iteration_.*\\.csv",
    full.names = TRUE
  )

  # function to process each file
  proportion_preclinical <- function(file_name) {

    dt <- fread(
      file_name,
      select = c("time", "id", "infection_status", "virus_nasal", "virus_serum", "score", "trial")
    )

    dt[, time := as.numeric(time)]
    dt[, virus_nasal := as.numeric(virus_nasal)]
    dt[, virus_serum := as.numeric(virus_serum)]
    dt[, score := as.numeric(score)]
    dt[, trial := as.integer(trial)]

    # filter infected animals by trial
    infected_animals <- unique(dt[infection_status == "infected", .(trial, id)])

    if (nrow(infected_animals) == 0) {
      return(NULL)
    }

    dt_infected <- merge(dt, infected_animals, by = c("trial", "id"))

    dt_infected <- dt_infected[
      (!is.na(virus_nasal) & virus_nasal >= 0) & (!is.na(virus_serum) & virus_serum >= 0)
    ]

    if (nrow(dt_infected) == 0) {
      return(NULL)
    }

    results_list <- list()

    for (virus_type in c("virus_nasal", "virus_serum")) {

      dt_infected_copy <- copy(dt_infected)

      # Set S(τ)
      dt_infected_copy[, S_tau := get(virus_type)]

      # clinical onset
      clinical_onset <- dt_infected_copy[score > 0, .(t_clinical = min(time)), by = .(trial, id)]

      # Merge clinical onset times back into main data
      dt_infected_copy <- merge(dt_infected_copy, clinical_onset, by = c("trial", "id"), all.x = TRUE)

      # time since infection
      infection_times <- dt_infected_copy[, .(t_infection = min(time)), by = .(trial, id)]
      dt_infected_copy <- merge(dt_infected_copy, infection_times, by = c("trial", "id"), all.x = TRUE)
      dt_infected_copy[, tau := time - t_infection]

      # all before clinical
      dt_infected_copy[, pre_clinical := ifelse(is.na(t_clinical), TRUE, time < t_clinical)]

      # time intervals
      dt_infected_copy <- dt_infected_copy[order(trial, id, tau)]
      dt_infected_copy[, delta_tau := shift(tau, type = "lead") - tau, by = .(trial, id)]
      dt_infected_copy[is.na(delta_tau), delta_tau := 0]  # Last time point

      dt_infected_copy[, S_delta_tau := S_tau * delta_tau]
      dt_infected_copy[, S_pre_clinical := S_tau * pre_clinical * delta_tau]

      # aggregate by animal
      results <- dt_infected_copy[, .(
        virus_type = virus_type,
        theta_numerator = sum(S_pre_clinical, na.rm = TRUE),
        theta_denominator = sum(S_delta_tau, na.rm = TRUE)
      ), by = .(trial, id)]

      results_list[[virus_type]] <- results
    }

    combined_results <- rbindlist(results_list)

    return(combined_results)
  }

  results_list <- list()

  # each file
  for (file_name in file_list) {
    file_results <- proportion_preclinical(file_name)
    if (!is.null(file_results)) {
      results_list[[length(results_list) + 1]] <- file_results
    }
  }

  # combine
  all_results <- rbindlist(results_list, use.names = TRUE, fill = TRUE)

  # calculate theta by animal
  all_results[, theta := theta_numerator / theta_denominator]

  # bootstrapping for confint intervals
  bootstrap_results <- data.table()
  for (i in 1:n_iterations) {
    sample_data <- all_results[sample(.N, replace = TRUE)]
    theta_values <- sample_data[, .(
      theta = sum(theta_numerator, na.rm = TRUE) / sum(theta_denominator, na.rm = TRUE)
    ), by = virus_type]
    theta_values[, iteration := i]
    bootstrap_results <- rbindlist(list(bootstrap_results, theta_values), use.names = TRUE)
  }

  confint_intervals <- bootstrap_results[, .(
    theta_mean = mean(theta),
    lower_ci = quantile(theta, probs = 0.025),
    upper_ci = quantile(theta, probs = 0.975)
  ), by = virus_type]

  return(confint_intervals)
}
