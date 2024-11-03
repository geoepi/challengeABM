calculate_proportion_preclinical <- function(data_path, n_iterations = 100, seed = 123) {

  set.seed(seed) # for bootstrapping

  # files
  file_list <- list.files(
    path = data_path,
    pattern = "final_results_iteration_.*\\.csv",
    full.names = TRUE
  )

  # process each file
  proportion_preclinical <- function(file_name) {

    dt <- read_csv(
      file_name,
      col_types = cols(
        time = col_double(),
        id = col_character(),
        infection_status = col_character(),
        virus_nasal = col_double(),
        virus_serum = col_double(),
        score = col_double(),
        trial = col_integer()
      )
    )

    # infected animals by trial
    infected_animals <- dt %>%
      filter(infection_status == "infected") %>%
      distinct(trial, id)

    if (nrow(infected_animals) == 0) {
      return(NULL)
    }

    # infected animals
    dt_infected <- dt %>%
      inner_join(infected_animals, by = c("trial", "id")) %>%
      filter(!is.na(virus_nasal) & virus_nasal >= 0 & !is.na(virus_serum) & virus_serum >= 0)

    if (nrow(dt_infected) == 0) {
      return(NULL)
    }

    results_list <- list()

    for (virus_type in c("virus_nasal", "virus_serum")) {
      dt_infected_copy <- dt_infected %>%
        mutate(S_tau = .data[[virus_type]])

      # clinical onset
      clinical_onset <- dt_infected_copy %>%
        filter(score > 0) %>%
        group_by(trial, id) %>%
        summarize(t_clinical = min(time, na.rm = TRUE), .groups = "drop")

      # merge back
      dt_infected_copy <- dt_infected_copy %>%
        left_join(clinical_onset, by = c("trial", "id"))

      # time since infection
      infection_times <- dt_infected_copy %>%
        group_by(trial, id) %>%
        summarize(t_infection = min(time, na.rm = TRUE), .groups = "drop")

      dt_infected_copy <- dt_infected_copy %>%
        left_join(infection_times, by = c("trial", "id")) %>%
        mutate(
          tau = time - t_infection,
          pre_clinical = if_else(is.na(t_clinical), TRUE, time < t_clinical)
        )

      # calculate
      dt_infected_copy <- dt_infected_copy %>%
        arrange(trial, id, tau) %>%
        group_by(trial, id) %>%
        mutate(
          delta_tau = lead(tau, default = last(tau)) - tau,
          delta_tau = replace_na(delta_tau, 0),
          S_delta_tau = S_tau * delta_tau,
          S_pre_clinical = S_tau * pre_clinical * delta_tau
        ) %>%
        ungroup()

      # aggregate by animal
      results <- dt_infected_copy %>%
        group_by(trial, id) %>%
        summarize(
          virus_type = virus_type,
          theta_numerator = sum(S_pre_clinical, na.rm = TRUE),
          theta_denominator = sum(S_delta_tau, na.rm = TRUE),
          .groups = "drop"
        )

      results_list[[virus_type]] <- results
    }

    combined_results <- bind_rows(results_list)
    return(combined_results)
  }

  # process and combine
  results_list <- file_list %>%
    map(proportion_preclinical) %>%
    compact()

  all_results <- bind_rows(results_list)

  # calculate theta by animal
  all_results <- all_results %>%
    mutate(theta = theta_numerator / theta_denominator)

  # bootstrapping for confidence intervals
  bootstrap_results <- replicate(n_iterations, {
    sample_data <- all_results %>%
      sample_frac(replace = TRUE)

    sample_data %>%
      group_by(virus_type) %>%
      summarize(
        theta = sum(theta_numerator, na.rm = TRUE) / sum(theta_denominator, na.rm = TRUE),
        .groups = "drop"
      )
  }, simplify = FALSE) %>%
    bind_rows(.id = "iteration")

  confint_intervals <- bootstrap_results %>%
    group_by(virus_type) %>%
    summarize(
      theta_mean = mean(theta),
      lower_ci = quantile(theta, probs = 0.025),
      upper_ci = quantile(theta, probs = 0.975),
      .groups = "drop"
    )

  return(confint_intervals)
}
