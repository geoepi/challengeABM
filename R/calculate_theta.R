#' Calculate proportion of shedding pre clinical
#'
#' Computes theta – the proportion of viral shedding that occurs before
#' clinical symptoms – from herd simulation outputs.
#'
#' @param data_path Directory of herd result CSV files.
#' @return A long data frame of summary statistics for theta.
calculate_theta <- function(data_path) {

  file_list <- list.files(
    path = data_path,
    pattern = "results_.*_herd_clin.csv",
    full.names = TRUE
  )

  process_file <- function(file_name) {
    dt <- data.table::fread(file_name)

    clinical_times <- dt %>%
      filter(score > 0) %>%
      group_by(trial, id) %>%
      summarize(
        clinical_symptom_time = ifelse(n() > 0, min(time, na.rm = TRUE), NA_real_),
        .groups = "drop"
      )

    if (nrow(clinical_times) == 0) return(NULL)

    dt <- dt %>%
      semi_join(clinical_times, by = c("trial", "id"))

    dt <- dt %>%
      left_join(clinical_times, by = c("trial", "id")) %>%
      mutate(
        before_clinical = ifelse(!is.na(clinical_symptom_time) & time < clinical_symptom_time, TRUE, FALSE),
        viral_shedding_nasal_pre_clinical = ifelse(before_clinical, virus_nasal, 0),
        viral_shedding_serum_pre_clinical = ifelse(before_clinical, virus_serum, 0)
      )

    summary_theta <- dt %>%
      group_by(trial, id) %>%
      summarize(
        total_shedding_nasal = sum(virus_nasal, na.rm = TRUE),
        pre_clinical_shedding_nasal = sum(viral_shedding_nasal_pre_clinical, na.rm = TRUE),
        total_shedding_serum = sum(virus_serum, na.rm = TRUE),
        pre_clinical_shedding_serum = sum(viral_shedding_serum_pre_clinical, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        theta_nasal_pre_clinical = pre_clinical_shedding_nasal / total_shedding_nasal,
        theta_serum_pre_clinical = pre_clinical_shedding_serum / total_shedding_serum
      ) %>%
      filter(!is.na(theta_nasal_pre_clinical), !is.na(theta_serum_pre_clinical))

    return(summary_theta)
  }

  theta_data <- file_list %>%
    map_dfr(process_file)

  theta_summary <- theta_data %>%
    summarize(
      median_theta_nasal_pre_clinical = median(theta_nasal_pre_clinical, na.rm = TRUE),
      lower_theta_nasal_pre_clinical = quantile(theta_nasal_pre_clinical, 0.025, na.rm = TRUE),
      upper_theta_nasal_pre_clinical = quantile(theta_nasal_pre_clinical, 0.975, na.rm = TRUE),
      median_theta_serum_pre_clinical = median(theta_serum_pre_clinical, na.rm = TRUE),
      lower_theta_serum_pre_clinical = quantile(theta_serum_pre_clinical, 0.025, na.rm = TRUE),
      upper_theta_serum_pre_clinical = quantile(theta_serum_pre_clinical, 0.975, na.rm = TRUE)
    ) %>%
    as.data.frame() %>%
    pivot_longer(cols = everything(), names_to = "Statistic", values_to = "Value")

  return(theta_summary)
}
