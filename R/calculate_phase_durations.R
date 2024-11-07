calculate_phase_durations <- function(data_path, sim_type = "R2R") {

  file_list <- list.files(
    path = data_path,
    pattern = if (sim_type == "R2R") "final_results_iteration_.*\\.csv" else "results_.*_herd_clin.csv",
    full.names = TRUE
  )

  process_file <- function(file_name) {
    dt <- data.table::fread(file_name)

    # infection time
    infection_times <- dt %>%
      filter(infection_status == "infected") %>%
      group_by(trial, id) %>%
      summarize(infection_time = ifelse(any(!is.na(time)), min(time, na.rm = TRUE), NA_real_), .groups = "drop")

    # infectious time
    infectious_times <- dt %>%
      filter(!is.na(infectious_t)) %>%
      group_by(trial, id) %>%
      summarize(infectious_t = ifelse(any(!is.na(infectious_t)), min(infectious_t, na.rm = TRUE), NA_real_), .groups = "drop")

    # clinical symptoms
    clinical_times <- dt %>%
      filter(score > 0) %>%
      group_by(trial, id) %>%
      summarize(clinical_symptom_time = ifelse(any(!is.na(time)), min(time, na.rm = TRUE), NA_real_), .groups = "drop")

    # merge
    animal_times <- infection_times %>%
      full_join(infectious_times, by = c("trial", "id")) %>%
      full_join(clinical_times, by = c("trial", "id"))

    # calculate durations
    animal_times <- animal_times %>%
      mutate(
        latent_period = ifelse(!is.na(infectious_t) & !is.na(infection_time), infectious_t - infection_time, NA_real_),
        subclinical_period = ifelse(!is.na(clinical_symptom_time) & !is.na(infectious_t), clinical_symptom_time - infectious_t, NA_real_),
        incubation_period = ifelse(!is.na(clinical_symptom_time) & !is.na(infection_time), clinical_symptom_time - infection_time, NA_real_)
      )

    if (sim_type == "R2R") {
      group_info <- dt %>%
        filter(!is.na(room)) %>%
        distinct(trial, id, is_donor, room)

      # merge and group assignment for R2R
      animal_times <- animal_times %>%
        left_join(group_info, by = c("trial", "id")) %>%
        mutate(
          room_number = as.numeric(str_extract(room, "\\d+")),
          group = case_when(
            is_donor ~ "Donor",
            room_number %in% 2:5 ~ paste0("Room ", room_number),
            TRUE ~ "Unknown"
          )
        )

    } else if (sim_type == "herd") {

      group_info <- dt %>%
        distinct(trial, id, is_donor)

      animal_times <- animal_times %>%
        left_join(group_info, by = c("trial", "id")) %>%
        mutate(
          group = case_when(
            is_donor ~ "Donor",
            TRUE ~ "Herd"
          )
        )
    }

    animal_times %>%
      select(trial, id, group, latent_period, subclinical_period, incubation_period)
  }

  # process each file
  summary_data <- file_list %>%
    map_dfr(process_file)

  # verify presence of columns before pivoting
  if (all(c("latent_period", "subclinical_period", "incubation_period") %in% colnames(summary_data))) {
    melted_data <- summary_data %>%
      pivot_longer(
        cols = c("latent_period", "subclinical_period", "incubation_period"),
        names_to = "Period",
        values_to = "Duration"
      ) %>%
      filter(!is.na(Duration)) %>%
      mutate(
        Period = factor(
          Period,
          levels = c("latent_period", "subclinical_period", "incubation_period"),
          labels = c("Latent", "Subclinical", "Incubation")
        )
      )

    melted_data %>%
      select(id, trial, group, Period, Duration)
  } else {
    stop("Required columns for pivoting are missing in the processed data.")
  }
}
