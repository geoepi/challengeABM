#' Extract infection phase durations for animals
#'
#' Reads simulation output and calculates latent, subclinical and incubation
#' periods for each animal.
#'
#' @param data_path Folder with simulation CSV files.
#' @param sim_type Either `"R2R"` or `"herd"` indicating file naming.
#' @return A tibble with one row per animal and period.
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
      summarize(infection_time = ifelse(any(!is.na(time)), min(time, na.rm = TRUE), NA_real_), .groups = "drop") %>%
      distinct(trial, id, .keep_all = TRUE)

    # infectious time
    infectious_times <- dt %>%
      filter(!is.na(infectious_t)) %>%
      group_by(trial, id) %>%
      summarize(infectious_t = ifelse(any(!is.na(infectious_t)), min(infectious_t, na.rm = TRUE), NA_real_), .groups = "drop") %>%
      distinct(trial, id, .keep_all = TRUE)

    # clinical symptoms
    clinical_times <- dt %>%
      filter(score > 0) %>%
      group_by(trial, id) %>%
      summarize(clinical_symptom_time = ifelse(any(!is.na(time)), min(time, na.rm = TRUE), NA_real_), .groups = "drop") %>%
      distinct(trial, id, .keep_all = TRUE)

    # merge
    animal_times <- infection_times %>%
      full_join(infectious_times, by = c("trial", "id")) %>%
      full_join(clinical_times, by = c("trial", "id")) %>%
      distinct(trial, id, .keep_all = TRUE)  # ensure no duplicates after merging

    # calculate duration
    animal_times <- animal_times %>%
      mutate(
        latent_period = ifelse(!is.na(infectious_t) & !is.na(infection_time), infectious_t - infection_time, NA_real_),
        subclinical_period = ifelse(!is.na(clinical_symptom_time) & !is.na(infectious_t), clinical_symptom_time - infectious_t, NA_real_),
        incubation_period = ifelse(!is.na(clinical_symptom_time) & !is.na(infection_time), clinical_symptom_time - infection_time, NA_real_)
      )

    if (sim_type == "R2R") {
      group_info <- dt %>%
        filter(!is.na(room)) %>%
        distinct(trial, id, is_donor, room) %>%
        distinct(trial, id, .keep_all = TRUE)

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
        distinct(trial, id, is_donor) %>%
        distinct(trial, id, .keep_all = TRUE)

      animal_times <- animal_times %>%
        left_join(group_info, by = c("trial", "id")) %>%
        mutate(
          group = case_when(
            is_donor ~ "Donor",
            TRUE ~ "Herd"
          )
        )
    }

    # ensure no duplicates
    animal_times %>%
      select(trial, id, group, latent_period, subclinical_period, incubation_period) %>%
      distinct(trial, id, group, .keep_all = TRUE)
  }

  # process each file
  summary_data <- file_list %>%
    map_dfr(process_file)

  # verify columns
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
      ) %>%
      distinct(trial, id, group, Period, .keep_all = TRUE)

    return(melted_data %>%
      select(id, trial, group, Period, Duration))
  } else {
    stop("Required columns missing.")
  }
}
