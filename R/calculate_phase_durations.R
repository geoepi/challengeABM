calculate_phase_durations <- function(data_path) {

  file_list <- list.files(
    path = data_path,
    pattern = "final_results_iteration_.*\\.csv",
    full.names = TRUE
  )

  process_file <- function(file_name) {

    dt <- read_csv(
      file_name,
      col_types = cols(
        time = col_double(),
        id = col_character(),
        infection_status = col_character(),
        infectious_t = col_double(),
        score = col_double(),
        is_donor = col_logical(),
        room = col_character(),
        trial = col_character()
      )
    )

    # infection time
    infection_times <- dt %>%
      filter(infection_status == "infected") %>%
      group_by(trial, id) %>%
      summarize(infection_time = min(time, na.rm = TRUE), .groups = "drop")

    # infectious time
    infectious_times <- dt %>%
      filter(!is.na(infectious_t)) %>%
      group_by(trial, id) %>%
      summarize(infectious_t = min(infectious_t, na.rm = TRUE), .groups = "drop")

    # clinical symptoms
    clinical_times <- dt %>%
      filter(score > 0) %>%
      group_by(trial, id) %>%
      summarize(clinical_symptom_time = min(time, na.rm = TRUE), .groups = "drop")

    # merge
    animal_times <- infection_times %>%
      full_join(infectious_times, by = c("trial", "id")) %>%
      full_join(clinical_times, by = c("trial", "id"))

    # calculate durations
    animal_times <- animal_times %>%
      mutate(
        latent_period = infectious_t - infection_time,
        subclinical_period = clinical_symptom_time - infectious_t,
        incubation_period = clinical_symptom_time - infection_time
      )

    group_info <- dt %>%
      filter(!is.na(room)) %>%
      distinct(trial, id, is_donor, room)

    # merge
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

    animal_times %>%
      select(trial, id, group, latent_period, subclinical_period, incubation_period)
  }

  # process each file
  summary_data <- file_list %>%
    map_dfr(process_file)

  melted_data <- summary_data %>%
    filter(!is.na(latent_period), !is.na(subclinical_period), !is.na(incubation_period)) %>%
    pivot_longer(
      cols = c("latent_period", "subclinical_period", "incubation_period"),
      names_to = "Period",
      values_to = "Duration"
    ) %>%
    mutate(
      Period = factor(
        Period,
        levels = c("latent_period", "subclinical_period", "incubation_period"),
        labels = c("Latent", "Subclinical", "Incubation")
      )
    )

  melted_data %>%
    select(id, trial, group, Period, Duration)
}
