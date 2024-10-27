calculate_phase_durations <- function(data_path) {

  file_list <- list.files(
    path = data_path,
    pattern = "final_results_iteration_.*\\.csv",
    full.names = TRUE
  )


  process_file <- function(file_name) {

    dt <- fread(
      file_name,
      select = c("time", "id", "infection_status", "infectious_t", "score", "is_donor", "room", "trial")
    )

    # data types
    dt[, time := as.numeric(time)]
    dt[, infectious_t := as.numeric(infectious_t)]
    dt[, score := as.numeric(score)]

    # infection time
    infection_times <- dt[infection_status == "infected", .(infection_time = min(time)), by = .(trial, id)]

    # infectious time
    infectious_times <- dt[!is.na(infectious_t), .(infectious_t = min(infectious_t)), by = .(trial, id)]

    # Calculate clinical symptom time
    clinical_times <- dt[score > 0, .(clinical_symptom_time = min(time)), by = .(trial, id)]

    animal_times <- merge(infection_times, infectious_times, by = c("trial", "id"), all = TRUE)
    animal_times <- merge(animal_times, clinical_times, by = c("trial", "id"), all = TRUE)

    # durations
    animal_times[, latent_period := infectious_t - infection_time]
    animal_times[, subclinical_period := clinical_symptom_time - infectious_t]
    animal_times[, incubation_period := clinical_symptom_time - infection_time]

    group_info <- dt[!is.na(room), .(is_donor = unique(is_donor), room = unique(room)), by = .(trial, id)]
    animal_times <- merge(animal_times, group_info, by = c("trial", "id"), all.x = TRUE)
    animal_times[, room_number := as.numeric(gsub("[^0-9]", "", room))]

    # labels
    animal_times[, group := ifelse(is_donor == TRUE, "Donor",
                                   ifelse(room_number %in% 2:5, paste0("Room ", room_number), "Unknown"))]

    return(animal_times)
  }

  summary_list <- lapply(file_list, process_file)
  summary_data <- rbindlist(summary_list, use.names = TRUE, fill = TRUE)

  summary_data[is.na(group), group := "Unknown"]

  summary_data <- summary_data[!is.na(latent_period) & !is.na(subclinical_period) & !is.na(incubation_period)]

  melted_data <- melt(
    summary_data,
    id.vars = c("id", "trial", "group"),
    measure.vars = c("latent_period", "subclinical_period", "incubation_period"),
    variable.name = "Period",
    value.name = "Duration"
  )

  # labels
  melted_data[, Period := factor(
    Period,
    levels = c("latent_period", "subclinical_period", "incubation_period"),
    labels = c("Latent", "Subclinical", "Incubation")
  )]

  return(melted_data)
}
