calculate_no_infection_percent <- function(data_path) {

  file_list <- list.files(
    path = data_path,
    pattern = "final_results_iteration_.*\\.csv",
    full.names = TRUE
  )

  # unique animal IDs in each room
  room_picks <- list(
    "2" = c(3:6),
    "3" = c(7:10),
    "4" = c(11:14),
    "5" = c(15:18)
  )

  results <- list()

  for (room in names(room_picks)) {
    uniq_ids <- room_picks[[room]]

    # counters
    total_trials <- 0
    room_no_infection_count <- 0

    for (file_name in file_list) {

      dt <- fread(file_name,
                  select = c("id", "trial", "infection_status", "is_donor", "room"),
                  nThread = 4,
                  colClasses = list(integer = c("id", "trial"),
                                    logical = "is_donor",
                                    character = c("infection_status", "room")))

      trials_in_file <- unique(dt$trial)
      total_trials <- total_trials + length(trials_in_file)

      for (trial_id in trials_in_file) {
        trial_data <- dt %>% filter(trial == trial_id)

        room_infections <- trial_data %>%
          filter(id %in% uniq_ids & infection_status == "infected")

        if (nrow(room_infections) == 0) {
          room_no_infection_count <- room_no_infection_count + 1
        }
      }
    }

    percentage_no_infection <- (room_no_infection_count / total_trials) * 100
    results[[room]] <- percentage_no_infection
  }

  results_df <- data.frame(
    Room = names(results),
    percentage_no_infection = unlist(results)
  )

  return(results_df)
}
