#' Build dose vs incubation table
#'
#' Joins incubation periods with agent metadata to facilitate dose response
#' plotting.
#'
#' @param path Directory of simulation results.
#' @param periods_melted Output from `calculate_phase_durations()`.
#' @return Data frame with dose and incubation time for each animal.
get_dose_table <- function(path, periods_melted) {

  # filter and summarize incubation period
  df_incub <- periods_melted %>%
    filter(Period %in% c("Latent", "Subclinical")) %>%
    mutate(id_trial = paste(id, trial, sep = "_")) %>%
    group_by(id_trial) %>%
    summarise(incub = sum(Duration, na.rm = TRUE))

  # trial agents metadata
  trials_agents <- get_agent_meta(path)

  # create table
  dose_plt <- trials_agents %>%
    mutate(id_trial = paste(id, trial, sep = "_")) %>%
    select(id_trial, dose) %>%
    left_join(df_incub, by = "id_trial") %>%
    filter(!is.na(dose) & !is.na(incub))

  return(dose_plt)
}
