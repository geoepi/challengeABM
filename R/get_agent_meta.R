get_agent_meta <- function(data_path, myVars = c("id", "trial", "dose")) {

  file_list <- list.files(
    path = data_path,
    pattern = "agents_results_iteration_.*\\.csv",
    full.names = TRUE
  )

  process_file <- function(file_name) {
    dt <- read_csv(file_name) %>%
      select(all_of(myVars))
    return(dt)
  }

  all_data <- lapply(file_list, process_file) %>%
    bind_rows()

  return(all_data)
}
