#' Retrieve agent metadata from result files
#'
#' Reads selected columns from all agent result files in a directory and
#' combines them into one table.
#'
#' @param data_path Directory containing agent result CSVs.
#' @param myVars Character vector of columns to keep.
#' @return A combined data frame of selected metadata.
get_agent_meta <- function(data_path, myVars = c("id", "trial", "dose")) {

  file_list <- list.files(
    path = data_path,
    pattern = "agents_results_iteration_.*\\.csv|agent_.*_herd_clin\\.csv",
    full.names = TRUE
  )

  process_file <- function(file_name) {
    dt <- read_csv(file_name, show_col_types = FALSE) %>%
      select(all_of(myVars))
    return(dt)
  }

  all_data <- lapply(file_list, process_file) %>%
    bind_rows()

  return(all_data)
}
