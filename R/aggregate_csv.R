aggregate_csv <- function(directory_path, output_path = NULL, fname = NULL) {

  require(data.table)

  file_list <- list.files(path = directory_path, pattern = "\\.csv$", full.names = TRUE)

  combined_df <- data.table::rbindlist(
    lapply(file_list, data.table::fread),
    use.names = TRUE,
    fill = TRUE
  )

  if (!is.null(output_path)) {

    fname <- paste0(output_path,"/", fname, ".csv")
    data.table::fwrite(combined_df, file = fname)
  }

  return(combined_df)
}
