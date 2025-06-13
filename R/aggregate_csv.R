#' Aggregate CSV files
#'
#' Combine all CSV files in a directory into a single data table. The result
#' can optionally be written to a new CSV file.
#'
#' @param directory_path Path containing CSV files to read.
#' @param output_path Optional directory to write the combined file.
#' @param fname Optional file name used when writing.
#' @return A `data.table` with all rows bound together.
aggregate_csv <- function(directory_path, output_path = NULL, fname = NULL) {


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
