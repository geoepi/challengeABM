#' Rename PDF files in a folder
#'
#' Renames PDF files using the first word and year found in the filename,
#' ensuring unique names.
#'
#' @param folder_path Directory containing PDF files.
#' @return Invisible `NULL`.
rename_pdfs <- function(folder_path) {

  # use functions from fs and stringr via explicit namespaces

  files <- fs::dir_ls(folder_path, regexp = "\\.pdf$")

  used_names <- character()

  for (file in files) {
    filename <- fs::path_file(file)

    # first word and 4-digit year
    first_word <- stringr::str_extract(filename, "^[a-zA-Z0-9]+")
    year <- stringr::str_extract(filename, "(19|20)\\d{2}")

    if (!is.na(first_word) && !is.na(year)) {

      base_name <- paste0(first_word, "_", year)
      new_filename <- paste0(base_name, ".pdf")
      counter <- 1

      # duplicates
      while (new_filename %in% used_names || fs::file_exists(fs::path(folder_path, new_filename))) {
        new_filename <- paste0(base_name, "_", counter, ".pdf")
        counter <- counter + 1
      }

      used_names <- c(used_names, new_filename)

      new_file_path <- fs::path(folder_path, new_filename)
      fs::file_move(file, new_file_path)
      message("Renamed: ", filename, " -> ", new_filename)
    } else {
      message("Skipped: ", filename, " (missing first word or year)")
    }
  }
}
