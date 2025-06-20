% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rename_pdfs.R
\name{rename_pdfs}
\alias{rename_pdfs}
\title{Rename PDF files in a folder}
\usage{
rename_pdfs(folder_path)
}
\arguments{
\item{folder_path}{Directory containing PDF files.}
}
\value{
Invisible `NULL`.
}
\description{
Renames PDF files using the first word and year found in the filename,
ensuring unique names.
}
