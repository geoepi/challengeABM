% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_agent_meta.R
\name{get_agent_meta}
\alias{get_agent_meta}
\title{Retrieve agent metadata from result files}
\usage{
get_agent_meta(data_path, myVars = c("id", "trial", "dose"))
}
\arguments{
\item{data_path}{Directory containing agent result CSVs.}

\item{myVars}{Character vector of columns to keep.}
}
\value{
A combined data frame of selected metadata.
}
\description{
Reads selected columns from all agent result files in a directory and
combines them into one table.
}
