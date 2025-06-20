% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generate_state_probability_table.R
\name{generate_state_probability_table}
\alias{generate_state_probability_table}
\title{Generate state transition probabilities}
\usage{
generate_state_probability_table(config)
}
\arguments{
\item{config}{Named list of parameter values.}
}
\value{
Data frame of state probabilities over time.
}
\description{
Uses Weibull distributions specified in `config` to build a table of hourly
probabilities for noninfectious, preclinical and clinical states.
}
