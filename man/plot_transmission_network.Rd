% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_transmission_network.R
\name{plot_transmission_network}
\alias{plot_transmission_network}
\title{Plot transmission network from agent data}
\usage{
plot_transmission_network(
  agent_file_path,
  layout_type = "graphopt",
  seed = NULL
)
}
\arguments{
\item{agent_file_path}{Path to the agent results CSV.}

\item{layout_type}{Either "graphopt" or "tree" for layout algorithm.}

\item{seed}{Optional random seed for layout reproducibility.}
}
\value{
Invisibly returns the igraph object plotted.
}
\description{
Reads an agent CSV file and visualises the directed transmission network.
}
