% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simulate_room_to_room.R
\name{simulate_room_to_room}
\alias{simulate_room_to_room}
\title{Simulate room-to-room transmission experiment}
\usage{
simulate_room_to_room(config_file = NULL, ...)
}
\arguments{
\item{config_file}{YAML configuration file.}

\item{...}{Optional overrides for configuration values.}
}
\value{
List containing agent trajectories and a final results table.
}
\description{
Runs the within-room model for multiple rooms with donor movement between
rooms according to the configuration parameters.
}
