% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_hill_demo.R
\name{plot_hill_demo}
\alias{plot_hill_demo}
\title{Demonstrate dose response curves}
\usage{
plot_hill_demo(
  virus_ratio = seq(0, 1, length.out = 100),
  dose_threshold = 0.5,
  dose_efficiency_at_threshold = 0.5,
  dose_max_efficiency = c(0.6, 0.8, 1),
  dose_scaling_factors = c(1, 2, 5)
)
}
\arguments{
\item{virus_ratio}{Vector of ratios to plot.}

\item{dose_threshold}{Midpoint of the curve.}

\item{dose_efficiency_at_threshold}{Efficiency at the threshold.}

\item{dose_max_efficiency}{Vector of maximum efficiencies.}

\item{dose_scaling_factors}{Vector of scaling factors.}
}
\value{
A ggplot object.
}
\description{
Plots Hill-type dose efficiency curves for a range of parameter values.
}
