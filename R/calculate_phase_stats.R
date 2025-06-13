#' Summarise infection phase durations
#'
#' Takes the long format data from `calculate_phase_durations()` and produces
#' summary statistics for each group and phase.
#'
#' @param melted_data Output of `calculate_phase_durations()`.
#' @param units Unit of time to report, "hours" or "days".
#' @return A data frame of summary statistics.
calculate_phase_stats <- function(melted_data, units = "hours") {

  if (units == "days") {
    melted_data <- melted_data %>%
      mutate(Duration = Duration / 24)
  }

  calc_mean_ci <- function(x) {
    n <- length(x)
    mean_x <- mean(x, na.rm = TRUE)
    se_x <- sd(x, na.rm = TRUE) / sqrt(n)
    ci_lower <- mean_x - qt(0.975, df = n - 1) * se_x
    ci_upper <- mean_x + qt(0.975, df = n - 1) * se_x
    return(list(mean = mean_x, lower_ci = ci_lower, upper_ci = ci_upper))
  }

  stats <- melted_data %>%
    group_by(group, Period) %>%
    summarize(
      mean_duration = calc_mean_ci(Duration)$mean,
      lower_ci = calc_mean_ci(Duration)$lower_ci,
      upper_ci = calc_mean_ci(Duration)$upper_ci,
      median_duration = median(Duration, na.rm = TRUE),
      min_duration = min(Duration, na.rm = TRUE),
      max_duration = max(Duration, na.rm = TRUE),
      quantile_0.025 = quantile(Duration, 0.025, na.rm = TRUE),
      quantile_0.05 = quantile(Duration, 0.05, na.rm = TRUE),
      quantile_0.25 = quantile(Duration, 0.25, na.rm = TRUE),
      quantile_0.75 = quantile(Duration, 0.75, na.rm = TRUE),
      quantile_0.95 = quantile(Duration, 0.95, na.rm = TRUE),
      quantile_0.975 = quantile(Duration, 0.975, na.rm = TRUE),
      .groups = "drop"
    ) %>%

    mutate(across(where(is.numeric), ~ round(., 2))) %>%

    mutate(
      Duration = paste0(
        formatC(median_duration, format = "f", digits = 2),
        " (",
        formatC(quantile_0.25, format = "f", digits = 2), ", ",
        formatC(quantile_0.75, format = "f", digits = 2),
        ")"
      )
    ) %>%
    select(group, Period, Duration, everything())

  return(stats)
}
