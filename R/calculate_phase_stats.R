calculate_phase_stats <- function(melted_data, units = "hours") {

  # duration to days
  if (units == "days") {
    melted_data <- melted_data %>%
      mutate(Duration = Duration / 24)
  }

  # function to calculate mean and confidence intervals
  calc_mean_ci <- function(x) {
    n <- length(x)
    mean_x <- mean(x, na.rm = TRUE)
    se_x <- sd(x, na.rm = TRUE) / sqrt(n)
    ci_lower <- mean_x - qt(0.975, df = n - 1) * se_x
    ci_upper <- mean_x + qt(0.975, df = n - 1) * se_x
    return(list(mean = mean_x, lower_ci = ci_lower, upper_ci = ci_upper))
  }

  # calculate statistics
  stats <- melted_data %>%
    group_by(group, Period) %>%
    summarize(
      mean_duration = calc_mean_ci(Duration)$mean,
      lower_ci = calc_mean_ci(Duration)$lower_ci,
      upper_ci = calc_mean_ci(Duration)$upper_ci,
      median_duration = median(Duration, na.rm = TRUE),
      min_duration = min(Duration, na.rm = TRUE),
      max_duration = max(Duration, na.rm = TRUE),
      .groups = "drop"
    )

  return(stats)
}
