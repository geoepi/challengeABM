calculate_phase_stats <- function(melted_data, units = "hours") {

  data_copy <- copy(melted_data)

  if (units == "days") {
    data_copy[, Duration := Duration / 24]
  }

  # calculate mean and confidence interval
  calc_mean_ci <- function(x) {
    n <- length(x)
    mean_x <- mean(x, na.rm = TRUE)
    se_x <- sd(x, na.rm = TRUE) / sqrt(n)
    ci_lower <- mean_x - qt(0.975, df = n - 1) * se_x
    ci_upper <- mean_x + qt(0.975, df = n - 1) * se_x
    return(list(mean = mean_x, lower_ci = ci_lower, upper_ci = ci_upper))
  }

  # statistics by group
  stats <- data_copy[, {
    ci <- calc_mean_ci(Duration)
    median_duration <- median(Duration, na.rm = TRUE)
    min_duration <- min(Duration, na.rm = TRUE)
    max_duration <- max(Duration, na.rm = TRUE)
    .(mean_duration = ci$mean, lower_ci = ci$lower_ci, upper_ci = ci$upper_ci,
      median_duration = median_duration, min_duration = min_duration, max_duration = max_duration)
  }, by = .(group, Period)]

  return(stats)
}
