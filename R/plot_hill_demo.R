#' Demonstrate dose response curves
#'
#' Plots Hill-type dose efficiency curves for a range of parameter values.
#'
#' @param virus_ratio Vector of ratios to plot.
#' @param dose_threshold Midpoint of the curve.
#' @param dose_efficiency_at_threshold Efficiency at the threshold.
#' @param dose_max_efficiency Vector of maximum efficiencies.
#' @param dose_scaling_factors Vector of scaling factors.
#' @return A ggplot object.
plot_hill_demo <- function(virus_ratio = seq(0, 1, length.out = 100),
                           dose_threshold = 0.5,
                           dose_efficiency_at_threshold = 0.5,
                           dose_max_efficiency = c(0.6, 0.8, 1.0),
                           dose_scaling_factors = c(1, 2, 5)) {

  plot_data <- data.frame()

  for (max_efficiency in dose_max_efficiency) {
    for (dose_scaling_factor in dose_scaling_factors) {

      threshold_adj <- dose_threshold + (1 / dose_scaling_factor) * log((max_efficiency / dose_efficiency_at_threshold) - 1)

      # dose_eff using the sigmoid function (Hill equation)
      dose_eff <- max_efficiency / (1 + exp(-dose_scaling_factor * (virus_ratio - threshold_adj)))

      df <- data.frame(virus_ratio = virus_ratio,
                       dose_eff = dose_eff,
                       max_efficiency = max_efficiency,
                       dose_scaling_factor = dose_scaling_factor)

      plot_data <- rbind(plot_data, df)
    }
  }

  plot_data$max_efficiency <- as.factor(plot_data$max_efficiency)
  plot_data$dose_scaling_factor <- as.factor(plot_data$dose_scaling_factor)

  plot_data$curve_label <- paste('Max Eff =', plot_data$max_efficiency, ', Scaling Factor =', plot_data$dose_scaling_factor)

  ggplot(plot_data, aes(x = virus_ratio, y = dose_eff, color = curve_label)) +
    geom_line(linewidth=1) +
    ylim(0, 1) +
    labs(title = 'Dose Efficiency (Hill Equation)',
         x = 'Virus Ratio',
         y = 'Dose Efficiency',
         color = 'Parameters') +
    theme_minimal() +
    guides(color = guide_legend(ncol = 2)) +
    theme(
      plot.margin = unit(c(1, 0.75, 1, 0.75), "cm"),
      legend.direction = "vertical",
      legend.position = "bottom",
      strip.text = element_text(size = 14, face = "bold"),
      strip.background = element_blank(),
      legend.key.size = unit(2, "line"),
      legend.key.width = unit(3, "line"),
      legend.text = element_text(size = 10, face = "bold"),
      legend.title = element_text(size = 18, face = "bold"),
      axis.title.x = element_text(size = 18, face = "bold"),
      axis.title.y = element_text(size = 22, face = "bold"),
      axis.text.x = element_text(face = "bold", size = 15, vjust = 0.5),
      axis.text.y = element_text(size = 18, face = "bold"),
      plot.title = element_text(size = 22, face = "bold"))
}
