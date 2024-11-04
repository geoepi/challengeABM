plot_herd_curve_mult <- function(infections_trend) {

  infections_trend_long <- infections_trend %>%
    pivot_longer(
      cols = c(median_infected, lower_infected, upper_infected, median_recovered, lower_recovered, upper_recovered),
      names_to = c(".value", "status"),
      names_pattern = "(.*)_(infected|recovered)"
    )

  line_colors <- c("infected" = "red4", "recovered" = "green")
  ribbon_colors <- c("infected" = "red3", "recovered" = "lightgreen")

  ggplot(infections_trend_long, aes(x = time, y = median, color = status)) +
    geom_line(linewidth = 1) +
    geom_ribbon(aes(ymin = lower, ymax = upper, fill = status), alpha = 0.3, linewidth = 0) +
    scale_color_manual(values = line_colors) +
    scale_fill_manual(values = ribbon_colors) +
    labs(
      title = " ",
      x = "Time (hours)",
      y = "Number of Cattle",
      color = "Status",
      fill = "Status"
    ) +
    theme_minimal() +
    theme(
      plot.margin = unit(c(1, 0.75, 1, 0.75), "cm"),
      legend.direction = "horizontal",
      legend.position = "bottom",
      strip.text = element_text(size = 14, face = "bold"),
      strip.background = element_blank(),
      legend.key.size = unit(2, "line"),
      legend.key.width = unit(3, "line"),
      legend.text = element_text(size = 16, face = "bold"),
      legend.title = element_text(size = 18, face = "bold"),
      axis.title.x = element_text(size = 18, face = "bold"),
      axis.title.y = element_text(size = 22, face = "bold"),
      axis.text.x = element_text(face = "bold", size = 15, vjust = 0.5),
      axis.text.y = element_text(size = 18, face = "bold"),
      plot.title = element_text(size = 22, face = "bold")
    )
}
