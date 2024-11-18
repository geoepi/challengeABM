plot_comparison_epicurves <- function(clinical_scenario, preclinical_scenario) {

  clinical_scenario$scenario <- "Clinical"
  preclinical_scenario$scenario <- "Preclinical"

  combined_trends <- bind_rows(clinical_scenario, preclinical_scenario) %>%
    mutate(time = time/24)

  combined_trends_long <- combined_trends %>%
    select(time, median_infected, scenario)

  line_colors = c("Preclinical" = "orange2", "Clinical" = "steelblue")
  ribbon_colors = c("Preclinical" = "orange2", "Clinical" = "steelblue")

  ggplot() +
    geom_ribbon(data = combined_trends, aes(x = time, ymin = lower_infected, ymax = upper_infected, fill = scenario),
                alpha = 0.3, color = NA) +
    geom_line(data = combined_trends_long, aes(x = time, y = median_infected, color = scenario), linewidth = 1) +
    scale_color_manual(values = line_colors) +
    scale_fill_manual(values = ribbon_colors) +
    labs(
      title = " ",
      x = "Time (days)",
      y = "Number of Infected Cattle",
      color = "Scenario",
      fill = "Scenario"
    ) +
    theme_minimal() +
    theme(
      plot.margin = unit(c(3, 0.2, 3, 0.2), "cm"),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.key.size = unit(2, "line"),
      legend.text = element_text(size = 14, face = "bold"),
      legend.title = element_text(size = 16, face = "bold"),
      axis.title = element_text(size = 20, face = "bold"),
      axis.text = element_text(size = 18, face = "bold")
    )
}
