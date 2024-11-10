plot_diverging_infections <- function(clinical_scenario, preclinical_scenario, peak_time = 168) {

  clinical_scenario$scenario <- "Clinical"
  preclinical_scenario$scenario <- "Preclinical"

  combined_trends <- bind_rows(clinical_scenario, preclinical_scenario) %>%
    mutate(day = time / 24) %>%
    filter(time <= peak_time)

  clinical_data <- combined_trends %>%
    filter(scenario == "Clinical") %>%
    select(day, median_infected) %>%
    mutate(
      scenario = "Clinical",
      portion = "Clinical"
    )

  preclinical_data <- combined_trends %>%
    filter(scenario == "Preclinical") %>%
    mutate(
      additional_cases = median_infectious - median_clinical,
      median_clinical = -median_clinical,
      additional_cases = -additional_cases
    ) %>%
    select(day, median_clinical, additional_cases) %>%
    pivot_longer(cols = c(median_clinical, additional_cases), names_to = "portion", values_to = "median_infectious") %>%
    mutate(
      portion = ifelse(portion == "median_clinical", "Preclinical (symptomatic)", "Preclinical (non-symptomatic)")
    )

  plot_data <- bind_rows(
    clinical_data %>% rename(median_count = median_infected),
    preclinical_data %>% rename(median_count = median_infectious)
  ) %>%
    filter(!is.na(median_count))

  fill_colors <- c(
    "Clinical" = "steelblue",
    "Preclinical (symptomatic)" = "darkorange3",
    "Preclinical (non-symptomatic)" = "orange"
  )

  ggplot(plot_data, aes(x = day, y = median_count, fill = portion)) +
    geom_col() +
    coord_flip() +
    scale_x_reverse(limits = c(max(plot_data$day, na.rm = TRUE), 0), breaks = seq(0, max(plot_data$day, na.rm = TRUE), by = 1)) +
    scale_fill_manual(
      values = fill_colors
    ) +
    scale_y_continuous(labels = abs, name = "Number of Cattle") +  # General label to encompass both metrics
    labs(
      title = " ",
      x = "Time (days)",
      fill = " "
    ) +
    theme_minimal() +
    theme(
      plot.margin = unit(c(1, 0.75, 1, 0.75), "cm"),
      legend.position = c(0.3, 0.8),
      legend.direction = "vertical",
      legend.key.size = unit(2, "line"),
      legend.text = element_text(size = 16, face = "bold"),
      legend.title = element_text(size = 18, face = "bold"),
      axis.title.x = element_text(size = 18, face = "bold"),
      axis.title.y = element_text(size = 22, face = "bold"),
      axis.text.x = element_text(face = "bold", size = 15, vjust = 0.5),
      axis.text.y = element_text(size = 18, face = "bold"),
      plot.title = element_text(size = 22, face = "bold")
    )
}
