plot_animal_status <- function(model_result) {
  status_data <- model_result$final_results %>%
    group_by(id) %>%
    summarize(
      infection_start = ifelse(any(infection_status == "infected"), min(time[infection_status == "infected"], na.rm = TRUE), NA),
      infectious_start = ifelse(any(!is.na(infectious_t)), min(time[!is.na(infectious_t)], na.rm = TRUE), NA),
      symptoms_start = ifelse(any(score > 0), min(time[score > 0], na.rm = TRUE), NA),
      total_time = max(time)
    ) %>%
    mutate(
      non_infected_start = 0,
      non_infected_end = ifelse(is.na(infection_start), total_time, infection_start),
      infected_non_infectious_start = infection_start,
      infected_non_infectious_end = ifelse(!is.na(infectious_start), infectious_start, total_time),
      infectious_start_time = infectious_start,
      infectious_end = ifelse(!is.na(symptoms_start), symptoms_start, total_time),
      symptomatic_start = symptoms_start,
      symptomatic_end = ifelse(!is.na(symptoms_start), total_time, NA)
    ) %>%
    pivot_longer(
      cols = starts_with(c("non_infected", "infected_non_infectious", "infectious", "symptomatic")),
      names_to = c("status", ".value"),
      names_pattern = "(.*)_(start|end)"
    ) %>%
    filter(!is.na(start) & !is.na(end)) %>%
    mutate(duration = end - start) %>%
    filter(duration > 0) %>%
    mutate(status = factor(status, levels = c("non_infected", "infected_non_infectious",
                                              "infectious", "symptomatic")))

  ggplot(status_data, aes(xmin = start, xmax = end, ymin =
                            id - 0.4, ymax = id + 0.4, fill = status)) +
    geom_rect() +
    scale_fill_manual(
      values = c(
        "non_infected" = "lightblue",
        "infected_non_infectious" = "orange",
        "infectious" = "red",
        "symptomatic" = "darkred"
      ),
      labels = c(
        "Non-Infected",
        "Infected (Non-Infectious)",
        "Subclinical Infectious",
        "Clinical Infectious"
      ),
      guide = guide_legend(nrow = 2)
    ) +
    labs(
      x = "Simulation Time (Hours)",
      y = "Animal ID",
      fill = "Animal Status",
      title = "Simulated Animal Status"
    ) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(breaks = unique(status_data$id)) +
    theme_minimal() +
    theme(legend.position = "bottom",
          panel.grid.major = element_blank(), panel.grid.minor = element_blank())  # Remove grid lines for clarity
}
