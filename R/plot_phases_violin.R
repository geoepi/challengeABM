#' Visualise phase durations by group
#'
#' Creates violin plots of latent, subclinical and incubation periods for each
#' experimental group.
#'
#' @param data Data frame from `calculate_phase_durations()`.
#' @param y_max Maximum y axis in days.
#' @param colors Fill colours for the phases.
#' @return A ggplot object.
plot_phases_violin <- function(data, y_max = 9, colors = c('steelblue', 'darkorange', 'darkgreen')) {

  data$group = ifelse(data$group == "Donor", "Donors", data$group)
  data$group = ifelse(data$group == "Room 2", "Group 1", data$group)
  data$group = ifelse(data$group == "Room 3", "Group 2", data$group)
  data$group = ifelse(data$group == "Room 4", "Group 3", data$group)
  data$group = ifelse(data$group == "Room 5", "Group 4", data$group)

  ggplot(data, aes(x = group, y = Duration / 24, fill = Period)) +
    geom_violin(position = position_dodge(width = 0.85), alpha=0.3, color = "gray20", size = 0.15) +
    stat_summary(
      fun.data = function(y) {
        data.frame(ymin = quantile(y, 0.25), y = median(y), ymax = quantile(y, 0.75))
      },
      geom = "errorbar", width = 0, color = "gray20", position = position_dodge(width = 0.85)
    ) +
    stat_summary(
      fun = median, geom = "point", shape = 16, size = 3, color = "gray20", position = position_dodge(width = 0.85)
    ) +
    scale_fill_manual(values = colors) +
    labs(
      title = " ",
      x = " ",
      y = "Duration (days)",
      fill = "Phase"
    ) +
    scale_y_continuous(breaks = seq(0, y_max, 2), labels =  seq(0, y_max, 2), limits = c(0, y_max)) +
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
      axis.text.x = element_text(face = "bold", size = 20, vjust = 0.5),
      axis.text.y = element_text(size = 20, face = "bold"),
      plot.title = element_text(size = 22, face = "bold")
    )
}
