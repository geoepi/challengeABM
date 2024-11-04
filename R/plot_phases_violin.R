plot_phases_violin <- function(data, colors = c('steelblue', 'darkorange', 'darkgreen')) {
  ggplot(data, aes(x = group, y = Duration / 24, fill = Period)) +
    geom_violin(position = position_dodge(width = 0.75), alpha=0.3, color = "gray20") +
    stat_summary(
      fun.data = function(y) {
        data.frame(ymin = quantile(y, 0.25), y = median(y), ymax = quantile(y, 0.75))
      },
      geom = "errorbar", width = 0, color = "gray20", position = position_dodge(width = 0.75)
    ) +
    stat_summary(
      fun = median, geom = "point", shape = 16, size = 3, color = "gray20", position = position_dodge(width = 0.75)
    ) +
    scale_fill_manual(values = colors) +
    labs(
      title = " ",
      x = " ",
      y = "Duration (days)",
      fill = "Phase"
    ) +
    ylim(0, 8) +
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
