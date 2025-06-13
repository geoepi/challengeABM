#' Plot infection and recovery counts
#'
#' Creates a simple line plot showing numbers infected and recovered over time
#' for a single simulation.
#'
#' @param model_result Result list from a simulation.
#' @return A ggplot object.
plot_herd_curve <- function(model_result) {

    infections_trend <- model_result$final_results %>%
      group_by(time) %>%
      summarize(
        Infected = sum(infection_status == "infected"),
        Recovered = sum(infection_status == "recovered")
      ) %>%
      pivot_longer(cols = c(Infected, Recovered), names_to = "status", values_to = "count")


    ggplot(infections_trend, aes(x = time, y = count, color = status)) +
      geom_line(linewidth = 1) +
      labs(title = "Infection and Recovery",
           x = "Time (hours)",
           y = "Number of Cattle",
           color = "Status") +
      theme_minimal() +
      theme(plot.margin = unit(c(1,0.75,1,0.75),"cm"),
            legend.direction = "horizontal",
            legend.position="bottom",
            strip.text = element_text(size=14, face="bold"),
            strip.background = element_blank(),
            legend.key.size = unit(2,"line"),
            legend.key.width = unit(3,"line"),
            legend.text = element_text(size=16, face="bold"),
            legend.title = element_text(size=18, face="bold"),
            axis.title.x = element_text(size=18, face="bold"),
            axis.title.y = element_text(size=22, face="bold"),
            axis.text.x = element_text(face="bold", size=15, vjust=0.5),
            axis.text.y = element_text(size=18, face="bold"),
            plot.title = element_text(size=22, face="bold")
      )

}
