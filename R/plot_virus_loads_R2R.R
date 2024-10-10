plot_virus_loads_R2R <- function(model_result) {

    agent_trajectories <- model_result$final_results

    agent_trajectories$group <- cut(agent_trajectories$id,
                                    breaks = c(-Inf, 2, 6, 10, 14, 18),
                                    labels = c("Donors", "Group 1", "Group 2", "Group 3", "Group 4"),
                                    right = TRUE)

    agent_trajectories <- agent_trajectories %>%
      select(time, group, id, virus_nasal, virus_serum)

    names(agent_trajectories) <- c("Time", "Group", "Animal", "Nasal", "Serum")

    long_data <- agent_trajectories %>%
      pivot_longer(cols = c(Nasal, Serum), names_to = "Type", values_to = "Value") %>%
      mutate(Animal = as.factor(Animal))

    ggplot(long_data, aes(x = Time, y = Value, color = Animal, group = Animal)) +
      geom_line() +
      facet_wrap(~ Group + Type, ncol = 2, scales = "free_y") +
      labs(title = "Nasal and Serum Virus Dynamics",
           x = "Time",
           y = "RNA (log10 copies/ml)") +
      scale_color_viridis_d(option = "turbo", name = "Animal", guide = guide_legend(nrow = 3)) +
      scale_y_continuous(labels = scales::number_format(accuracy = 0.1)) +
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
