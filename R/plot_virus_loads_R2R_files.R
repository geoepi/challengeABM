#' Plot virus loads from result files
#'
#' Reads multiple room-to-room result files and plots all animal virus curves
#' with optional scaling and median calculation.
#'
#' @param data_path Directory containing CSV outputs.
#' @param sample_files_proportion Proportion of files to sample.
#' @param sample_trials Optional number of trials to sample.
#' @param scale_serum Optional maximum for serum axis.
#' @param scale_nasal Optional maximum for nasal axis.
#' @param calc_median Logical, add median trend lines.
#' @param line_alpha Alpha transparency for individual lines.
#' @return A ggplot object.
plot_virus_loads_R2R_files <- function(data_path, sample_files_proportion = 1, sample_trials = NULL,
                                       scale_serum = NULL, scale_nasal = NULL,
                                       calc_median = FALSE, line_alpha = 0.3) {
  # use functions from forcats via explicit namespaces

  file_list <- list.files(
    path = data_path,
    pattern = "final_results_iteration_.*\\.csv",
    full.names = TRUE
  )

  n_files <- ceiling(length(file_list) * sample_files_proportion)

  sampled_files <- sample(file_list, n_files)

  combined_data <- vroom(sampled_files, col_types = cols())

  if (!is.null(sample_trials)) {
    unique_trials <- combined_data %>%
      distinct(trial) %>%
      pull(trial)

    if (length(unique_trials) < sample_trials) {
      warning("sample_trials exceeds the number of unique trials available. Including all trials.")
      sampled_trials <- unique_trials
    } else {
      sampled_trials <- sample(unique_trials, sample_trials)
    }

    combined_data <- combined_data %>%
      filter(trial %in% sampled_trials)
  }

  combined_data <- combined_data %>%
    mutate(
      group = cut(
        id,
        breaks = c(-Inf, 2, 6, 10, 14, 18),
        labels = c("Donors", "Group 1", "Group 2", "Group 3", "Group 4"),
        right = TRUE
      )
    )

  combined_data <- combined_data %>%
    mutate(Animal = paste0("Trial_", trial, "_ID_", id))

  agent_trajectories <- combined_data %>%
    select(time, group, Animal, virus_nasal, virus_serum) %>%
    rename(Time = time, Group = group, Nasal = virus_nasal, Serum = virus_serum)

  if (!is.null(scale_nasal)) {
    min_nasal <- min(agent_trajectories$Nasal, na.rm = TRUE)
    max_nasal <- scale_nasal
    agent_trajectories <- agent_trajectories %>%
      mutate(Nasal = scales::rescale(Nasal, to = c(min_nasal, max_nasal)))
  }

  if (!is.null(scale_serum)) {
    min_serum <- min(agent_trajectories$Serum, na.rm = TRUE)
    max_serum <- scale_serum
    agent_trajectories <- agent_trajectories %>%
      mutate(Serum = scales::rescale(Serum, to = c(min_serum, max_serum)))
  }

  long_data <- agent_trajectories %>%
    pivot_longer(cols = c(Nasal, Serum), names_to = "Type", values_to = "Value") %>%
    mutate(Animal = as.factor(Animal))

  long_data <- long_data %>%
    mutate(Animal = forcats::fct_shuffle(Animal)) %>%
    arrange(Animal)

  # calculate medians if calc_median is TRUE
  if (calc_median) {
    median_data <- long_data %>%
      group_by(Group, Type, Time) %>%
      summarize(Median = median(Value, na.rm = TRUE), .groups = "drop")
  }

  max_time <- max(long_data$Time / 24, na.rm = TRUE)

  vline_data <- data.frame(
    Group = c("Donors", "Group 1", "Group 2", "Group 3", "Group 4"),
    xintercept = c(6, 16, 10, 11, 12),
    xmin = c(6, 16, 10, 11, 12),
    xmax = max_time
  )

  # background shading for exposure period
  background_shading <- data.frame(
    Group = c("Donors", "Group 1", "Group 2", "Group 3", "Group 4"),
    xmin = c(0, 1, 2, 3, 4),
    xmax = c(1, 2, 3, 4, 5)
  )

  plot <- ggplot(long_data, aes(x = Time / 24, y = Value, color = Animal, group = Animal)) +
    geom_rect(data = vline_data, aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
              fill = "gray50", alpha = 0.2, inherit.aes = FALSE) +
    geom_rect(data = background_shading, aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
              fill = "tan3", alpha = 0.5, inherit.aes = FALSE) +
    geom_line(alpha = line_alpha, linewidth = 0.2) +
    geom_vline(data = vline_data, aes(xintercept = xintercept), linetype = "dashed", color = "gray20") +
    facet_grid(rows = vars(Group), cols = vars(Type), scales = "free_y") +
    labs(title = "Nasal and Serum Virus Dynamics",
         x = "Experiment Day",
         y = "RNA (log10 copies/ml)") +
    scale_color_viridis_d(option = "turbo", name = "Animal", guide = guide_legend(nrow = 3)) +
    scale_x_continuous(breaks = seq(0, 14, 2), limits = c(0, 15)) +
    scale_y_continuous(labels = scales::number_format(accuracy = 0.1), limits = c(0, 10)) +
    theme_minimal() +
    theme(
      plot.margin = unit(c(1, 0.75, 1, 0.75), "cm"),
      legend.direction = "horizontal",
      legend.position = "none",
      strip.text = element_blank(),
      strip.background = element_blank(),
      legend.key.size = unit(2, "line"),
      legend.key.width = unit(3, "line"),
      legend.text = element_text(size = 16, face = "bold"),
      legend.title = element_text(size = 18, face = "bold"),
      axis.title.x = element_text(size = 18, face = "bold"),
      axis.title.y = element_text(size = 22, face = "bold"),
      axis.text.x = element_text(face = "bold", size = 18, vjust = 0.5),
      axis.text.y = element_text(size = 16, face = "bold"),
      plot.title = element_text(size = 22, face = "bold"),
      panel.spacing = unit(1.5, "lines")
    )

  # median lines if calc_median is TRUE
  if (calc_median) {
    plot <- plot +
      geom_line(data = median_data, aes(x = Time / 24, y = Median, group = Type), color = "black", linetype = "dashed", inherit.aes = FALSE)
  }

  return(plot)
}
