#' Postprocess final agent results
#'
#' Adjusts infectious start times so that animals with no clinical signs have
#' `NA` for `infectious_t`.
#'
#' @param agents Data frame of agent trajectories.
#' @return Updated data frame.
process_final_results <- function(agents) {
  for (agent_id in 1:nrow(agents)) {
    if (!is.na(agents$infectious_t[agent_id]) && !is.na(agents$score_t[agent_id])) {
      if (agents$score_t[agent_id] == 0) {
        agents$infectious_t[agent_id] <- NA
      } else if (agents$infectious_t[agent_id] > agents$score_t[agent_id]) {
        agents$infectious_t[agent_id] <- agents$score_t[agent_id]
      }
    }
  }

  return(agents)
}
