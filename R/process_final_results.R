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
