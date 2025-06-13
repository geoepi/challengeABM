#' Asymmetric sigmoid helper
#'
#' Computes a sigmoidal curve used for within herd state transitions.
#'
#' @param t Time value.
#' @param onset Onset in days.
#' @param duration Duration in days.
#' @return Probability value between 0 and 1.
aesigmoid <- function(t, onset, duration) {
  steep <- 4 / (duration * 24)
  1 / (1 + exp(-steep * (t - onset * 24)))
}
