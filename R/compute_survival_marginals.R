#' Compute marginal survival curves from INLA model
#'
#' Generates posterior survival probabilities for a set of time points using an
#' INLA fitted model.
#'
#' @param model Fitted INLA model object.
#' @param steps Maximum time step to evaluate.
#' @return A data frame containing survival quantiles over time.
compute_survival_marginals <- function(model, steps) {


  # number of cores
  options(mc.cores = 4)

  # time sequence
  times <- seq(0.001, steps, by = 0.1)

  # Marginal of alpha
  alpha.marg <- model$marginals.fixed[["(Intercept)"]]

  #  posterior marginal of survival function
  S.inla <- parallel::mclapply(times, function(t) {
    S.marg <- inla.tmarginal(function(x) { exp(-exp(x) * t) }, alpha.marg)
    S.stats <- inla.zmarginal(S.marg, silent = TRUE)
    return(unlist(S.stats[c("quant0.025", "quant0.25", "quant0.5", "quant0.75", "quant0.975")]))
  })

  S.inla <- as.data.frame(do.call(rbind, S.inla))
  S.inla$time_vect <- times

  return(S.inla)
}
