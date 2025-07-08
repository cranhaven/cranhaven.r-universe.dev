#' Energy Loss Calculation with Beta Scaling
#'
#' This function calculates the energy loss for given tensors. The loss is calculated
#' as the mean of the L2 norms between `x0` and `x` and between `x0` and `xp`, each raised to the power of `beta`,
#' subtracted by half the mean of the L2 norm between `x` and `xp`, also raised to the power of `beta`.
#'
#' @param x0 A tensor representing the target values.
#' @param x A tensor representing the model's stochastic predictions.
#' @param xp A tensor representing another draw of the model's stochastic predictions.
#' @param beta A numeric value for scaling the energy loss.
#' @param verbose A boolean indicating whether to return prediction loss s1 = E(||x0-x||) and variance loss s2 = E(||x-xp||).
#'
#' @return A scalar representing the calculated energy loss.
#'
#' @keywords internal
#'
energylossbeta <- function(x0, x, xp, beta, verbose = FALSE) {
  s1 <- torch_pow(torch_mean(torch_norm(x0 - x, 2, dim = 2)), beta) / 2 + torch_pow(torch_mean(torch_norm(x0 - xp, 2, dim = 2)), beta) / 2
  s2 <- torch_pow(torch_mean(torch_norm(x - xp, 2, dim = 2)), beta)
  if (verbose) {
    return(c((s1 - s2 / 2), s1, s2))
  } else {
    return(s1 - s2 / 2)
  }
}
