##
#' Threshold estimation
#'
#' Function to implement the soft-, MCP, SCAD thresholding rule in the ADMM method.
#'
#'
#'
#' @param z a vector where the function is to be evaluated.
#' @param lambda a number representing a tuning parameter.
#' @param tau the penalty parameter in the ADMM method.
#' @param a the tuning parameter of the MCP/SCAD penalty (see details).
#' Default is 3 for MCP and 3.7 for SCAD.
#' @param penalty The penalty to be applied to the model. Either "lasso" (the default), "SCAD",
#' or "MCP".
#'
#' @export thresh_est
#'
#' @return A vector containing the threshlding values at z.
#'
#' @references {'Pairwise Fusion Approach Incorporating Prior Constraint Information' by Yaguang Li}
#'

thresh_est <- function(z, lambda, tau, a = 3, penalty = c("MCP", "SCAD", "lasso")) {

  ### function to implement the soft-, MCP, SCAD thresholding rule Input
  ### variables: z: argument type: thresholding rule 1 = (Adaptive) LASSO
  ### (default) 2 = MCP 3 = SCAD lambda: thresholding level a: default
  ### choice for SCAD penalty

  penalty <- match.arg(penalty)

  if (penalty == "lasso") {
    return(sign(z) * (abs(z) >= lambda/tau) * (abs(z) - lambda/tau))
  }

  if (penalty == "MCP") {
    # a = 2.7 + 1/tau
    return(sign(z) * (abs(z) >= lambda/tau) * (abs(z) - lambda/tau)/(1 -
                                                                       1/(a * tau)) * (abs(z) <= a * lambda) + z * (abs(z) > a * lambda))
  }

  if (penalty == "SCAD") {
    # a = 3.7 + 1/tau
    return(sign(z) * (abs(z) >= lambda/tau) * (abs(z) - lambda/tau) *
             (abs(z) <= lambda + lambda/tau) + sign(z) * (abs(z) >= a *
                                                            lambda/(tau * (a - 1))) * (abs(z) - a * lambda/(tau * (a -
                                                                                                                     1)))/(1 - 1/((a - 1) * tau)) * (lambda + lambda/tau < abs(z)) *
             (abs(z) <= a * lambda) + z * (abs(z) > a * lambda))
  }

}

