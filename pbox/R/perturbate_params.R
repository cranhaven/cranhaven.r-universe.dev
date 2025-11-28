#' Perturb Parameters
#'
#' This function defines a generic function to perturbate parameter values for each distribution
#' within a copula, using random perturbations to simulate variability or uncertainty.
#'
#' @name perturbate_params
#' @export
#' @importFrom stats rbinom rnorm
#' @param paramMargins A list containing lists of parameter values for each distribution in the copula.
#' @return A list of lists containing perturbed parameter values.
#' @examples
#' paramMargins <- list(list(0.2, 0.3), list(0.4, 0.5))
#' perturbed <- perturbate_params(paramMargins)
#' print(perturbed)
setGeneric("perturbate_params",
           def = function(paramMargins) {
             standardGeneric("perturbate_params")
           })

#' Perturb Parameters Method
#'
#' This method implements the generic `perturbate_params` function specifically
#' for lists of copula distribution parameters. It applies a random perturbation
#' to each parameter based on a normal distribution centered at zero with a
#' standard deviation of 0.05.
#'
#' @param paramMargins A list containing lists of parameter values for each distribution in the copula.
#' @return A list of lists containing perturbed parameter values.
#' @importFrom stats rbinom rnorm
#' @seealso \code{\link{perturbate_params}} for the generic function definition.

setMethod("perturbate_params",
          definition=function(paramMargins) {
  # Define a function to perturb a single parameter value
  perturb_param <- function(orig_param) {
    ind <- rbinom(1, 1, 0.5) == 1
    orig_param[ind] <- orig_param[ind] + rnorm(1, 0, 0.05)
    return(orig_param)
  }

  # Apply perturbation to each parameter in each distribution
  perturbed_params <- lapply(paramMargins, function(dist_params) {
    lapply(dist_params, function(param) {
      perturb_param(param)
    })
  })
  return(perturbed_params)
})

