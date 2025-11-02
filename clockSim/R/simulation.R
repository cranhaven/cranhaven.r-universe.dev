# Run simulation

#' Get all clock model Odin generator objects
#' 
#' Refer to package documentation on preconfigured models in this package. 
#' This function is intended for advanced usage only; normally calling 
#' other helper functions will suffice for simulation.
#'
#' @returns Named list of Odin generator R6 objects.
#' @export
#'
#' @examples
#' names(getOdinGen()) # All available models
#' vignette("clock-models", "clockSim")
#' vignette("noisy-LG-model", "clockSim") # Noise-incorporated model using SDE simulation
getOdinGen <- function(){
  return(list(
    discrete_LG = discrete_leloup_goldbeter,
    continuous_LG = deriv_leloup_goldbeter,
    noisy_LG = list(
      gen = discreteNoisy_leloup_goldbeter,
      # This must be updated if noisy model volume changes!!
      count2nM = 1 / (6.02214076E23 * 1E-9 * 1E-15 * 10^3)
    )
  ))
}
