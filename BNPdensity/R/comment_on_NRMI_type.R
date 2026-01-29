#' Comment on the NRMI process depending on the value of the parameters
#'
#' @param NRMI_param A named list of the form list("Alpha" = 1, "Kappa" = 0, "Gamma" = 0.4)
#'
#' @return A string containing a comment on the NRMI process
#'
#' @examples
#' BNPdensity:::comment_on_NRMI_type(list("Alpha" = 1, "Kappa" = 0, "Gamma" = 0.4))
#' BNPdensity:::comment_on_NRMI_type(list("Alpha" = 1, "Kappa" = 0.1, "Gamma" = 0.4))
#' BNPdensity:::comment_on_NRMI_type(list("Alpha" = 1, "Kappa" = 0.1, "Gamma" = 0.5))
comment_on_NRMI_type <- function(NRMI_param = list("Alpha" = 1, "Kappa" = 0, "Gamma" = 0.4)) {
  if (NRMI_param$Gamma == 0) {
    return(paste("Dirichlet process,\nwith concentration parameter Alpha =", NRMI_param$Alpha))
  } else {
    if (NRMI_param$Alpha == 1) {
      if (NRMI_param$Kappa == 0) {
        return(paste("Normalized stable process,\nwith stability parameter Gamma =", NRMI_param$Gamma))
      } else if (NRMI_param$Gamma == 0.5) {
        return(paste("Normalized inverse Gaussian process,\nwith parameter Kappa =", NRMI_param$Kappa))
      } else {
        return(paste("Normalized generalised gamma process,\nwith parameter Alpha =", NRMI_param$Alpha, "Kappa =", NRMI_param$Kappa, "Gamma =", NRMI_param$Gamma))
      }
    } else {
      return(paste("Normalized generalised gamma process,\nwith parameter Alpha =", NRMI_param$Alpha, "Kappa =", NRMI_param$Kappa, "Gamma =", NRMI_param$Gamma))
    }
  }
}
