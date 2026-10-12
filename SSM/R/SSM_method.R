
#' Summarise SSM class object
#'
#' Printing an SSM will summarise the parameters d, N, and n.  If no model has
#' been fit then this will be noted, otherwise The smoothness of the SSM will be
#' shown.  If sensitivity analysis has been performed, the Sobol indices and
#' Total indices for main effects are displayed and if cross-validation has been
#' performed the Standardised LOO RMSE is also shown.
#'
#' @param object An SSM object
#'
#' @export
setMethod("show", "SSM",
          function(object){
            cat("Smooth supersaturated model in d=",
                object@dimension,
                " variables with N=",
                object@basis_size,
                " terms over design of n=", object@design_size, " points.\n", sep="")
            if (object@fail) {
              cat("No SSM has been fit.\n")
            } else {
              cat("Smoothness measure Psi_2=", object@theta %*% object@K %*% object@theta, "\n", sep="")
            }
            if (length(object@variances)>0) {
              cat("Main effect Sobol' indices:\n")
              print(round(object@main_sobol, digits = 3))
              cat("Total indices:\n")
              print(round(object@total_sobol, digits = 3))
            }
            if (length(object@LOO_RMSE)>0)
              cat("Standardised LOO RMSE =", object@LOO_RMSE,"\n")
          }
)
