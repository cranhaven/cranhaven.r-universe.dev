#' set_starting_values
#'
#' set the starting values of an OpenMx model. This is just an interface to
#' omxSetParameters.
#' @param mx_model model of class mxModel
#' @param values vector with labeled parameter values
#' @return mxModel with changed parameter values
#' @export
#' @examples
#' library(mxsem)
#'
#' model <- '
#'   # latent variable definitions
#'      ind60 =~ x1 + x2 + x3
#'      dem60 =~ y1 + a1*y2 + b*y3 + c1*y4
#'      dem65 =~ y5 + a2*y6 + b*y7 + c2*y8
#'
#'   # regressions
#'     dem60 ~ ind60
#'     dem65 ~ ind60 + dem60
#'
#'   # residual correlations
#'     y1 ~~ y5
#'     y2 ~~ y4 + y6
#'     y3 ~~ y7
#'     y4 ~~ y8
#'     y6 ~~ y8
#' '
#'
#' fit <- mxsem(model = model,
#'             data  = OpenMx::Bollen) |>
#'   set_starting_values(values = c("a1" = .4, "c1" = .6)) |>
#'   mxTryHard()
set_starting_values <- function(mx_model, values){

  if(is.null(names(values)))
    stop("values must be a vector with labels. See ?mxsem::set_starting_values")

  model_parameters <- OpenMx::omxGetParameters(mx_model)

  for(i in seq_along(values)){
    if(names(values)[i] %in% names(model_parameters)){
      model_parameters[names(values)[i]] <- values[i]
    }else{
      stop("The following parameter was not found in the model:", names(values)[i])
    }
  }

  return(OpenMx::omxSetParameters(model = mx_model,
                                  labels = names(model_parameters),
                                  values = model_parameters))

}
