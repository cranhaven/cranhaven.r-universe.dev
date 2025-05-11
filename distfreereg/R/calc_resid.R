calc_resid <-
function(Y, fitted_values){
  # Calculate raw residuals.
  stopifnot(is.numeric(Y), is.numeric(fitted_values),
            identical(length(Y), length(fitted_values)))
  return(as.vector(Y - fitted_values))
}
