################################################################################
#
# Wrapper for scar shape constrained smoothing
#
################################################################################

smooth_scar <- function(x, y, shape, Xcov = NULL, ...)
{
  scar_data <- data.matrix(cbind(x, Xcov))
  gfit <- scar::scar(x = scar_data, y = y, shape = shape, ...)
  gx <- gfit$componentfit
  dgx <- mapply(function(x, gx) stats::splinefun(x, gx)(x, deriv = 1), 
    as.data.frame(scar_data), as.data.frame(gx))
  beta0 <- gfit$constant
  return(list(intercept = beta0, gz = gx, dgz = dgx, edf = NA))
}
