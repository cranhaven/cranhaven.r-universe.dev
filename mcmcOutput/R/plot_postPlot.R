


postPlot <- function(object, params, layout=c(3,3),
    center = c("mean", "median", "mode"), CRImass=0.95,
    compVal = NULL, ROPE = NULL, HDItextPlace = 0.7,
    showCurve = FALSE, shadeHDI = NULL, ...) {

  dots <- list(...)
  if(is.null(dots$main))
    dots$main <- deparse(substitute(object))
  center <- match.arg(center)

  object <- mcmcOutput(object)
  plot.mcmcOutput(object, params=params, layout=layout,
    center = center, CRImass=CRImass,
    compVal = compVal, ROPE = ROPE, HDItextPlace = HDItextPlace,
    showCurve = showCurve, shadeHDI = shadeHDI, dots)
}
