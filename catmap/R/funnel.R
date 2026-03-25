#' catmap: Funnel Plot
#'
#' The \code{catmap.funnel} creates a funnel plot of the individual Log Odds Ratio
#'  against the standard error of the Log Odds Ratio. The vertical line indicates
#'  the combined Log Odds Ratio. Per the \code{metafor} package, "A pseudo
#'  confidence interval region is drawn around this value with bounds equal
#'  to +/- 1.96 SE".
#'
#' @param catmapobject A catmap object created by \code{\link{catmap}}.
#' @param funnel A boolean. Toggles whether the funnel plot should get saved
#'  to the current working directory.
#'
#' @author Algorithm designed and implemented by Kristin K. Nicodemus.
#'  Code modified and updated by Thom Quinn.
#' @seealso \code{\link{catmap}}, \code{\link{catmap.forest}},
#'  \code{\link{catmap.sense}}, \code{\link{catmap.cumulative}},
#'  \code{\link{catmap.funnel}}
#'
#' @examples
#' data(catmapdata)
#' catmapobject <- catmap(catmapdata, 0.95, TRUE)
#' catmap.funnel(catmapobject, TRUE)
#' @export
catmap.funnel <- function(catmapobject, funnel = FALSE){

  # The original catmap.funnel function plotted SE of Log OR
  #  against exp(Log OR), using $combinedOR as reference line
  # Here, we plot SE of Log OR against Log OR itself, using
  #  $combinedLogOR as reference line
  if(funnel) pdf(file = paste0(catmapobject$dataset, ".funnel.plot.pdf"))
  m <- metafor::funnel(x = catmapobject$logOR, sei = sqrt(catmapobject$comvarlogOR),
                       refline = catmapobject$combinedLogOR, xlab = "Log Odds Ratio")
  if(funnel) graphics.off()
  return(m)
}
