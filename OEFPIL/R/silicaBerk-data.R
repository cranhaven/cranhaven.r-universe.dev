#' @title Nanoindentation measurements data
#'
#' @description Load and depth measured with Berkovich diamond tip on fused silica material.
#'  Data contains only unloading part of indentation curve. The relationship between \emph{Load} (\emph{F}) and \emph{Depth} (\emph{h}) is given by equation
#'  \eqn{F = \alpha (h - h_p)^{m}}, where \eqn{h_{p}} is permanent indentation depth after
#'  removal of the load, \eqn{\alpha} is fitting constant related to the indenter geometry and
#'  power law exponent \emph{m} should be from (1,2) interval.
#'
#' @docType data
#'
#' @usage silicaBerk
#'
#' @format The data frame contains two columns:
#' \describe{
#'   \item{Depth}{nanoindentation depth, in nanometers.}
#'   \item{Load}{load, in milinewtons.}
#' }
#'
#' @keywords datasets
#'
#' @source Czech Metrology Institute, Brno, Czech Republic.
#'
#' @examples
#' attach(silicaBerk)
#' plot(Depth, Load, main = 'Graph of nanoindentation data', xlab = 'Depth (nm)', ylab = 'Load (mN)',
#' col = 'darkgreen', cex = 1)
"silicaBerk"
