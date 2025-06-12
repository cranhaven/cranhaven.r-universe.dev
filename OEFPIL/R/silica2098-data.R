#' @title Nanoindentation measurements data
#'
#' @description Load and depth measured with Berkovich diamond tip on fused silica material.
#'  Data contains only cutted part of unloading indentation curve from \code{silicaBerk} data (20-98 \%, as recommended in
#'  ISO 14577 standard). The relationship between \emph{Load} (\emph{F}) and \emph{Depth} (\emph{h}) is given by equation
#'  \eqn{F = \alpha (h - h_p)^{m}}, where  \eqn{h_{p}} is permanent indentation depth after
#'  removal of the load, \eqn{\alpha} is fitting constant related to the indenter geometry and
#'  power law exponent \emph{m} should be from (1,2) interval.
#'
#' @docType data
#'
#' @usage silica2098
#'
#' @format The data frame contains two columns:
#' \describe{
#'   \item{Depth}{nanoindentation depth, in nanometers.}
#'   \item{Load}{load, in milinewtons.}
#' }
#'
#' @keywords datasets
#'
#' @references ISO/IEC: \emph{14577-1:2015 Metallic materials – Instrumented indentation test for hardness and materials parameters – Part 1: Test method} (ISO/IEC, Internation Organisation for Standardisation, 2015).
#'
#' @source Czech Metrology Institute, Brno, Czech Republic.
#'
#' @examples
#' attach(silica2098)
#' plot(Depth, Load, main = 'Graph of nanoindentation data', xlab = 'Depth (nm)', ylab = 'Load (mN)',
#' col = 'darkgreen', cex = 1)
"silica2098"
