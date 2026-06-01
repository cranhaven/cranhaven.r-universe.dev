#' sumR: Approximate series withing a desired error margin
#'
#' \if{html}{\figure{logo.jpeg}{options: width="120"}}
#' 
#' The \pkg{sumR} package provides some `R` functions that allow the
#' user to approximate the sum of series. It also interfaces the low level
#' `C` functions that perform the actual summation, so that other packages
#' can apply them to their own functions written in `C` or `C++`. The
#' [GitHub](https://github.com/GuidoAMoreira/sumR) page provides a short
#' tutorial on how to achieve this.
#'
#' The underlying code is under frequent revision for improvements in speed,
#' memory use and precision. As the need to implement other summation algorithms
#' arise, they will be added.
#'
#' The theoretical foundations for the approximations will be submitted to a
#' peer-reviewed journal shortly.
#'
#' @author
#' Authors:
#' \itemize{
#' \item{Guido A. Moreira}
#' \item{Luiz Max Carvalho}
#' }
#'
#' @docType package
#' @name sumR
#'
#' @useDynLib sumR, .registration = TRUE
#' @keywords internal
"_PACKAGE"

.onUnload <- function (libpath) {
  library.dynam.unload("sumR", libpath)
}
