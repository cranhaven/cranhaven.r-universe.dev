#' @title Evaluation with Julia
#' @description Evaluate the hypergeometric function of a matrix argument with
#'   Julia. This is highly faster.
#'
#' @return A function with the same arguments as \code{\link{hypergeomPFQ}}.
#'
#' @importFrom JuliaConnectoR juliaSetupOk juliaCall juliaImport
#' @export
#'
#' @note See \code{\link[JuliaConnectoR]{JuliaConnectoR-package}} for
#'   information about setting up Julia. If you want to directly use Julia,
#'   you can use \href{https://github.com/stla/HypergeoMat.jl}{my package}.
#'
#' @examples library(HypergeoMat)
#' \donttest{if(JuliaConnectoR::juliaSetupOk()){
#'   jhpq <- hypergeomPFQ_julia()
#'   jhpq(30, c(1+1i, 2, 3), c(4, 5), c(0.1, 0.2, 0.3+0.3i))
#'   JuliaConnectoR::stopJulia()
#' }}
hypergeomPFQ_julia <- function(){
  if(!juliaSetupOk()){
    stop("Julia setup is not OK.")
  }
  module <- system.file("julia", "HypergeoMat.jl", package = "HypergeoMat")
  . <- juliaCall("include", module)
  HypergeomPQ <- juliaImport(".HypergeomPQ", all = FALSE)
  function(m, a, b, x, alpha = 2){
    stopifnot(
      isPositiveInteger(m),
      is.null(a) || isNumericOrComplex(a),
      !anyNA(a),
      is.null(b) || isNumericOrComplex(b),
      !anyNA(b),
      isSquareMatrix(x) || isNumericOrComplex(x),
      length(x) != 0L,
      !anyNA(x),
      isNumber(alpha),
      alpha > 0
    )
    if(!is.matrix(x)){
      x <- unname(as.list(x))
    }
    HypergeomPQ$hypergeomPQ(
      as.integer(m), unname(as.list(a)), unname(as.list(b)), x, unname(alpha)
    )
  }
}
