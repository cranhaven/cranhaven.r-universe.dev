#' @importFrom JuliaConnectoR stopJulia
#' @noRd
.onUnLoad <- function(libpath){
  stopJulia()
}
