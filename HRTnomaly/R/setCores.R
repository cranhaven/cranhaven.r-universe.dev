#' @name setCores
#' @aliases setCores
#' @title Set the number of CPU cores for HPC
#' 
#' @description
#' The function set the number of CPU cores for parallel computation by the use of OpenMP library (\url{https://www.openmp.org/}). If the package was not complied with the library OpenMP (>= 3.0), this function is disabled.
#' 
#' @usage setCores(n)
#' 
#' @param n an integer value denoting the number of CPU cores to use; if it exceeds the total number of cores, all of them will be used. If missing, the number of CPU cores in use will be displayed.
#' 
#' @details
#' When the package is loaded, only one CPU core is used.
#' 
#' @return The total number of CPU cores in use will be returned and a message will be displayed. If the package was not complied with the library OpenMP (>= 3.0), the value one will be returned.
#' 
#' @references SunTM ONE Studio 8 (2003) \emph{OpenMP API User's Guide}. Sun Microsystems Inc., Santa Clara, U.S.A.
#' @author Luca Sartore \email{drwolf85@gmail.com}
#' @examples \donttest{
#' #Display the number of CPU cores in use
#' setCores()
#'
#' #Set 2 CPU cores for parallel computation
#' setCores(2)
#'
#' #Set 1 CPU core for serial computation
#' setCores(1)
#' }
#' @keywords programming
NULL
setCores <-
function(n) {

  # Set number of CPU cores which will be used by the package
  #
  #     n number of CPU cores

  if (!missing(n)) {
    if (is.numeric(n)) {
      n <- as.integer(ceiling(n))
      n <- .C('setNThreads', n = as.integer(n), PACKAGE = "HRTnomaly")$n
    }
  }
  n <- 0L
  crTot <- 0L
  n <- .C('getNThreads', n = as.integer(n), PACKAGE = "HRTnomaly")$n
  if (n == 1L) {
    if (.Call("isOmp", PACKAGE = "HRTnomaly")) packageStartupMessage("Parallel computation will not perform. CPU cores in use: 1.")
  }
  else if (n > 1L){
    crTot <- .C('getNCores', n = as.integer(crTot), PACKAGE = "HRTnomaly")$n
    packageStartupMessage("Parallel computation will perform.")
    packageStartupMessage("  Total CPU cores available: ", crTot, ".", sep = "")
    packageStartupMessage("  CPU cores in use: ", n, ".", sep = "")
  }
  invisible(n)
}
