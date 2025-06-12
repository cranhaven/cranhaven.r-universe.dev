#' Creates starting State Vector
#' 
#' Creates a vector with the starting state of the given \code{\link{ODEnetwork}}.
#'
#' @param odenet [\code{ODEnetwork}]\cr
#'   List of class \code{\link{ODEnetwork}}.
#' @return a named vector with assigned starting state
#' @export
#' @examples
#' if (interactive()) {
#'   masses <- c(1, 2)
#'   dampers <- diag(c(0.1, 0.5))
#'   dampers[1, 2] <- 0.05
#'   springs <- diag(c(4, 10))
#'   springs[1, 2] <- 6
#'   odenet <- ODEnetwork(masses, dampers, springs)
#'   createState(odenet)
#' }
createState <- function(odenet) {
  UseMethod("createState")
}

#' @method createState ODEnetwork
#' @export
createState.ODEnetwork <- function(odenet) {
  if (is.null(odenet$state)) {
    mState <- matrix(0, ncol = 2, nrow = length(odenet$masses))
  } else {
    # convert from polar to cartesian
    if (odenet$coordtype == "polar") {
      mState <- convertCoordinates(odenet$state)
    } else {
      mState <- odenet$state
    }
  }
  # create vector for state
  strState <- "c("
  for (i in 1:length(odenet$masses)) {
    # Startauslenkung und -geschwindigkeit der Massen
    strState <- paste(strState, "x.", i, " = ", mState[i, 1], ", ", sep = "")
    strState <- paste(strState, "v.", i, " = ", mState[i, 2], sep = "")
    # Komma oder Abschluss
    if (i < length(odenet$masses)) {
      strState <- paste(strState, ",")
    } else {
      strState <- paste(strState, ")", sep = "")
    }	
  }	
  # Rueckgabe
  return(eval(parse(text = strState)))
}
