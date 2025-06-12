#' Update oscillator parameter of an existing ODE network.
#' 
#' Updates the parameters of an existing ODE network.
#' Possible parameters are the oscillator configuration and the starting values.
#' The function overwrites the parameters in the vector and matrices.
#' It will not change the size of the network.
#' It is possible to set a new vector and new matrices, or single parameters in a names vector.
#' 
#' @param odenet [\code{ODEnetwork}]\cr
#'    List of class \code{\link{ODEnetwork}}.
#' @param ParamVec [\code{vector}] of length n\cr
#'   Named vector to overwrite corresponding parameters.
#'   Masses start with "m." followed by a number (e.g.: "m.12").
#'   Dampers start with "d." followed by one or two numbers separated by a dot (e.g.: "d.2", "d.5.6").
#'   Springs start with "k.", like dampers (e.g.: "k.4", "k.3.9")
#'   Distances start with "r.", like dampers (e.g.: "r.7", "r.1.9")
#'   The triangle elements of the dampers, springs, and distances are characterised by increasing numbers.
#'   A name "d.3.5" is correct, in contrast to "d.5.3" which is ignored.
#'   This is done to speed up, because the matrices are symmetric.
#'   State1 and state2 start with "st1." or "st2." respectively, followed by a number (e.g.: "st1.15", "st2.8").
#'   If the vector is set, the following parameters are ignored.
#' @param masses [\code{vector}] of length n\cr
#'   The masses of the mechanical oscillators.
#' @param dampers [\code{matrix}] quadratic of size n\cr
#'   The dampers of the mechanical oscillators on the main diagonal.
#'   Connecting dampers between oscillators on the upper triangle.
#'   (Will be copied automatically to create a symmetric matrix.)
#' @param springs [\code{matrix}] quadratic of size n\cr
#'   The springs are defined in the network like matrix of dampers.
#' @param distances [\code{matrix}] quadratic of size n\cr
#'   Describe spring distance between two masses i < j.
#'   Negative value will be copied automatically to lower triangle.
#' @param state1 [\code{vector}] of length n\cr
#'   Starting values of state 1 (position or angle).
#' @param state2 [\code{vector}] of length n\cr
#'   Starting values of state 2 (velocity or magnitude).
#' @return an extended list of class [\code{\link{ODEnetwork}}].
#' @export
#' @examples
#' masses <- c(1:5)
#' dampers <- diag(11:15)
#' springs <- diag(21:25)
#' odenet <- ODEnetwork(masses, dampers, springs)
#' odenet <- updateOscillators(odenet, masses = c(3:7))
#' odenet <- updateOscillators(odenet, c(k.1.2 = 201, k.3.5 = 202, r.1 = 2))
#' # Warning: Following value is ignored, because it is on the lower triangle
#' odenet <- updateOscillators(odenet, c(d.2.1 = 101))
updateOscillators <- function(odenet, ParamVec=NULL
                              , masses=NULL, dampers=NULL, springs=NULL, distances=NULL
                              , state1=NULL, state2=NULL) {
  UseMethod("updateOscillators")
}

#' @method updateOscillators ODEnetwork
#' @export
updateOscillators.ODEnetwork <- function(odenet, ParamVec=NULL
                                         , masses=NULL, dampers=NULL, springs=NULL, distances=NULL
                                         , state1=NULL, state2=NULL) {
  cLen <- length(odenet$masses)
  if (!is.null(ParamVec)) {
    # checking arguments
    assertNumeric(ParamVec)
    assertVector(ParamVec, strict = TRUE, any.missing = FALSE, min.len = 1L)
    # delete matrices
    masses <- NULL
    dampers <- NULL
    springs <- NULL
    distances <- NULL
    
    # extract parameter settings from the vector to change odenet parameters
    # masses
    cPosSource <- grep("^m\\.\\d+$", names(ParamVec))
    if (length(cPosSource) > 0) {
      masses <- odenet$masses
      cPosTarget <- as.numeric(gsub("\\D", "", names(ParamVec)[cPosSource]))
      masses[cPosTarget] <- ParamVec[cPosSource]
    }
    
    # dampers
    # diagonal elements d.2
    cPosSource <- grep("^d\\.\\d+$", names(ParamVec))
    if (length(cPosSource) > 0) {
      dampers <- odenet$dampers
      cPosTarget <- as.numeric(gsub("\\D", "", names(ParamVec)[cPosSource]))
      diag(dampers)[cPosTarget] <- ParamVec[cPosSource]
    }
    # off diagonal elements d.1.45
    cPosSource <- grep("^d(\\.\\d+){2}$", names(ParamVec))
    if (length(cPosSource) > 0) {
      if (is.null(dampers))
        dampers <- odenet$dampers
      cTemp <- sub("\\D+", "", names(ParamVec)[cPosSource])
      cPosTarget <- as.numeric(sub("\\.\\d+$", "", cTemp))
      cPosTarget2 <- as.numeric(sub("^\\d+\\.", "", cTemp))
      # delete lower triangonal
      cTemp <- cPosTarget < cPosTarget2
      cPosTarget <- cPosTarget[cTemp]
      cPosTarget2 <- cPosTarget2[cTemp]
      for (i in 1:length(cPosTarget)) {
        dampers[cPosTarget[i], cPosTarget2[i]] <- ParamVec[cPosSource[i]]
      }
    }

    # springs
    # diagonal elements k.2
    cPosSource <- grep("^k\\.\\d+$", names(ParamVec))
    if (length(cPosSource) > 0) {
      springs <- odenet$springs
      cPosTarget <- as.numeric(gsub("\\D", "", names(ParamVec)[cPosSource]))
      diag(springs)[cPosTarget] <- ParamVec[cPosSource]
    }
    # off diagonal elements k.1.45
    cPosSource <- grep("^k(\\.\\d+){2}$", names(ParamVec))
    if (length(cPosSource) > 0) {
      if (is.null(springs))
        springs <- odenet$springs
      cTemp <- sub("\\D+", "", names(ParamVec)[cPosSource])
      cPosTarget <- as.numeric(sub("\\.\\d+$", "", cTemp))
      cPosTarget2 <- as.numeric(sub("^\\d+\\.", "", cTemp))
      # delete lower triangonal
      cTemp <- cPosTarget < cPosTarget2
      cPosTarget <- cPosTarget[cTemp]
      cPosTarget2 <- cPosTarget2[cTemp]
      for (i in 1:length(cPosTarget)) {
        springs[cPosTarget[i], cPosTarget2[i]] <- ParamVec[cPosSource[i]]
      }
    }
    
    # distances
    # diagonal elements r.2
    cPosSource <- grep("^r\\.\\d+$", names(ParamVec))
    if (length(cPosSource) > 0) {
      distances <- odenet$distances
      cPosTarget <- as.numeric(gsub("\\D", "", names(ParamVec)[cPosSource]))
      diag(distances)[cPosTarget] <- ParamVec[cPosSource]
    }
    # off diagonal elements r.1.45
    cPosSource <- grep("^r(\\.\\d+){2}$", names(ParamVec))
    if (length(cPosSource) > 0) {
      if (is.null(distances))
        distances <- odenet$distances
      cTemp <- sub("\\D+", "", names(ParamVec)[cPosSource])
      cPosTarget <- as.numeric(sub("\\.\\d+$", "", cTemp))
      cPosTarget2 <- as.numeric(sub("^\\d+\\.", "", cTemp))
      # delete lower triangonal
      cTemp <- cPosTarget < cPosTarget2
      cPosTarget <- cPosTarget[cTemp]
      cPosTarget2 <- cPosTarget2[cTemp]
      for (i in 1:length(cPosTarget)) {
        distances[cPosTarget[i], cPosTarget2[i]] <- ParamVec[cPosSource[i]]
      }
    }
    
    # state1
    cPosSource <- grep("^st1\\.\\d+$", names(ParamVec))
    if (length(cPosSource) > 0) {
      state1 <- odenet$state[, "state1"]
      cPosTarget <- as.numeric(gsub("^st1\\.", "", names(ParamVec)[cPosSource]))
      state1[cPosTarget] <- ParamVec[cPosSource]
    }
    
    # state2
    cPosSource <- grep("^st2\\.\\d+$", names(ParamVec))
    if (length(cPosSource) > 0) {
      state2 <- odenet$state[, "state2"]
      cPosTarget <- as.numeric(gsub("^st2\\.", "", names(ParamVec)[cPosSource]))
      state2[cPosTarget] <- ParamVec[cPosSource]
    }
  } else {
    if (is.null(masses) & is.null(dampers) & is.null(springs) 
        & is.null(distances) & is.null(state1) & is.null(state2))
      stop("Set at least one parameter.")
  }
  
  if (!is.null(masses)) {
    qassert(masses, "N+(0,)")
    assertVector(masses, strict = TRUE, any.missing = FALSE, len = cLen)
    odenet$masses <- masses
  }
  if (!is.null(dampers)) {
    assertMatrix(dampers, mode = "numeric", any.missing = FALSE, nrows = cLen, ncols = cLen)
    # copy upper triangle to lower triangle => symmetric matrix
    dampers[lower.tri(dampers)] <- t(dampers)[lower.tri(dampers)]
    odenet$dampers <- dampers
  }
  if (!is.null(springs)) {
    assertMatrix(springs, any.missing = FALSE, nrows = cLen, ncols = cLen)
    # positive springs
    assertNumeric(springs, lower = 0)
    # copy upper triangle to lower triangle => symmetric matrix
    springs[lower.tri(springs)] <- t(springs)[lower.tri(springs)]
    odenet$springs <- springs
  }
  if (!is.null(distances)) {
    assertMatrix(distances, mode = "numeric", any.missing = FALSE, nrows = cLen, ncols = cLen)
    # copy upper triangle to lower triangle => symmetric matrix
    distances[lower.tri(distances)] <- t(distances)[lower.tri(distances)]
    odenet$distances <- distances
  }
  if (!is.null(state1)) {
    assertNumeric(state1)
    assertVector(state1, strict = TRUE, any.missing = FALSE, len = cLen)
    odenet$state[, "state1"] <- state1
  }
  if (!is.null(state2)) {
    assertNumeric(state2)
    assertVector(state2, strict = TRUE, any.missing = FALSE, len = cLen)
    odenet$state[, "state2"] <- state2
  }
  
  # return
  odenet
}
