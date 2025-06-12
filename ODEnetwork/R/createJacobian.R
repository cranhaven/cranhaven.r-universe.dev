#' Create Jacobian matrix for the parameters of the ODE.
#' 
#' Creates the Jacobian matrix for a special set of parameters of the ODE.
#' The first n columns contain the derivatives with respect to d_ii, 
#' followed by the derivatives with respect to k_ii.
#' The last 2*n columns include the derivatives with respect to the states.
#'
#' @param odenet [\code{ODEnetwork}]\cr
#'    List of class \code{\link{ODEnetwork}}.
#' @param ParamVec [\code{vector}] of length n\cr
#'   Named vector to overwrite corresponding parameters (see \code{\link{updateOscillators}}).
#'   Masses start with "m." followed by a number (e.g.: "m.12").
#'   Dampers start with "d." followed by one or two numbers separated by a dot (e.g.: "d.2", "d.5.6").
#'   Springs start with "k.", like dampers (e.g.: "k.4", "k.3.9").
#'   The triangle elements of the dampers and springs are characterised by increasing numbers.
#'   A name "d.3.5" is correct, in contrast to "d.5.3" which is ignored.
#'   This is done to speed up, because the matrices are symmetric.
#'   State1 and state2 start with "st1." or "st2." respectively, followed by a number (e.g.: "st1.15", "st2.8").
#'   If the vector is set, the following parameters are ignored.
#' @return the Jacobian matrix of size 2n*4n.
#' @export
#' @examples
#' masses <- 4:6
#' dampers <- diag(1:3)
#' springs <- diag(7:9)
#' odenet <- ODEnetwork(masses, dampers, springs)
#' position <- rep(10, 3)
#' velocity <- rep(0, 3)
#' odenet <- setState(odenet, position, velocity)
#' jac <- createJacobian(odenet)
createJacobian <- function(odenet, ParamVec=NA) {
  UseMethod("createJacobian")
}

#' @method createJacobian ODEnetwork
#' @export
createJacobian.ODEnetwork <- function(odenet, ParamVec=NA) {
  if (sum(!is.na(ParamVec)) > 0) {
    odenet <- ODEnetwork::updateOscillators(odenet, ParamVec)
  }
  
  # get parameters from the odenet
  cN <- length(odenet$masses)
  # derive My'' + Dy' + Ky = 0
  # convert to y'' = -M^-1*D*y' - M^-1*K*y
  mMinv <- diag(1/odenet$masses, cN)
  mD <- odenet$dampers
  diag(mD) <- -rowSums(mD)
  mD <- -mD
  mK <- odenet$springs
  diag(mK) <- -rowSums(mK)
  mK <- -mK
  # switch to ODEs of first order with x' = C * x
  # C = rbind( (0, I), (-M^-1*K,-M^-1*D))
  mC <- rbind(cbind(diag(0, cN), diag(1, cN)), cbind(-mMinv%*%mK, -mMinv%*%mD))
  # derivative with respect to d_ii
  mDerivDii <- rbind(diag(0, cN), -mMinv %*% diag(odenet$state[, "state2"], cN))
  # derivative with respect to d_ii
  mDerivKii <- rbind(diag(0, cN), -mMinv %*% diag(odenet$state[, "state1"], cN))
  
  # jacobian
  return(cbind(mDerivDii, mDerivKii, mC))
}
