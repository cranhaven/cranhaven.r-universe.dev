eval.FEM.fd <- function(pts, fdobj, nderivs=rep(0,2)) {
  # EVAL_FEM_FD evaluates the FEM fd object at points (Xvec,Yvec)
  #
  # Arguments:
  # PTS ... A two-column matrix of points at which the surface is to be
  #         evaluated.
  # FDOBJ   ... A functional data object whose basis is of the FEM type.
  # NDERIVS ... A vector of length 2 containing the orders of derivatives
  #             for X and Y, respectively.
  #
  # Output:
  # EVALMAT   A matrix with number of rows equal to the length of Xvec
  #           containing the values of of one or more surfaces at these points.
  
  #  Last modified 19 November 2021 by Jim Ramsay.

  #  check Pts

  if (!is.numeric(pts))
  {
    stop('PTS is not a numerical array')
  } else {
    if (ncol(pts) != 2) {
      stop("PTS does not have two columns.")
    }
  }

  #  check fdobj

  if (!is.fd(fdobj)) {
    stop("FDOBJ is not a FEM functional data object.")
  }

  #  get basis

  basisobj <- fdobj$basis
  basismat <- eval.FEM.basis(pts, basisobj, nderivs)

  # get coefficient matrix

  coefs <- fdobj$coefs

  # compute surface values

  evalmat <- basismat %*% coefs

  return(evalmat)

}

