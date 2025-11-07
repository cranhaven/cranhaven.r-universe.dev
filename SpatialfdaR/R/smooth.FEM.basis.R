smooth.FEM.basis <- function(FEMloc, FEMdata, FEMbasis, lambda=1e-12,
                             wtvec=NULL, covariates=NULL, Laplace=NULL) {
  #  SMOOTH_FEM_Basis Smooths spatial data observed at a set of points,
  #  possibly using covariate values as additional contributors to the fit to
  #  the data values.  The basis object is of type FEM defined by a
  #  triangular mesh within one or more bounding polygons.  The basis
  #  functions are either piecewise linear or piecewise quadratic functions.
  #  The piecewise linear functions are nonzero only over the hexagon
  #  surrounding a VERTEX and with value one at the VERTEX.  The piecewise
  #  quadratic functions include these three functions, and in addition
  #  including the additional three functions that reach one at the midpoints
  #  of the edges of triangles.  The points at which basis functions reach
  #  are called NODES of the mesh. Thus, vertices and nodes correspond for
  #  piecewise linear basis functions, but the nodes also include midpoints
  #  in the case of piecewise quadratidc basis functions.  Note that all
  #  basis functions are continuous across edges and vertices of triangles,
  #  but only differentiable within a triangle.  In the piecewise quadratic
  #  case, basis functions also so have nonzero second partial derivatives
  #  within triangles, but not along edges or at vertices.
  #
  #  The roughness penalty on the fitting function u(x,y) uses the Laplacian
  #  operator
  #             di^2 u/di x^2 + di^2 u/ di y^2
  #  which measures the local curvature at point (x,y).
  #  Arguments:
  # FEMLOC     ... A two-column matrix containing locations of data values.
  #                The first column contains the X-coordinates of the points
  #                where observations have been made, and the second column
  #                contains their Y-coordinates.
  # FEMDATA    ... An nobs by NSURF data matrix containing observations at nobs
  #                points for each of NSURF surfaces.
  # FEMBASIS   ... A functional basis object of the FEM type.
  # LAMBDA     ... a scalar smoothing parameter, must be non-negative
  # WTVEC      ... A vector of length nobs, containing nonnegative weights to be
  #                applied to the data values, or a symmetric positive
  #                definite matrix of order nobs.  Defaults to ones if NULL.
  # COVARIATES ... a design matrix with rows corresponding to data
  #                points and columns corresponding to covariates.
  #                Defaults to NULL
  #
  #  Output: A list object containing these names fields:
  #
  # SMOOTH.FD  ...  A list object with names "coefs" and "FEMbasis" defining
  #                 the FEM fd object for the data smooth
  # DF         ...  A measure of degrees of freedom for the smooth surface
  # GCV        ...  A generalized cross-validation coefficient
  # LAPLACE.FD ...  A list object with names "coefs" and "FEMbasis" defining
  #                 the FEM fd object for the Laplace operator
  #
  #  Last modified on 10 December 2021 by Jim Ramsay.
  
  #  ---------------------------------------------------------------
  #                      Check arguments
  #  ---------------------------------------------------------------
  
  FEMdata <- as.matrix(FEMdata)
  datadim <- dim(FEMdata)
  nobs    <- datadim[1]
  nsurf   <- datadim[2]
  
  if (dim(FEMloc)[1] != nobs)
    stop("Number of observation points not equal to number of observations,")
  
  #  LAMBDA
  
  if (!is.numeric(lambda)) stop('LAMBDA is not numeric.')
  
  #  check WTVEC
  
  results <- wtcheck(nobs, wtvec)
  wtvec   <- results$wtvec
  onewt   <- results$onewt
  matwt   <- results$matwt
  
  #  ---------------------------------------------------------------
  #                      Set up node information
  #  ---------------------------------------------------------------
  
  order     <- FEMbasis$params$order
  nodes     <- FEMbasis$params$nodes
  nodeindex <- FEMbasis$nparams$odeindex
  Jvec      <- FEMbasis$params$J
  metric    <- FEMbasis$params$metric
  numnodes  <- nrow(nodes)
  indnodes  <- 1:numnodes
  
  #  Construct penalty matrix and 'b' vector for Ax=b.
  
  #  ---------------------------------------------------------------
  # construct mass matrix K0
  #  ---------------------------------------------------------------
  
  K0 <- mass.FEM(FEMbasis)
  
  #  ---------------------------------------------------------------
  # construct stiffness matrix K1
  #  ---------------------------------------------------------------
  
  K1 <- stiff.FEM(FEMbasis)
  
  #  ------------------------------------------------------------------
  # Construct the basis value matrix Phimat,
  # If there are no data points (loc == NULL), this is the identity
  # matrix with data points assumed to be at nodes.
  # Otherwise, Phimat is an nobs by numnodes matrix containing
  # values of each basis function at an observation point.
  # This will be a highly sparse matrix.
  #  ------------------------------------------------------------------
  
  Phimat <- eval.FEM.basis(FEMloc, FEMbasis, c(0,0))
  
  #  set up projection matrix for space orthogonal to covariate space
  #  Matlab uses qr decomposition here
  
  if(!is.null(covariates)) {
    covariates <- as.matrix(covariates)
    covdim <- dim(covariates)
    if (covdim[1] != nobs)
      stop("Number of covariate values not equal to number of observations.")
    q <- covdim[2]
    M <- ( solve( crossprod(covariates) ) ) %*% t(covariates)
    H <- covariates %*% M
    beta <- M %*% FEMdata
    Q <- diag(rep(1,nobs)) - H
  } else {
    beta <- NA
    Q <- diag(rep(1,nobs))
  }
  
  #  ---------------------------------------------------------------
  # construct vector b for system Ax=b
  #  ---------------------------------------------------------------
  
  PhitYmat <- matrix(0,numnodes*2,nsurf)
  
  
  wtdiag  <- diag(as.numeric(wtvec))
  if (!is.null(covariates)) {
    PhitYmat[1:numnodes,] <- t(Phimat) %*% wtdiag %*% Q %*% FEMdata
  } else {
    PhitYmat[1:numnodes,] <- t(Phimat) %*% wtdiag %*%       FEMdata
  }
  
  #  ---------------------------------------------------------------
  #  construct crossproduct matrix PhitPhimat
  #  ---------------------------------------------------------------
  
  PhitPhimat <- t(Phimat) %*% wtdiag %*% Phimat
  
  #  ---------------------------------------------------------------
  #            Solve the linear equations
  #  ---------------------------------------------------------------
  
  if (!is.null(Laplace)) {
    #  Laplace.fd is required
    # construct matrix A for system Ax=b.
    Amat  <- rbind(cbind(PhitPhimat, -lambda*K1),
                   cbind(K1,             K0))
    # solve the linear equations using lsfit
    PhitYmatfit <- lsfit(Amat, PhitYmat, intercept=FALSE)
    coefmat <- as.matrix(PhitYmatfit$coefficients)
    
    coef1 <- coefmat[indnodes,]
    coef2 <- as.matrix(coefmat[numnodes+indnodes,])
    smooth.fd  <- fd(coef1, FEMbasis)
    Laplace.fd <- fd(coef2, FEMbasis)
  } else {
    #  Laplace.fd not required
    coef1 <- solve(PhitPhimat + lambda*K1) %*% PhitYmat[1:numnodes]
    coef2 <- NULL    
    Laplace.fd <- NULL
  }
  
  smooth.fd <- fd(coef1, FEMbasis)
  
  #  compute the effective degrees of freedom df
  Smat <- Phimat %*% solve(PhitPhimat + lambda*K1) %*% t(Phimat)
  if(is.null(covariates))
  {
    df <- sum(diag(Smat))
  } else {
    df  <- ncol(covariates) +
      sum(diag(Phimat %*% Smat %*% t(Phimat) %*% Q))
  }
  
  #  compute error sum of squares
  
  datahat <- Phimat %*% coef1
  SSE <- sum((FEMdata - datahat)^2)
  
  #  compute generalized cross-validation gcv
  
  if (!is.na(df) && df < numnodes) {
    gcv <- (SSE/nobs)/((nobs - df)/nobs)^2
  } else {
    gcv <- NULL
  }
  
  smoothList <- list(fd=smooth.fd, df=df, gcv=gcv, beta=beta,
                     SSE=SSE, Laplace.fd=Laplace.fd)
  
  return(smoothList)
}
