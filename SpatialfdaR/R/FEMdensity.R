FEMdensity <- function(cvec, obspts, FEMbasis, K1=NULL, lambda=0) {

#FEMDENSITY computes the negative log likelihood and its first and second
#  derivatives with respect to CVEC for (the inhomogeneous Poisson
#  model for (the density of spatially distributed data whose
#  coordinates are XVEC and YVEC.  Covariate values in Zmat may also be
#  used to model the data
#
#  Input:
#
#  CVEC   ...  A column vector of n coefficients for first order finite
#                 element functions
#  OBSPTS ...  An N by 2 matrix of locations of observed points
#  FEMbasis ...  An order 1 FEM basis object defined by function
#                 createFEMBASIS, a functional data object defined in terms of
#              an FEM basis object, or an fdPar object using an FEM basis object
#  K1      ... An order n square stiffness matrix produced using function stiff
#  LAMBDA  ... A non-negative real number controlling the size of the roughness
#              penalty LAMBDA*t(CVEC) %*% K1 %*% CVEC
#
#  Output:
#
#  F    ... Objective function value
#  GRAD ... Gradient with respect to coordinates
#  NORM ... Norm of the gradient vector
#  PVEC ... Vector of probabilities
#  PDEN ... Norming constant for probabilities

    # Last modified on 8 November 2021 by Jim Ramsay.

    #  ensure that CVEC is in column matrix format

    cvec = as.matrix(cvec)

    #  retrieve nodeList from FEMbasis and arrays from nodeList

    pts        <- FEMbasis$params$p
    tri        <- FEMbasis$params$t
    nodeindex  <- FEMbasis$params$nodeindex
    ntri       <- dim(nodeindex)[1]
    nodes      <- FEMbasis$params$nodes
    if (is.null(K1) && lambda > 0) {
        K1 <- stiff.FEM(FEMbasis)
    }

    #  check obspts

    obsptsdim <- dim(obspts)
    N <- obsptsdim[1]

    if (!is.matrix(obspts)) {
        stop("Argument OBSPTS is not a matrix")
    }
    if (obsptsdim[2] != 2) {
        stop("Argument OBSPTS does not have two columns.")
    }

    # identify element containing point in vector (obspts[i,])
    # if (no element contains a point, ind[i] is NaN

    print("tricoefCal")
    print(pts)
    print(tri)
    tricoef <- tricoefCal(pts, tri)
    indpts  <- insideIndex(obspts, pts, tri, tricoef)
  
    if (any(is.na(indpts))) {
        stop('Points are outside of boundary.')
    }

    #  evaluate basis functions at points

    phimatData <- eval.FEM.basis(obspts, FEMbasis)
    nbasis <- dim(phimatData)[2]

    #  check cvec, which should of length NBASIS if (there are no covariates,
    #  or NBASIS + Q is there Zmat is N by q

    q <- 0
    if (length(cvec) != nbasis+q) {
        stop('CVEC is not of correct length.')
    }

    #  compute first term of log likelihood

    lnintens <- phimatData %*% cvec
    F    <- -sum(lnintens)
    grad <- -apply(phimatData,2,sum)
    Pnum <-  exp(lnintens)
    if (lambda > 0) {
        F    <- F + lambda*t(cvec) %*% K1 %*% cvec
        grad <- grad + 2*lambda*K1 %*% cvec
    }

    #  loop through elements and compute integrals over elements

    nquad   <- 6
    gradsum <- matrix(0,nbasis,1)
    tripts  <- matrix(0,3,2)
    int0el  <- matrix(0,ntri,1)
    for (el in 1:ntri) {
        #  point indices for (this triangle
        triel <- tri[el,]
        #  set up 3 by 2 matrix of vertex coordinates
        for (j in 1:3) {
            tripts[j,] <- pts[triel[j],]
        }
        #  set up quadrature points and weights for (this triangle
        #  Note 10May14:  pull these out of basis object if (present.
        #  Check iMac to see if (this is done there.
        triquadList <- triquad(nquad,tripts)
        Xel   <- triquadList$X
        Yel   <- triquadList$Y
        Wx    <- triquadList$Wx
        Wy    <- triquadList$Wy
        Xquad <- matrix(Xel,nquad^2,1)
        Yquad <- matrix(Yel,nquad^2,1)
        #  evaluate basis at quadrature points
        phimatquad <- eval.FEM.basis(cbind(Xquad, Yquad), FEMbasis)
        #  log intensity at quadrature points
        lnintensel <- phimatquad %*% cvec
        #  intensity values
        intensvec  <- exp(lnintensel)
        intensmat  <- matrix(intensvec,nquad,nquad)
        #  compute approximation to integrated intensity
        int0el[el] <- t(Wx) %*% intensmat %*% Wy
        #  derivatives with respect to coefficients
        gmat   <- phimatquad*matrix(intensvec,nquad^2,nbasis)
        garray <- array(gmat,c(nquad,nquad,nbasis))
        #  approximation to integrated derivatives
        for (k in 1:nbasis) {
            gradsum[k] <- gradsum[k] + t(Wx) %*% garray[,,k] %*% Wy
        }
    }
    int0sum <- sum(int0el)
    #  compute vector of density values at observation points
    Pden <- int0sum
    Pvec <- Pnum/Pden

    #  complete calculation of negative log likelihood and its gradient

    F    <- F    + N*log(int0sum)
    grad <- grad + N*gradsum/int0sum

    gradnorm = sqrt(sum(grad^2))

    return(list(F=F, grad=grad, norm=gradnorm, Pvec=Pvec, Pden=Pden))

}
