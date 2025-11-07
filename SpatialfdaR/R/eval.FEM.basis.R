eval.FEM.basis <- function(obspts, FEMbasis, nderivs=rep(0,2)) {
  # EVAL.FEM.BASIS evaluates the FEM basis object at points in obspts
  #
  # Arguments:
  # OBSPTS  ... A two-column of matrix of X and Y coordinates of points in a 
  #              triangular mesh
  # BASISOBJ ... A functional basis object of the FEM type.
  # NDERIVS  ... A vector of length 2 containing the orders of derivatives
  #              with respect to X and Y, respectively.  The derivative orders
  #              are restricted to 0, 1 and 2, and 2 may only be used if the
  #              basis is quadratic.
  #
  # Output:
  # EVALMAT  ... A matrix of the the values of FEMOBJ at obspts for one or more 
  #              surfaces.
  #
  #  Last modified on 19 November 2021 by Jim ramsay.

  #  check obspts matrix

  if (!is.matrix(obspts) ) stop("OBSPTS argument is not a matrix")
  if (dim(obspts)[2] != 2) stop("OBSPTS argument does not have two columns.")
  
  #  check derivatives

  if (length(nderivs) != 2) {
    stop("NDERIVS not of length 2.")
  }
  if (sum(nderivs) > 2) {
    stop("Maximum derivative order is greater than two.")
  }

  N <- dim(obspts)[1]

  #  Augment OBSPTS by one's for computing barycentric coordinates

  Pgpts <- cbind(rep(1,N), obspts)

  #  get nodes and index

  pts       <- FEMbasis$params$p
  tri       <- FEMbasis$params$t
  order     <- FEMbasis$params$order
  nodes     <- FEMbasis$params$nodes
  nodeindex <- FEMbasis$params$nodeindex
  Jvec      <- FEMbasis$params$J
  numnodes  <- dim(nodeindex)[1]

  # 1st, 2nd, and 3rd vertices of triangles

  if (order == 2) {
    if (numnodes == 1) {
      v1 <- matrix(nodes[nodeindex[,1],],1,2)
      v2 <- matrix(nodes[nodeindex[,3],],1,2)
      v3 <- matrix(nodes[nodeindex[,5],],1,2)
    } else {
      v1 <- nodes[nodeindex[,1],]
      v2 <- nodes[nodeindex[,3],]
      v3 <- nodes[nodeindex[,5],]
    }
  } else {
    if (order == 1) {
      if (numnodes == 1) {
        v1 <- matrix(nodes[nodeindex[,1],],1,2)
        v2 <- matrix(nodes[nodeindex[,2],],1,2)
        v3 <- matrix(nodes[nodeindex[,3],],1,2)
      } else {
        v1 <- nodes[nodeindex[,1],]
        v2 <- nodes[nodeindex[,2],]
        v3 <- nodes[nodeindex[,3],]
      }
    } else {
      stop("ORDER is neither 1 nor 2.")
    }
  }

  # 1st, 2nd, and 3rd columns of transformations to barycentric coordinates,
  # with a row for each vertex

  Jmat <- as.matrix(Jvec) %*% matrix(1,1,3)
  M1 <- cbind(v2[,1]*v3[,2] - v3[,1]*v2[,2], v2[,2]-v3[,2], v3[,1]-v2[,1])/Jmat/2
  M2 <- cbind(v3[,1]*v1[,2] - v1[,1]*v3[,2], v3[,2]-v1[,2], v1[,1]-v3[,1])/Jmat/2
  M3 <- cbind(v1[,1]*v2[,2] - v2[,1]*v1[,2], v1[,2]-v2[,2], v2[,1]-v1[,1])/Jmat/2

  # Identify triangles containing points in OBSPTS[i,]
  # if no triangle contains a point, ind[i] is NaN
  tricoef <- tricoefCal(pts, tri)
  ind  <- insideIndex(obspts, pts, tri, tricoef)

  #  interpolate values

  ones3 <- matrix(c(1,1,1),ncol=1)
  nnodes <- dim(nodes)[1]
  evalmat <- matrix(0,N,nnodes)
  for (i in 1:N) {
    indi <- ind[i]
    if (!is.na(indi)) {
      #  change to barycentric coordinates
      baryc1 <- (M1[indi,]*Pgpts[i,]) %*% ones3
      baryc2 <- (M2[indi,]*Pgpts[i,]) %*% ones3
      baryc3 <- (M3[indi,]*Pgpts[i,]) %*% ones3
      if (order == 2) {
        if (sum(nderivs) == 0) {
          evalmat[i,nodeindex[indi,1]] <- 2*baryc1^2 - baryc1
          evalmat[i,nodeindex[indi,2]] <- 2*baryc2^2 - baryc2
          evalmat[i,nodeindex[indi,3]] <- 2*baryc3^2 - baryc3
          evalmat[i,nodeindex[indi,4]] <- 4*baryc1*baryc2
          evalmat[i,nodeindex[indi,5]] <- 4*baryc2*baryc3
          evalmat[i,nodeindex[indi,6]] <- 4*baryc3*baryc1
        } else if (nderivs[1] == 1 && nderivs[2] == 0) {
          evalmat[i,nodeindex[indi,1]] <- (4*baryc1 - 1)*M1[indi,2]
          evalmat[i,nodeindex[indi,2]] <- (4*baryc2 - 1)*M2[indi,2]
          evalmat[i,nodeindex[indi,3]] <- (4*baryc3 - 1)*M3[indi,2]
          evalmat[i,nodeindex[indi,4]] <- 4*baryc2*M1[indi,2] +  4*baryc1*M2[indi,2]
          evalmat[i,nodeindex[indi,5]] <- 4*baryc3*M2[indi,2] +  4*baryc2*M3[indi,2]
          evalmat[i,nodeindex[indi,6]] <- 4*baryc1*M3[indi,2] +  4*baryc3*M1[indi,2]
        } else if (nderivs[1] == 0 && nderivs[2] == 1) {
          evalmat[i,nodeindex[indi,1]] <- (4*baryc1 - 1)*M1[indi,3]
          evalmat[i,nodeindex[indi,2]] <- (4*baryc2 - 1)*M2[indi,3]
          evalmat[i,nodeindex[indi,3]] <- (4*baryc3 - 1)*M3[indi,3]
          evalmat[i,nodeindex[indi,4]] <- 4*baryc2*M1[indi,3] +  4*baryc1*M2[indi,3]
          evalmat[i,nodeindex[indi,5]] <- 4*baryc3*M2[indi,3] +  4*baryc2*M3[indi,3]
          evalmat[i,nodeindex[indi,6]] <- 4*baryc1*M3[indi,3] +  4*baryc3*M1[indi,3]
        } else if (nderivs[1] == 1 && nderivs[2] == 1) {
          evalmat[i,nodeindex[indi,1]] <- 4*M1[indi,2]*M1[indi,3]
          evalmat[i,nodeindex[indi,2]] <- 4*M2[indi,2]*M2[indi,3]
          evalmat[i,nodeindex[indi,3]] <- 4*M3[indi,2]*M3[indi,3]
          evalmat[i,nodeindex[indi,4]] <- 4*M2[indi,2]*M1[indi,3] +  4*M2[indi,3]*M1[indi,2]
          evalmat[i,nodeindex[indi,5]] <- 4*M3[indi,2]*M2[indi,3] +  4*M3[indi,3]*M2[indi,2]
          evalmat[i,nodeindex[indi,6]] <- 4*M1[indi,2]*M3[indi,3] +  4*M1[indi,3]*M3[indi,2]
        } else if (nderivs[1] == 2 && nderivs[2] == 0) {
          evalmat[i,nodeindex[indi,1]] <- 4*M1[indi,2]*M1[indi,2]
          evalmat[i,nodeindex[indi,2]] <- 4*M2[indi,2]*M2[indi,2]
          evalmat[i,nodeindex[indi,3]] <- 4*M3[indi,2]*M3[indi,2]
          evalmat[i,nodeindex[indi,4]] <- 8*M2[indi,2]*M1[indi,2]
          evalmat[i,nodeindex[indi,5]] <- 8*M3[indi,2]*M2[indi,2]
          evalmat[i,nodeindex[indi,6]] <- 8*M1[indi,2]*M3[indi,2]
        } else if (nderivs[1] == 0 && nderivs[2] == 2) {
          evalmat[i,nodeindex[indi,1]] <- 4*M1[indi,3]*M1[indi,3]
          evalmat[i,nodeindex[indi,2]] <- 4*M2[indi,3]*M2[indi,3]
          evalmat[i,nodeindex[indi,3]] <- 4*M3[indi,3]*M3[indi,3]
          evalmat[i,nodeindex[indi,4]] <- 8*M2[indi,3]*M1[indi,3]
          evalmat[i,nodeindex[indi,5]] <- 8*M3[indi,3]*M2[indi,3]
          evalmat[i,nodeindex[indi,6]] <- 8*M1[indi,3]*M3[indi,3]
        } else {
          stop("Inadmissible derivative orders.")
        }
      } else {
        if (sum(nderivs) == 0) {
          evalmat[i,nodeindex[indi,1]] <- baryc1
          evalmat[i,nodeindex[indi,2]] <- baryc2
          evalmat[i,nodeindex[indi,3]] <- baryc3
        } else if (nderivs[1] == 1 && nderivs[2] == 0) {
          evalmat[i,nodeindex[indi,1]] <- M1[indi,2]
          evalmat[i,nodeindex[indi,2]] <- M2[indi,2]
          evalmat[i,nodeindex[indi,3]] <- M3[indi,2]
        } else if (nderivs[1] == 0 && nderivs[2] == 1) {
          evalmat[i,nodeindex[indi,1]] <- M1[indi,3]
          evalmat[i,nodeindex[indi,2]] <- M2[indi,3]
          evalmat[i,nodeindex[indi,3]] <- M3[indi,3]
        } else if (nderivs[1] == 1 && nderivs[2] == 1) {
        } else {
          stop("Inadmissible derivative orders.")
        }
      }
    }
  }

  return(evalmat)

}

