stiff.FEM <- function(FEMbasis) {
  # STIFF.FEM produces the nnod*nnod stiffness matrix K1
  # defined (K1)jk = int(dpsik/da*dpsij/da + dpsik/db*dpsij/db).
  #
  # Input: FEMbasis is a List object produced by function makenodes.
  #    It contains:
  #        ORDER     ... The order of the element (1 or 2)
  #        NODES     ... Coordinates of node points
  #        NODEINDEX ... indices of node points for each element
  #        JVEC      ... Jacobian of the affine transformation of each
  #                      element to the master element
  #        METRIC    ... The crossproduct of the inverse of the linear
  #                      part of the transformation
  #
  # Output: K1 is an nnod*nnod matrix out which is
  #        the sum of the nele element stiffness matrices
  #        and the penalty stiffness matrix.
  #        These i'th element matrix has (ij)'th element defined
  #        as follows:
  #        Let psita and psitb be the partial derivatives of the
  #        t'th shape function with respect to a and b (1<=t<=6).
  #        Then the integral of the sum of products
  #        (psija*psika+psijb+psikb) over the i'th element is
  #        computed.  Then that value is assigned to the
  #        (nodeindex(i,j),nodeindex(i,k))'th entry of the i'th elemental
  #        stiffness matrix and the other elements are given the value zero.
  #
  #
  #  Last modified 19 November 2021 by Jim Ramsay.
  
  #  retrieve arrays from FEMbasis
  
  order     <- FEMbasis$params$order
  nodes     <- FEMbasis$params$nodes
  nodeindex <- FEMbasis$params$nodeindex
  Jvec      <- FEMbasis$params$J
  metric    <- FEMbasis$params$metric
  
  nele  <- dim(nodeindex)[[1]]
  nnod  <- dim(nodes)[[1]]
  
  K1   <- matrix(0,nrow=nnod,ncol=nnod)
  
  #  assemble the stiffness matrix
  
  if (order == 2) {
    #  values of K1 for master elements
    
    KXX <- matrix( c( 3, -4,  1,  0,  0,  0,
                     -4,  8, -4,  0,  0,  0,
                      1, -4,  3,  0,  0,  0,
                      0,  0,  0,  8,  0, -8,
                      0,  0,  0,  0,  0,  0,
                      0,  0,  0, -8,  0,  8), ncol=6, nrow=6, byrow=T)/6
    
    KXY <- matrix( c( 3,  0,  0,  0,  1, -4,
                     -4,  4,  0, -4,  0,  4,
                      1, -4,  0,  4, -1,  0,
                      0, -4,  0,  4,  4, -4,
                      0,  0,  0,  0,  0,  0,
                      0,  4,  0, -4, -4,  4), ncol=6, nrow=6, byrow=T)/6
    
    KYY <- matrix( c( 3,  0,  0,  0,  1, -4,
                      0,  8,  0, -8,  0,  0,
                      0,  0,  0,  0,  0,  0,
                      0, -8,  0,  8,  0,  0,
                      1,  0,  0,  0,  3, -4,
                     -4,  0,  0,  0, -4,  8), ncol=6, nrow=6, byrow=T)/6
    
    for (el in 1:nele) {
      ind    <- nodeindex[el,]
      K1M <- (metric[el,1,1]*KXX    + metric[el,1,2]*KXY +
                metric[el,2,1]*t(KXY) + metric[el,2,2]*KYY)
      K1[ind,ind] <- K1[ind,ind] + K1M*Jvec[el]
    }
  } else if (order == 1) {
    KXX <- matrix( c(  1, -1,  0,
                      -1,  1,  0,
                       0,  0,  0), ncol=3, nrow=3, byrow=T) /2
    
    KXY <- matrix( c(  1,  0, -1,
                      -1,  0,  1,
                       0,  0,  0), ncol=3, nrow=3, byrow=T) /2
    
    KYY <- matrix( c(  1,  0, -1,
                       0,  0,  0,
                      -1,  0,  1), ncol=3, nrow=3, byrow=T) /2
    
    #  assemble the stiffness matrix
    
    for (el in 1:nele) {
      ind      <- nodeindex[el,]
      K1M <- (metric[el,1,1]*KXX    + metric[el,1,2]*KXY +
                metric[el,2,1]*t(KXY) + metric[el,2,2]*KYY)
      K1[ind,ind] <- K1[ind,ind] + K1M*Jvec[el]
    }
  } else {
    stop("ORDER not 1 or 2")
  }
  
  K1
  
}
