triquad <- function(nquad,v) {
  ###########################################################################
  #
  # triquad.m - Gaussian Quadrature for a triangular domain
  #
  # This scripts computes the nquad^2 nodes and weights for a triangle with
  # vertices given by the 3x2 vector v. The nodes are produced by collapsing
  # the square to a triangle.
  #
  #  X and Y are nquad by nquad matrices, and Wx and Wy are nquad-vectors.
  #
  # Sample Usage:
  #
  # >>[X,Y,Wx,Wy] = triquad(8,[0 0; 0 2; 2 1])
  # >>f = @(x,y) exp(x+y);
  # >>Q = Wx'*feval(f,X,Y)*Wy;
  #
  # Reference:  J.N. Lyness, Ronald Cools, A Survey of Numerical Cubature
  #             over Triangles (1994)
  #             http://citeseer.ist.psu.edu/lyness94survey.html
  #
  # Written by: Greg von Winckel
  # Contact: gregvw(at)math(dot)unm(dot)edu
  # http://math.unm.edu/~gregvw
  #
  ###########################################################################

  n    <- 1:nquad
  nnk  <- 2*n+1
  A    <- c(1/3,rep(1,nquad)/(nnk*(nnk+2)))
  n    <- 2:nquad
  nnk  <- nnk[n]
  B1   <- 2.0/9.0
  nk   <- n+1
  nnk2 <- nnk*nnk
  B    <- 4*(n*nk)^2/(nnk2*nnk2-nnk2)
  ab   <- matrix(c(A, 2, B1, t(B)),length(A),2)
  s    <- sqrt(ab[2:nquad,2])
  abmat <- diag(ab[1:nquad,1])
  for (i in 1:nquad-1) {
    abmat[i,i+1] <- s[i]
    abmat[i+1,i] <- s[i]
  }
  abeig <- eigen(abmat)
  V <- abeig$vectors
  D <- abeig$values
  ind <- nquad - 1:nquad + 1
  x  <- (D[ind]+1)/2
  wx <- ab[1,2]*V[1,ind]^2/4

  nquadm1 <- nquad-1
  nquadp1 <- nquad+1
  y  <- as.matrix(cos((2*seq(nquadm1,0,-1)+1)*pi/(2*nquadm1+2)))
  L  <- matrix(0,nquad,nquadp1)
  y0 <- 2
  eps  <- 1e-7
  crit <- 1
  while (crit > eps) {
    L[,1] <- 1
    L[,2] <- y
    for (k in 2:nquad) {
      L[,k+1] <- ( (2*k-1)*y*L[,k]-(k-1)*L[,k-1] )/k
    }
    Lp <- (nquadp1)*( L[,nquad]-y*L[,nquadp1] )/(1-y^2)
    y0 <- y
    y  <- y0-L[,nquadp1]/Lp
    crit <- max(abs(y-y0))
  }

  cd <- t(matrix(c(1, 0, 0, -1, 0, 1, 0, 1,-1),3,3)) %*% v
  t  <- (1+y)/2
  Wx <- as.matrix(abs(det(cd[2:3,]))*wx)
  Wy <- as.matrix(1/((1-y^2)*Lp^2)*(nquadp1/nquad)^2)
  tt <- t(matrix(t,nquad,nquad))
  xx <-   matrix(x,nquad,nquad)
  yy <- tt*xx
  X  <- cd[1,1]+cd[2,1]*xx+cd[3,1]*yy
  Y  <- cd[1,2]+cd[2,2]*xx+cd[3,2]*yy
  return(list(X=X, Y=Y, Wx=Wx, Wy=Wy))
}

