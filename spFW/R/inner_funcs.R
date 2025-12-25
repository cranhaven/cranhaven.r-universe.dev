
A_change <- function(A, g_ind){
  indA <- NULL
  A_names <- rownames(A)
  for (i in 1:length(g_ind)){
    ind <- which(g_ind[i]==A_names)
    indA <- c(indA,ind)
  }
  A1 <- A[indA,indA]
  return(A1)
}


Z_change <- function(Z, h_ind){
  indZ <- NULL
  Z_names <- rownames(Z)
  for (i in 1:length(h_ind)){
    ind <- which(h_ind[i]==Z_names)
    indZ <- c(indZ,ind)
  }
  Z1 <- Z[indZ,]
  return(Z1)
}


g_change <- function(g, g_ind){
  indg <- NULL
  g_names <- names(g)
  for (i in 1:length(g_ind)){
    ind <- which(g_ind[i]==g_names)
    indg <- c(indg,ind)
  }
  g1 <- as.numeric( g[indg] )
  return(g1)
}

b_change <- function(b, b_ind){
  indb <- NULL
  b_names <- names(b)
  for (i in 1:length(b_ind)){
    ind <- which(b_ind[i]==b_names)
    indb <- c(indb,ind)
  }
  b1 <- as.numeric( b[indb] )
  return(b1)
}

h_change <- function(h, h_ind){
  indh <- NULL
  h_names <- names(h)
  for (i in 1:length(h_ind)){
    ind <- which(h_ind[i]==h_names)
    indh <- c(indh,ind)
  }
  h1 <- as.numeric( h[indh] )
  return(h1)
}


find_pos <- function(X, x_ind){
  X1 <- rep(0,length(X))
  for (i in 1:length(x_ind)){
    ind <- which(X==x_ind[i])
    X1[ind] <- i
  }
  return(X1)
}

## Return the index of Y for different genotypes
Geno_ind <- function(VAR){
  g_ind <- names(table(VAR))
  m <- length(g_ind)
  geno_ind <- vector("list",m)
  for (i in 1:m){
    geno_ind[[i]] <- which(g_ind[i]==VAR)
  }
  return(geno_ind)
}


## Return the index of Y for different locations
loc_relation0 <- function(ENV){
  loc <- names(table(ENV))
  n <- length(loc)

  loc_ind <- vector("list",n)
  for (i in 1:n){
    indi <- which(ENV==loc[i])
    loc_ind[[i]] <- indi
  }
  loc_rela <- list(loc_name = loc,
                   loc_ind = loc_ind)
  return(loc_rela)
}


####################################################################################
## Spatial functions
####################################################################################


## transform the coordinates
trans_coor <- function(coor){
  x <- coor[,1]
  y <- coor[,2]
  rx <- range(x)
  ry <- range(y)
  qx <- rx[2]-rx[1]+1
  qy <- ry[2]-ry[1]+1
  x0 <- rx[1]-1
  y0 <- ry[1]-1
  x1 <- x-x0
  y1 <- y-y0
  coor1 <- cbind(x1 = x1, y1 = y1)
  dim1 <- c(qx,qy)
  position1 <- (y1-1)*qx + x1
  output <- list(coor1 = coor1, dim1 = dim1, position1 = position1)
  return(output)
}



## transform the coordinates
trans_coor2 <- function(coorA, coorB){
  coor <- rbind(coorA, coorB)
  x <- coor[,1]
  y <- coor[,2]
  xa <- coorA[,1]
  ya <- coorA[,2]
  xb <- coorB[,1]
  yb <- coorB[,2]

  rx <- range(x)
  ry <- range(y)
  qx <- rx[2]-rx[1]+1
  qy <- ry[2]-ry[1]+1
  x0 <- rx[1]-1
  y0 <- ry[1]-1
  xa1 <- xa-x0
  ya1 <- ya-y0
  xb1 <- xb-x0
  yb1 <- yb-y0

  coorA1 <- cbind(x1 = xa1, y1 = ya1)
  coorB1 <- cbind(x1 = xb1, y1 = yb1)
  dim1 <- c(qx,qy)
  positionA1 <- (ya1-1)*qx + xa1
  positionB1 <- (yb1-1)*qx + xb1
  output <- list(dim1 = dim1, coorA1 = coorA1, coorB1 = coorB1,
                 positionA1 = positionA1,  positionB1 = positionB1)
  return(output)
}



## Phi & Psi relationship
loc_relation <- function(ENV, COOR){
  loc <- names(table(ENV))
  n <- length(loc)
  loc_dim <- vector("list",n)
  loc_ind <- vector("list",n)
  loc_pos <- vector("list",n)
  for (i in 1:n){
    indi <- which(ENV==loc[i])
    coori <- COOR[indi,]
    Outi <- trans_coor(coori)
    dimi <- Outi$dim1
    posi <- Outi$position1
    loc_dim[[i]] <- dimi
    loc_ind[[i]] <- indi
    loc_pos[[i]] <- posi
  }
  loc_rela <- list(loc_name = loc, loc_dim = loc_dim,
                   loc_ind = loc_ind, loc_pos = loc_pos)
  return(loc_rela)
}

## Phi & Psi relationship
## for training set
loc_relation1 <- function(ENV1, ENV2, COOR1, COOR2){
  loc <- names(table(ENV1))
  n <- length(loc)
  loc_dim <- vector("list",n)
  loc_ind <- vector("list",n)
  loc_pos <- vector("list",n)
  for (i in 1:n){

    ind1i <- which(ENV1==loc[i])
    ind2i <- which(ENV2==loc[i])
    coor1i <- COOR1[ind1i,]
    coor2i <- COOR2[ind2i,]

    Outi <- trans_coor2(coor1i, coor2i)
    dimi <- Outi$dim1
    posi <- Outi$positionA1
    loc_dim[[i]] <- dimi
    loc_ind[[i]] <- ind1i
    loc_pos[[i]] <- posi
  }
  loc_rela <- list(loc_name = loc, loc_dim = loc_dim,
                   loc_ind = loc_ind, loc_pos = loc_pos)
  return(loc_rela)
}


## for testing set
loc_relation2 <- function(ENV1, ENV2, COOR1, COOR2){
  loc <- names(table(ENV2))
  n <- length(loc)
  loc_dim <- vector("list",n)
  loc_ind <- vector("list",n)
  loc_pos <- vector("list",n)
  for (i in 1:n){

    ind1i <- which(ENV1==loc[i])
    ind2i <- which(ENV2==loc[i])
    coor1i <- COOR1[ind1i,]
    coor2i <- COOR2[ind2i,]

    Outi <- trans_coor2(coor1i, coor2i)
    dimi <- Outi$dim1
    posi <- Outi$positionB1
    loc_dim[[i]] <- dimi
    loc_ind[[i]] <- ind2i
    loc_pos[[i]] <- posi
  }
  loc_rela <- list(loc_name = loc, loc_dim = loc_dim,
                   loc_ind = loc_ind, loc_pos = loc_pos)
  return(loc_rela)
}





## transform from psi to phi
psi2phi <- function(Psi, loc_rela, N){
  Phi <- rep(0,N)
  n <- length(loc_rela$loc_name)
  for (i in 1:n){
    ind_i <- loc_rela$loc_ind[[i]]
    phi_i <- (Psi[[i]])[ loc_rela$loc_pos[[i]] ]
    Phi[ind_i] <- phi_i
  }
  return(Phi)
}




## transform from varphi to psi
varphi2psi <- function(Varphi, n, B){
  Psi <- vector("list",n)
  for (j in 1:n){
    psi_j <- (B[[j]])%*%Varphi[[j]]
    Psi[[j]] <- psi_j
  }
  return(Psi)
}




## compute W in one dimension
W1dim <- function(q){
  W <- matrix(0, ncol = q, nrow = q)
  W[1,1] <- 1
  for (i in 2:q){
    W[i,i] <- 2
    W[i,i-1] <- -1
    W[i-1,i] <- -1
  }
  W[q,q] <- 1
  return(W)
}





## only eigenvalues of Wj
besag_Wj_eig_val <- function(dim_l, theta){
  Wq1 <- W1dim(dim_l[2])
  Wq2 <- W1dim(dim_l[1])
  svd_Wq1 <- svd(Wq1)
  svd_Wq2 <- svd(Wq2)
  Dq1 <- svd_Wq1$d[(dim_l[2]):1]
  Dq2 <- svd_Wq2$d[(dim_l[1]):1]
  D <- theta * rep(Dq1, each = dim_l[1]) +
    (1 - theta) * rep(Dq2, dim_l[2])
  return(D)
}




## compute Bj (return a list that contains {B1 ... Bn})
besag_B <- function(loc_rela){
  loc <- loc_rela$loc_name
  n <- length(loc)
  B <- vector("list",n)
  for (j in 1:n){
    dimj <- loc_rela$loc_dim[[j]]
    r <- dimj[1]
    c <- dimj[2]
    Bj <- matrix(0, ncol = r*c-1, nrow = r*c)
    for ( i in 1:(r*c-1)){
      x <- rep(0,r*c-1)
      x[i] <- 1
      Bj[,i] <- idct2mod(x,r,c)
    }

    B[[j]] <- Bj
  }
  return(B)
}



