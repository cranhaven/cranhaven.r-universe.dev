ditocord <- function(X, res, nextres, ang, type){
  
  out <- parsebonds(res, nextres)
  bond_len <- out$bond_len
  bond_ang <- out$bond_ang
  
  v12 <- X[,2] - X[,1]
  v12 <- v12/base::norm(v12,"2")
  v23 <- X[,3] - X[,2]
  v23 <- v23/base::norm(v23,"2")
  
  vector.cross <- function(a, b) {
    if(length(a)!=3 || length(b)!=3){
      stop("Cross product is only defined for 3D vectors.");
    }
    i1 <- c(2,3,1)
    i2 <- c(3,1,2)
    return (a[i1]*b[i2] - a[i2]*b[i1])
  }
  
  ang_12_23 <- angvec(v12,v23)
  na <- vector.cross(v12, v23)
  na <- na/base::norm(na,"2")
  
  switch(type,
         "PHI" = {d34 <- bond_len[3]
                  ang_23_34 <- pi - pi*bond_ang[7]/180},
         "PSI" = {d34 <- bond_len[2]
                  ang_23_34 = pi - pi*bond_ang[3]/180})
  
  p_na <- d34*sin(ang_23_34)*cos(ang-pi/2)
  p_v12 <- d34*sin(ang_23_34)*sin(ang-pi/2)/sin(ang_12_23)
  p_v23 <- d34*cos(ang_23_34) - d34*sin(ang_23_34)*sin(ang-pi/2)/tan(ang_12_23)
  
  v34 <- p_na*na + p_v12*v12 + p_v23*v23
  point <- X[,3] + v34
  
  return(point=point)
  
}

angvec <- function(a,b){
  ang <- acos((a %*% b)/norm(a,"2")/norm(b,"2"))
  return(ang)
}

PlaneFinder <- function(X, cur, next_){
  
  out <- parsebonds(cur,next_)
  bond_len <- out$bond_len
  bond_ang <- out$bond_ang
  
  if(cur == 15){
    bond_len[5] <- 1.473
    bond_ang[5] <- 125
  }
  
  tX <- matrix(rep(0,3*6), nrow=3)
  
  tX[2,4] <- bond_len[2]
  
  tX[1,2] <- bond_len[1]*cos((bond_ang[2] - 90)*pi/180)
  tX[2,2] <- -bond_len[1]*sin((bond_ang[2] - 90)*pi/180)
  
  tX[1,1] <- -bond_len[3]*cos((bond_ang[3] - 90)*pi/180)
  tX[2,1] <- -bond_len[3]*sin((bond_ang[3] - 90)*pi/180)
  
  tX[1,6] <- bond_len[4]*cos((bond_ang[4] - 90)*pi/180)
  tX[2,6] <- tX[2,4] + bond_len[4]*sin((bond_ang[4] - 90)*pi/180)
  
  tX[1,5] <- -bond_len[5]*cos((bond_ang[5] - 90)*pi/180)
  tX[2,5] <- tX[2,4] + bond_len[5]*sin((bond_ang[5] - 90)*pi/180)
  
  talign <- tX[,c(1,3,4)]
  
  out <- procrustesb(X, talign)
  R <- out$R
  t <- out$t
  err <- out$err
  
  if(err > 1e-2){
    warning("error is too large")
  }
  
  outX <- R %*% tX + matrix(t, ncol=ncol(tX), nrow=nrow(R))
  
  return(outX)
  
}

SidechainFinder <- function(X, index_align, A){
  
  tX <- as.matrix(A$X[,A$first_meta:A$last_meta])
  talign <- tX[,index_align]
  
  reflection <- 0
  out <- procrustesb(X, talign, reflection)
  R <- out$R
  t <- out$t
  err <- out$err
  
  if(err > 5e-1){
    warning("Error is too large")
  }
  
  outX <- R %*% tX + matrix(t, ncol=ncol(tX), nrow=nrow(R))
  
  return(outX)
}

parsebonds <- function(cur, next_){
  
  bond_len <- rep(0,5)
  
  #C-O bond
  bond_len[1] <- 1.231
  
  #C-N bond
  if(next_ == 15){
    bond_len[2] <- 1.341
  }else{
    bond_len[2] <- 1.329
  }
  
  #C-CA bond
  if(cur == 8){
    bond_len[3] <- 1.516
  }else{
    bond_len[3] <- 1.525
  }
  
  #CA-N bond
  if(cur == 8){
    bond_len[4] <- 1.451
  }else if(cur == 15){
    bond_len[4] <- 1.466
  }else{
    bond_len[4] <- 1.458
  }
  
  #N-H bond
  bond_len[5] <- 0.98
  
  
  ## Bond Angles ##
  bond_ang <- rep(0, 7)
  
  #CA-C-O
  if(cur == 8){
    bond_ang[1] <- 120.8
  }else{
    bond_ang[1] <- 120.8
  }
  
  #N-C-O
  if(next_ == 15){
    bond_ang[2] <- 122
  }else{
    bond_ang[2] <- 123
  }
  
  #CA-C-N
  bond_ang[3] <- 360 - sum(bond_ang[1:2])
  
  #C-N-CA
  if(next_ == 8){
    bond_ang[4] <- 120.6
  }else if(cur == 15){
    bond_ang[4] <- 122.6
  }else{
    bond_ang[4] <- 121.7
  }
  
  #C-NH1-H
  bond_ang[5] <- 120
  bond_ang[6] <- 360 - sum(bond_ang[4:5])
  
  #C-CA-N
  if(cur == 15){
    bond_ang[7] <- 111.8
  }else{
    bond_ang[7] <- 111.2
  }
  
  return(list(bond_len=bond_len, bond_ang=bond_ang))
}

procrustesb <- function(X,Y,reflection=1){

  dx <- nrow(X)
  nx <- ncol(X)
  
  dy <- nrow(Y)
  ny <- ncol(Y)
  
  if(dx != dy || nx != ny){
    stop("X and Y must be the same size")
  }
  
  mx <- rowSums(X)/nx
  my <- rowSums(Y)/ny
  
  cX <- X - mx
  cY <- Y - my
  
  normX <- 1
  normY <- 1
  
  cX <- cX/normX
  cY <- cY/normY
  
  M <- cY %*% t(cX)
  
  out <- svd(M)
  U <- out$u
  S <- out$d
  V <- out$v

  R <- V %*% t(U)
  
  if(!reflection && det(R) < 0){
    V[,ncol(V)] <- -V[,ncol(V)]
    R <- V %*% t(U)
  }
  
  t <- mx - R %*% my
  err <- base::norm(cX - R %*% cY, "F")
  
  return(list(R=R, t=t, err=err))
}