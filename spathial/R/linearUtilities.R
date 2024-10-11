#Initialize NC medoids with init_type rational.
initMedoids <- function(X, XX, n, init_type, boundary_ids){
  N<-nrow(X)
  d<-ncol(X)
  med_ids<-rep(-1,n)
  if(init_type=="kpp"){
    #Select a random point (not a boundary id)
    med_ids[1]<-sample(setdiff(rownames(X),boundary_ids),1)
    # Now fill the path with n randomly picked medoids
    boundary_coords<-X[boundary_ids,]
    Xmed_dst <- dist(as.matrix(X), as.matrix(rbind(X[med_ids[1],],boundary_coords)), XX)
    for(i in 1:(n-1)){
      # Calculate all distances between points up to here and boundaries
      Xmed_dst_sq<-Xmed_dst^2
      D2<-apply(Xmed_dst_sq,1,min)
      D2_n<-1.0/sum(D2)

      med_id<-rownames(X)[which.max(D2*D2_n)]

      med_ids[i+1]<-med_id
      dst <- dist(as.matrix(X), as.matrix(X[med_ids[i+1],,drop=FALSE]), XX)
      Xmed_dst <- cbind(Xmed_dst[,1:(ncol(Xmed_dst)-2), drop = FALSE], dst, Xmed_dst[,(ncol(Xmed_dst)-1):ncol(Xmed_dst)])
    }
  } else if (init_type=='uniform'){
    med_ids<-sample(rownames(X),n)
  } else {
    stop("init_type not recognized.")
  }
  return(med_ids)
}

#Find the elbow in a fuction f, as the point on f with max distance
#from the line connecting f[0,:] and f[-1,:]
find_elbow <- function(f){
  ps <- array(c(f[1,1], f[1,2]), dim = c(2))
  pe <- array(c(f[nrow(f),1], f[nrow(f),2]), dim = c(2))
  p_line_dst = array(data = 0.0, (nrow(f)-2))
  for(i in (2:(nrow(f)-1))){
    p <- array(c(f[i,1],f[i,2]), dim= c(2))

    mtx_1 <- pe-ps
    mtx_2 <- ps-p
    mtx_3 <- pe-ps
    cp <- ((mtx_1[1]*mtx_2[2]) - (mtx_1[2]*mtx_2[1]))
    den <- norm(as.matrix(cp), "F")
    num <- norm(as.matrix(mtx_3), "F")

    p_line_dst[i-1] <- (den / num)
  }
  elb_id = (which.max(p_line_dst)) + 1
  return(elb_id)
}

#Optimized Euclidean distance
dist <- function(X, Y, XX=NULL){
  m  <- nrow(X); n <- nrow(Y)
  if(is.null(XX)){
    XX <- matrix(rep(matrixStats::rowSums2(X*X), m), n, m, byrow=F)
  }
  sub <- XX[,1:n]
  XY <- X %*% t(Y)    # (m,n)-matrix
  YY <- matrix(rep(matrixStats::rowSums2(Y*Y), m), m, n, byrow=T)
  return(sqrt(pmax(sub + YY - 2*XY, 0)))
}
