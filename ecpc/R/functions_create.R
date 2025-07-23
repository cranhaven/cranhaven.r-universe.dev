#Create co-data matrix Z for group set----
createZforGroupset <- function(groupset,p=NULL){
  #groupset: a list with index vectors of groups of covariates, as given in ecpc
  #p: number of covariates in total; taken as maximum index if not given
  
  if(is.null(p)) p <- max(unlist(groupset))
  G <- length(groupset) #number of groups
  Kg <- sapply(groupset,length) #group sizes
  i<-unlist(sapply(1:G,function(x){rep(x,Kg[x])}))
  j<-unlist(groupset)
  ind <- Matrix::sparseMatrix(i,j,x=1) #sparse matrix with ij element 1 if jth element in group i (global index), otherwise 0
  
  Ik <- as.vector(rep(1,G)%*%ind) #px1 vector with number of groups beta_k is in
  
  #make co-data matrix Z 
  #sparse matrix with ij element 1/Ij if beta_j in group i
  Z <- Matrix::t(ind)
  if(G>1){
    Z[,1:G]<- Matrix::t(ind[1:G,])/apply(ind[1:G,],2,sum)
  }
  #append for possible unpenalised covariates at the end
  if(dim(Z)[2]<p) Z <- rbind(Z,matrix(NaN,p-dim(Z)[1], G)) 
  
  return(Z)
}


#Obtain spline basis functions from continuous co-data variable z----
createZforSplines <- function(values,G=10,bdeg=3,index=NULL,p=NULL){
  #values: p-dimensional co-data vector
  #G: number of splines
  #bdeg: degree of the b-spline basis functions
  #index: index of the covariates corresponding to the values supplied. Useful when 
  #       part of the co-data is missing/seperated and only the non-missing/remaining 
  #       part should be modelled with splines
  #p: total number of covariates supplied in values and possibly other missing co-data.
  #   Assumed to be equal to the length of values
  
  if(!is.vector(values)) stop("Supply continuous co-data as vector")
  if(is.null(p)) p <- length(values)
  if(is.null(index)) index <- 1:p
  
  Z <- matrix(NA,p,G)
  splineB <- JOPS::bbase(values,nseg=G-bdeg,bdeg=bdeg,
                         xl = min(values)-10^-6*diff(range(values)),
                         xr = max(values)+10^-6*diff(range(values)))
  Z[index,] <- splineB
  return(Z)
}

#Create penalty matrix S----
createS <- function(orderPen=2,G=10,categorical=FALSE){
  #Create penalty matrix S
  if(orderPen==0){
    splineS <- diag(G)
  }else{
    if(!categorical){
      splineD <- diff(diag(G),diff=orderPen)
      splineS <- t(splineD)%*%splineD
    }else{
      splineS <- matrix(-1/(G-1),G,G)
      diag(splineS) <- 1
    }
  }
  return(splineS)
}

#Create common constraints for argument paraCon----
createCon <- function(G,shape="positive+monotone.i+convex"){
  #G: number of groups in corresponding co-data source
  #shape: any of positive, monotone.i/monotone.d (for increasing or decreasing),
  # convex/concave, or a combination thereof by appending with +
  
  shapes <- strsplit(shape,split='+',fixed=TRUE)[[1]]
  if(!(all(shapes%in%c("positive","monotone.i","monotone.d","convex","concave")))){
    stop("Shape not supported")
  }
  if(any(shapes=="concave")&any(shapes=="convex")){
    stop("Shape cannot be both convex and concave (then just use a constant intercept)")
  }
  if(any(shapes=="monotone.i")&any(shapes=="monotone.d")){
    stop("Shape cannot be both monotonically increasing and decreasing 
         (then just use a constant intercept)")
  }
  
  M.ineq <- b.ineq <- NULL
  for(shape in shapes){
    switch(shape,
           "positive"={
             M.ineq <- rbind(M.ineq, -diag(1,G))
             b.ineq <- c(b.ineq, rep(0,G))
           },
           "monotone.i"={
             M.ineq <- rbind(M.ineq, -diff(diag(1,G),differences = 1))
             b.ineq <- c(b.ineq, rep(0,G-1))
           },
           "monotone.d"={
             M.ineq <- rbind(M.ineq, diff(diag(1,G),differences = 1))
             b.ineq <- c(b.ineq, rep(0,G-1))
           },
           "convex"={
             M.ineq <- rbind(M.ineq, -diff(diag(1,G),differences = 2))
             b.ineq <- c(b.ineq, rep(0,G-2))
           },
           "concave"={
             M.ineq <- rbind(M.ineq, diff(diag(1,G),differences = 2))
             b.ineq <- c(b.ineq, rep(0,G-2))
           }
           )
  }
  
  return(list(M.ineq=M.ineq, b.ineq=b.ineq))
}