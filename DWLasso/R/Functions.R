library(glmnet)

# =========== Internal functions =========== #
degreeComp <- function(out.mat){
  # Weight computation
  out.mat[abs(out.mat) > 0] <- 1
  d.wlasso <- apply(out.mat,2,sum)
  d.wlasso[d.wlasso == 0] <- 1
  w.wlasso <- 1/d.wlasso
  wlasso_norm <- w.wlasso/sum(w.wlasso)
  return(wlasso_norm)
}

weightComp <- function(dat,lam=0.4,w.mb){
  # Degree computation
  adj.out <- MBLasso(dat,lambda=lam,w.mb)
  d.mb <- degreeComp(adj.out)
  return(d.mb)
}

MBLasso <- function(dat,lambda=0.4,w.mb){

  # Solving Meinhausen-Buhlmann method by coordinate descent
  error <- list()
  for (i in 1:ncol(dat)){
    x = scale(dat)
    m <- ncol(x)
    noti <- (1:m)[-i]
    yi <- x[ ,i]       ## response
    Xi <- x[ ,noti]    ## predicted by all other nodes with i missing
    output <- glmnet(Xi,yi,lambda=lambda, intercept=F,penalty.factor=as.vector(w.mb[-i]))$beta
    error[[i]] <- as.vector(output)
  }

  # Combine the values into the adjacency matrix
  adj_out <- abs(combined(dat,error))
  
  # Make the matrix symmetric
  adj_out2 <- (adj_out + t(adj_out))/2
  return(adj_out2)
}


combined <- function(dat,y){
  # Function to convert list to matrix
  output <- matrix(unlist(y), ncol = ncol(dat)-1, byrow = TRUE)
  symMat <- matrix(rep(2,length(y)*length(y)),ncol=length(y))
  for (i in 1:length(y)){
    symMat[i,] <- append(output[i,],0,after=i-1)
  }
  return(symMat)
}

