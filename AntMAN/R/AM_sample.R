#######################################################################################
###############
############### AntMAN Package
###############
###############
#######################################################################################

library(mvtnorm)

#' AM_sample_uninorm
#'  
#' @keywords internal
#' @export
AM_sample_uninorm <- function(n,pro,mmu,ssd){
  if(length(mmu)!=length(pro)){stop()}
  if(length(ssd)!=length(pro)){stop()}
  
  y <- vector(length=n)
  ci <- vector(length=n)
  for(i in 1:n){
    u <- runif(1)
    if(u<pro[1]){
      ci[i] <- 0
      y[i] <- rnorm(1,mean=mmu[1],sd=ssd)
    }
    else{
      if(u<(pro[1]+pro[2])){
        ci[i] <- 1
        y[i] <- rnorm(1,mean=mmu[2],sd=ssd[2])
      }
      else{
        ci[i] <- 2
        y[i] <- rnorm(1,mean=mmu[3],sd=ssd[3]) 
      }
    }
  }
  return(list(y=y,ci=ci))
}


#' AM_sample_unipois
#'  
#'  
#' @keywords internal
#' @export
AM_sample_unipois <- function(n=1000,pro=c(0.2,0.5,0.3),mth=c(5,25,50)){
  if(length(mth)!=length(pro)){stop()}
  
  y <- vector(length=n)
  ci <- vector(length=n)
  for(i in 1:n){
    u <- runif(1)
    if(u<pro[1]){
      ci[i] <- 0
      y[i] <- rpois(1,lambda=mth[1])
    }
    else{
      if(u<(pro[1]+pro[2])){
        ci[i] <- 1
        y[i] <- rpois(1,lambda=mth[2])
      }
      else{
        ci[i] <- 2
        y[i] <- rpois(1,lambda=mth[3])
        
      }
    }
  }
  return(list(y=y,ci=ci))
}



# #' AM_sample_unibin
# #'  
# #'  
# #' @keywords internal
# #' @export
# AM_sample_unibin <- function(n,mb, pro,mth){
#   if(length(mth)!=length(pro)){stop()}
  
#   y <- vector(length=n)
#   ci <- vector(length=n)
#   for(i in 1:n){
#     u <- runif(1)
#     if(u<pro[1]){
#       ci[i] <- 0
#       y[i] <- rbinom(1, size=mb, prob=mth[1])
#     }
#     else{
#       if(u<(pro[1]+pro[2])){
#         ci[i] <- 1
#         y[i] <- rbinom(1, size=mb, prob=mth[2])
#       }
#       else{
#         ci[i] <- 2
#         y[i] <- rbinom(1, size=mb, prob=mth[3])
        
#       }
#     }
#   }
#   return(list(y=y,ci=ci))
# }

# #' AM_sample_multibin
# #'  
# #'  
# #' @keywords internal
# #' @export
# AM_sample_multibin <- function(n,d,pro,TH){
  
#   y <- matrix(nrow=n,ncol=d)
#   ci <- vector(length=n)
#   for(i in 1:n){
#     u <- runif(1)
#     if(u<pro[1]){
#       ci[i] <- 0
#       y[i,] <-rbinom(d, rep(1,d), TH[1,])
#     }
#     else{
#       if(u<(pro[1]+pro[2])){
#         ci[i] <- 1
#         y[i,] <-rbinom(d, rep(1,d), TH[2,])
        
#       }
#       else{
#         ci[i] <- 2
#         y[i,] <-rbinom(d, rep(1,d), TH[3,])
        
#       }
#     }
#   }
#   return(list(y=y,ci=ci))
# }

#' AM_sample_multinorm
#'  
#' @keywords internal
#' @importFrom mvtnorm rmvnorm
#' @export
AM_sample_multinorm <- function(n,d,pro,MU,SIG){
  if(dim(SIG)[1]!=length(pro)){stop()}
  y <- matrix(nrow=n,ncol=d)
  ci <- vector(length=n)
  for(i in 1:n){
    u <- runif(1)
    if(u<pro[1]){
      ci[i] <- 0
      y[i,] <-rmvnorm(1, mean = MU[1,], sigma = SIG[1,,])
    }
    else{
      if(u<(pro[1]+pro[2])){
        ci[i] <- 1
        y[i,] <- rmvnorm(1, mean = MU[2,], sigma = SIG[2,,])
        
      }
      else{
        ci[i] <- 2
        y[i,]  <- rmvnorm(1, mean = MU[3,], sigma = SIG[3,,])
        
      }
    }
  }
  return(list(y=y,ci=ci))
}



#' AM_sample_multibin
#'  
#'  
#' @keywords internal
#' @export
AM_sample_multibin <- function(n,d,pro,TH){
  
  y <- matrix(nrow=n,ncol=d)
  ci <- vector(length=n)
  for(i in 1:n){
    u <- runif(1)
    if(u<pro[1]){
      ci[i] <- 0
      y[i,] <-rbinom(d, rep(1,d), TH[1,])
    }
    else{
      if(u<(pro[1]+pro[2])){
        ci[i] <- 1
        y[i,] <-rbinom(d, rep(1,d), TH[2,])
        
      }
      else{
        ci[i] <- 2
        y[i,] <-rbinom(d, rep(1,d), TH[3,])
        
      }
    }
  }
  return(list(y=y,ci=ci))
}


