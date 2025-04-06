# Generate the valid significant level grid values

Alpha <- function(r, N3){
  n_dim <- length(r)
  if(n_dim == 2){
    a <- 1:100/4000.001
    alpha_tol <- as.matrix(expand.grid(alpha1=a))
  }
  if(n_dim == 3){
    a <- 1:100/4000.001
    alpha_tol <- as.matrix(expand.grid(alpha1=a,alpha2=a))
  }
  if(n_dim==4){
    a <- 1:30/1200.001
    alpha_tol <- as.matrix(expand.grid(alpha1=a,alpha2=a, alpha3=a))
  }
  if(n_dim==5){
    a <- 1:18/720.001
    alpha_tol <- as.matrix(expand.grid(alpha1=a,alpha2=a, alpha3=a, alpha4=a))
  }
  
  alpha_1n <- split(alpha_tol, row(alpha_tol))
  clnum <- parallel::detectCores()
  mc <- getOption("mc.cores", clnum)
  alpha <- parallel::mclapply(alpha_1n,alpha_kernel,r, sig.lv=0.025 ,mc.cores = mc)
  alpha_tol <- cbind(alpha_tol, rep(0,(length(a))^(n_dim-1)))
  for(i in 1:(length(a))^(n_dim-1)){
    if (is.null(alpha[[i]])){
      alpha_tol[i,] <- rep(1,n_dim)
    }
    else{
      alpha_tol[i,n_dim] <- alpha[[i]]
      
    }
  }
  index <- which(alpha_tol==rep(1,n_dim))
  if(!setequal(which(alpha_tol==rep(1,n_dim)), integer(0))){
    alpha_tol <- alpha_tol[-index,]
  }#for 2-dimensional case there is no 1 vector
  if(nrow(alpha_tol)>=N3){
    alpha <- alpha_tol[sample(1:nrow(alpha_tol),N3),]
  }
  return(alpha)
} 

#' Point estimator for the power value
#' @description This function is to estimate the power values given fixed proportion r for each sub-population, which we utilize Monte Carlo method and GPU accelerator to estimate the power value. The user can specify the standard deviation and harzard reduction for each sub-population as the prior information of harzard reduction distribution, when not specified, we apply a default setting of linear harzard reduction scheme and the sd for each sub-population is inversely proportional to sqrt(r_i)
#'
#' @param r vector for the proportion for each sub-population, r_1is 1, r_i>r_{i+1}
#' @param N1 integer, which is fixed as 10240 in our package
#' @param N2 integer, which is fixed as 20480 in our package
#' @param N3 integer, the number of grid point for the sig.lv, which should be the multiples of 5, because we apply 5 stream parallel
#' @param E integer, the total number of events for the Phase 3 clinical trail, if not specified, then an estimation will be applied
#' @param sig the vector of standard deviation of each sub-population 
#' @param sd_full a numeric number, which denotes the prior information of standard deviation for the harzard reduction. If sig is not specified, then sd_full must has an input value to define the standard deviation of the full population
#' @param delta vector, the point estimation of harzard reduction in prior information, if not specified we apply a linear scheme by giving bound to the linear harzard reduction 
#' @param delta_linear_bd vector of length 2, specifying the upper bound and lower bound for the harzard reduction; if user don't specify the delta for each sub-population, then the linear scheme will apply and the input is a must. 
#' @param seed integer,  seed for random number generation
#' @details We interface python by reticulate package to utilize numba(cuda version) module to accelerate calculation. 
#' @return list of 2 parts of the sampling points given specific r; alpha is the matrix as each row is the given sig.lv for each population; power is the corresponding power values given each row of the alpha


phat <- function(r,N1,N2,N3,E=NULL,sig=NULL,sd_full,delta=NULL,delta_linear_bd,seed=NULL){
  n_dim <- length(r)
  rr <- base::sqrt(r)
  mat <- rr%*%(1/t(rr))
  mat[upper.tri(mat)]<- t(mat)[upper.tri(mat)]
  diag(mat) <- rep(1, n_dim)
  sigma1 <- mat
  #verify n_dim == length(sig)==length(delta)
  if((!is.null(sig))){
    if((n_dim!=length(sig))){
      stop("Length of sig not coincides with the dimension!")
    }
  }
  if((!is.null(delta))){
    if(n_dim!=length(delta)){
      stop("Length of delta not coincides with the dimension!")
    }
  }
  if(is.null(sig)){
    sig <- sd_full*(1/rr)
  }
  # If user dont input the delta for each population, then the default setting is linear
  if(is.null(delta)){
    if(delta_linear_bd[2]>=delta_linear_bd[1]){
      delta <- delta_linear_bd[2]-(delta_linear_bd[2]-delta_linear_bd[1])*r
    }
    else{
      stop("Input error of upper bound and lower bound of linear biomarker effect!")
    }
  }
  sigma2 <- diag(sig)%*%sigma1%*%diag(sig)
  
  
  # overall drug effect = lower_bio_eff
  # calculate the mean for the drug effect in each subset
  mean2 <- -base::log(1-delta)
  #generate random vectors for sampling
  if(is.null(seed)) set.seed(205851) else set.seed(seed) #  for weak 76605863
  R1 <- mnormt::rmnorm(n=N1,mean=rep(0,n_dim), varcov=sigma1)
  R2 <- mnormt::rmnorm(n=N2, mean=mean2, varcov=sigma2)
  #call power in power4R.py and calculate the N power values
  alpha <- Alpha(r,N3=N3)
  # If user denote the number of events, then the information units in the algorithm should be E/4, 
  # else we estimate it by the following 
  if(is.null(E)) It <- (stats::qnorm(0.975)+stats::qnorm(0.9))^2/base::log(1-delta[1])^2 else It <- E/4
  a <- r
  if(exists("Power_sampling", where = parent.env(environment()))){ # check whether the Power_sampling exists in the parent envir
    Power_sampling <- get("Power_sampling",envir = parent.env(environment()), mode = "function")
    pp <- Power_sampling(R1,R2,a,It,alpha)
  }
  else{
    reticulate::source_python(system.file("python","power4R.py",package="DesignCTPB"), envir =environment(), convert = TRUE)
    pp <- Power_sampling(R1,R2,a,It,alpha)

  }
  
  return(list(alpha=alpha, power=pp))
}
