

# Main dispatching function RDS estimation 
Estimate.b.k<- function (rds.object, type='mle', jack.control=NULL) {
  
  ### Initializing:
  result <- rds.object$estimates
  imput.ind <- which(result$convergence==1)
  
  
  # Vanilla MLE estimation:
  if(type=='mle'){
    result <- estimate.b.k(rds.object = rds.object, type='mle')  
  }
  
  # Integrated maximum likelihood
  else if(type=='integrated'){
    result <- estimate.b.k(rds.object = rds.object, type='integrated')  
  }
  
  # Impute using observed degrees
  else if(type=='observed'){
    if (length(rds.object$estimates)==0) {
      stop('Initial estimates required in the rds-objcet for this type of estimation.')
    }
    result$Nk.estimates[imput.ind] <- result$n.k.counts[imput.ind]
  }
  
  # Regulirize using Jeffrey's prior
  else if(type=='jeffreys'){
    result <- estimate.b.k(rds.object = rds.object, type='jeffreys')
  }
  
  # Parametric smoothing using beta[k] = beta*theta^k:
  else if(type=='parametric'){
    if (length(rds.object$estimates)==0) stop('Initial estimates in the rds-objcet for this type of estimation.')
    
    imput.ind <- which(!is.na(result$log.bk.estimates))# estimates to impute:
    result$Nk.estimates[imput.ind] <- thetaSmoothingNks(rds.object)
  }
  
  # Impute using a naive rescaling heuristic
  else if(type=='rescaling'){
    if (length(rds.object$estimates)==0) {
      stop('Initial estimates required in the rds-object for this type of estimation.')
    }
    
    result$Nk.estimates <- imputeEstimates(result$Nk.estimates, result$n.k.counts, result$convergence)
  }
  
  # delete-d resampling:
  else if(type=='leave-d-out'){
    ## Sketch:
    # delete a random subset of d observations (not necesarily from missing degree!)
    # repeat process B times.
    # return the degree-wise median of the converging repeats
    
    ## Verifications
    if(is.null(result)) stop('Initial estimates required in the rds-object for this type of estimation.')
    stopifnot(class(jack.control)=='jack.control')
    
    ## Initialization
    message('If resampling takes too long, try reducing B, or write the author for a parallelized version.')
    d <- jack.control$d # number of observation to drop
    n.deletions <- jack.control$B # number of repeats
    N <- length(rds.object$I.t)
    stopifnot(d<N) # verify number of deletion smaller than sample size
    imput.ind <- !is.na(result$convergence)
    n.nks <- sum(imput.ind)
    
    ## Core
    jack.Nks <- matrix(NA, nrow=n.deletions, ncol=n.nks)
    for(i in 1:n.deletions){
      try({
        deletion <- sample(N, d) # select arrivals to remove
        .temp <- estimate.b.k(rds.object, delete.ind = deletion, type=type)
        jack.Nks[i,] <- .temp$Nk.estimates[imput.ind]
      })
    }
    
    # Compute degree-wise median
    clean.median <- function(x) median(x[x<Inf], na.rm=TRUE) # return median of non Inf
    result$Nk.estimates[imput.ind] <- apply(jack.Nks, 2, clean.median)
  }
  
  else{
    warning('Invalid re-estimation method.')
  }
  
  
  # Packing output:
  result$likelihood <- likelihood(log.bk = result$log.bk.estimates, 
                                  Nk.estimates = result$Nk.estimates, 
                                  I.t = rds.object$I.t, 
                                  n.k.counts = result$n.k.counts, 
                                  degree.in = rds.object$degree.in , 
                                  degree.out = rds.object$degree.out , 
                                  arrival.intervals = result$arrival.intervals, 
                                  arrival.degree = result$arrival.degree)
  
  result$Nk.estimates <- ceiling(result$Nk.estimates)
  rds.object$estimates <- result
  return(rds.object)    					
}
## Testing:
# data(brazil)
# rds.object2<- initializeRdsObject(brazil)
# # Estimate:
# rds.object.3 <- Estimate.b.k(rds.object = rds.object2 )





# estimate beta_k from sampled degrees and snowball matrix:
estimate.b.k<- function (rds.object,
                         delete.ind=NULL, 
                         type) {
  ### Sketch:
  # Generate estimable parameters vector. Optimized parameter-wise for efficiency.
  # rds.object: self explanatory.
  # delete.ind: the indexes of observations to be deleted. 
  # type: if imputation is part of the estimation. 
  
  
  ### Initialize:
  arrival.times <- formatArrivalTimes(rds.object$rds.sample$interviewDt)
  arrival.intervals <- diff(arrival.times)
  
  arrival.degree<- rds.object$rds.sample$NS1
  max.observed.degree<- max(arrival.degree)
  ## TODO: should the degree of the first (seed) be removed?
  degree.counts<- table(arrival.degree)
  # Sequences per degree
  I.t <- rds.object$I.t
  degree.in <- rds.object$degree.in
  degree.out <- rds.object$degree.out
  likelihood <- NA
  
  ### Preliminaries:
  Nk.estimates<- rep(9999L, max.observed.degree) 
  names(Nk.estimates)<- seq_len(max.observed.degree)
  log.bk.estiamtes<- rep(NA, max.observed.degree) 
  A.ks<- rep(NA, max.observed.degree) 
  B.ks<- rep(NA, max.observed.degree) 
  n.k.counts<- rep(NA, max.observed.degree)
  convergence<- rep(NA, max.observed.degree)
  
  # If jacknifing:
  jack.indicators <- rep(1, length(arrival.intervals))
  if(!is.null(delete.ind)) jack.indicators[delete.ind] <- 0
  
  A.k <- sum( jack.indicators * head(I.t,-1) * arrival.intervals, na.rm=TRUE)
  
  uniques<- as.integer(names(degree.counts))
  Nk.estimates[-uniques]<- 0
  
  for(k in uniques){
    k.ind <- arrival.degree==k
    k.ind[1] <- FALSE # dealing with sample kickoff
    
    n.k <- cumsum((arrival.degree==k))
    n.k.count <- degree.counts[paste(k)]
    B.k <- sum( jack.indicators * head(I.t,-1) * arrival.intervals * head(n.k,-1), na.rm=TRUE)    
    
    # Root finding
    .temp <- estimate.b.k.2(k=k, 
                            A.k=A.k, 
                            B.k=B.k, 
                            n.k=n.k, 
                            n.k.count= n.k.count, 
                            k.ind=k.ind, 
                            delete.ind = delete.ind,
                            type=type)    
    
    convergence[k] <- .temp$converge
    A.ks[k] <- A.k
    B.ks[k] <- B.k
    n.k.counts[k] <- n.k.count
    
    Nk.estimates[k]<- .temp$N.k
    log.bk.estiamtes[k] <- log(n.k.count) - log(.temp$N.k *  A.k - B.k)
  } # End looping over estimable degrees.  
  
  
  # Compute likelihood at converged estimates.
  likelihood.val <- likelihood(log.bk = log.bk.estiamtes, 
                               Nk.estimates = Nk.estimates, 
                               I.t = I.t, 
                               n.k.counts = n.k.counts, 
                               degree.in = degree.in , 
                               degree.out = degree.out , 
                               arrival.intervals = arrival.intervals, 
                               arrival.degree = arrival.degree)
  
  # Pack output
  result<- list(
    call=sys.call(),
    Nk.estimates=Nk.estimates, 
    log.bk.estimates=log.bk.estiamtes,
    A.ks=A.ks,
    B.ks=B.ks,
    n.k.counts=n.k.counts,
    arrival.intervals=arrival.intervals,
    arrival.degree=arrival.degree, 
    convergence=convergence,
    likelihood=likelihood.val)
  return(result)    					
}
## Testing:
# See 







estimate.b.k.2 <- function(k, A.k, B.k, n.k, n.k.count, k.ind, delete.ind, type){
  ## Initialize:
  result <- list(N.k=NA, converge=FALSE)
  N.k <- NA
  type.switch <- FALSE
  
  # Conditional execution
  if(type=='integrated'){
    type.switch <- 'integrated'
    type <- 'mle'
  }
  
  if(type=='mle'){
    if(!is.null(delete.ind)) message('MLE estimation selected. Ignoring Delete-d.')
    
    target <- function(N.k){
      const1 <- N.k - (B.k/A.k)
      pre.const2 <- (N.k - seq(0, n.k.count-1))
      # Deal with impossible N.k:
      if(any(pre.const2 < 0)) return(-Inf)
      const2 <- sum(1/pre.const2)
      const1 * const2 - n.k.count
    }
  } 
  
  else if(type=='leave-d-out'){
    if(is.null(delete.ind)) warning('Deletion indexes not specified!')
    
    target <- function(N.k){
      const1 <- N.k - (B.k/A.k)
      pre.const2 <- (N.k - seq(0, n.k.count-1))
      if(any(pre.const2 < 0)) return(-Inf) # Deal with impossible N.k
      const2 <- sum(1/pre.const2)
      const2 <- const2 - sum(1/(N.k-delete.ind+1))
      n.k.count <- n.k.count-length(delete.ind)
      
      const1 * const2 - n.k.count
    }
  } 
  
  else if(type=='jeffreys'){
    # Construct target function:
    target <- function(N.k){
      const1 <- N.k-(B.k/A.k)
      # pre.const2 <- (N.k - n.k[which(k.ind)-1])
      pre.const2 <- (N.k - seq(0, n.k.count-1))
      if(any(pre.const2<0)) return(-Inf) # Deal with impossible N.k:
      
      const2.1 <- sum(1/pre.const2^3)
      const2.2 <- sum(1/pre.const2^2)
      const2.3 <- sum(1/pre.const2)
      const2 <- const2.3 - const2.1/const2.2
      const1*const2 - n.k.count
    }
  }
  
  # Re-estimate if not converged
  else if(type=='integrated2'){
    # Construct target function: 
    ## Yakir target:  sum(((N.k-n+1):N.k)^-1) - (n+1)*A/(N.k*A-B)
    
    target <- function(N.k){
      pre.const2 <- (N.k - seq(0, n.k.count-1))
      if(any(pre.const2<0)) return(-Inf) # Deal with impossible N.k:
      const2 <- sum(1/pre.const2)
      const1 <- (n.k.count+1)*A.k/(N.k*A.k-B.k)
      const2- const1
    }
  }
  
  
  # Finding root of target:
  roots <- NULL
  .interval <- n.k.count * c(1, 10*max(1,1/A.k))
  try(
    roots <- uniroot(f = target, interval =.interval, extendInt = "no", maxiter = 1e4), 
    silent = TRUE
    )
  
  # Indicate type of convergence:
  if(length(roots)>0) {
    result$N.k <- roots$root
    result$converge <- 0 # 0 marks succeful convergence!
  } 
  else if(sign(target(.interval[2]))>0) {
    result$N.k <- Inf 
    result$converge <- 1
  }
  else if(sign(target(.interval[2]))<0) {
    result$N.k <- n.k.count
    result$converge <- -1
  }
  else warning('Could not compute estimator nor gradient. Returning NA.')
  
  
  
  # Re estimate if not converged:
  if(result$converge==1 && type.switch=='integrated'){
      result <- Recall(k=k, 
                     A.k=A.k, 
                     B.k=B.k, 
                     n.k=n.k, 
                     n.k.count= n.k.count, 
                     k.ind=k.ind, 
                     delete.ind = delete.ind,
                     type='integrated2')
  }
  
  return(result)
}
## Testing:
# save(n.k, k.ind,A.k, B.k, file='temp/testing_setup.RData')
# load(file='temp/testing_setup.RData')
# chords:::estimate.b.k.2(k = 3, A.k = A.k, B.k = B.k, n.k =n.k, n.k.count = n.k.count, delete.ind = NULL, type='mle' )














