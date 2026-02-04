simpars <- function(x0,z,n){
  l <- length(z) ## number of loci
  matt <- p0 <- matrix(0,3,l) ## allocate (P(x0=0|x+), P(x0=1|x+), P(x0=2|x+)) placeholders for l loci
  var_t <- 0
  kappa <- to <- rep(0,l) ## kappa and t_obs (also l loci)
  low <- hi <- 0 ## count if we always 'hit' the lowest or highest score on loci (boundary issue)
  for(i in 1:l){
      p <- dist_x0_cond_x(n[i],z[i]) ## hypergeometric dist at locus i
      p0[,i] <- p ## save P(x0={0,1,2} | x+ = x0 + x1)
      zx <- z[i]-0:2 ## x1 = x+ - x0
      t <- xlx(zx)+xlx(n[i]-zx)-((0:2)==1)*2*log(2) ## the score contributions at current locus ('stat' in sim.R)
      t <- t-sum(p*t) ## Substract the mean 
      vt <- sum(t^2*p) ## Compute the variance
      matt[,i] <- t ## store the (centered) score - centered for numerical stability
      var_t <- var_t + vt ## Store the variance
      to[i] <- t[x0[i]+1] ## extract the score contribution for the given locus
      low <- low+(to[i]==min(t)) ## add to 'low' if observed score equals the minimum
      hi <- hi+(to[i]==max(t)) ## add to 'hi' if observed score equals the maximum
      }
  logl <- function(th){ ## log likelihood. function of theta (th)
      l <- length(z) ## number of loci
      kappa <- rep(0,l) ## placeholder 
      for(i in 1:l) kappa[i] <- log(sum(exp(th*matt[,i]/sqrt(var_t))*p0[,i])) # kappa_i = log( sum( exp(theta*{centered score}_i)*P0(x0 = i) ) )
      th*sum(to)/sqrt(var_t)-sum(kappa) ## likelihood equation: theta * t_obs - kappa_+  where kappa_+ = sum(kappa_i)
  }
  logl_ <- Vectorize(logl, "th")
  hi <- hi==l ## is all loci on 'hi'? (BOOL): if TRUE the p-value is zero (0)
  low <- low==l ## is all loci on low? (BOOL): if TRUE the p-value is one (1)
  psim <- w <- matrix(1,3,l) ## construct placeholders for simulations
  theta <- NULL
  if(!hi & !low){ ## If neither hi nor low is TRUE make simulations...
    # i <- 1
    # ii <- (-i):i
    # lld <- diff(sign(diff(logl_(ii))))
    # ## browser()
    # while(max(logl(i),logl(-i)) < 0){ 
    #   i <- i + 1 ## if() break ## max search range for theta: [-100, 100]
    #   ## if(i > 100) browser()
    #   if(i==100){
    #     if(brow) browser()
    #     stop("problems with loglik - pval=0")
    #   }
    # }
    i <- 100
    theta <- suppressWarnings(optimize(logl,c(-i,i),maximum=T)$m) ## find theta that equates expected with observed score
    for(i in 1:l) { ## this is actually kappa[i], but by substracting for each [i] below it becomes kappa = sum(kappa[i])
        kappa <- log(sum(exp(theta*matt[,i]/sqrt(var_t))*p0[,i,drop=FALSE])) ## Computate the cummulant transform
        psim[,i] <- exp(theta*matt[,i]/sqrt(var_t)-kappa)*p0[,i,drop=FALSE] ## the P_theta(x) density
    }
  }
  ## browser()
  v <- sum(apply(p0*matt^2,2,sum)) ## The variance of the score
  ## print(v); print(var_t)
  tobs <- sum(to) ## The observed score
  list(matt=matt,var_t=var_t,psim=psim,wei=p0/psim,tobs=tobs/sqrt(var_t), theta = theta,p0 = p0,
       ## wei are the important weights; P_0(x)/P_\theta(x)
       hi=hi,low=low,pnval=pnorm(tobs/sqrt(v),lower.tail=FALSE)) ## pnval = pvalue based on normal
}

#' P-values from Importing Sampling using Exponential tilting
#' 
#' @param x0 Allele count of profile
#' @param x1 Population allele count
#' @param n Sampled alleles in total in population
#' @param p_limit Upper limit to which we use the normal approximation
#' @param B An integer specifying the number of importance samples.
#' @param return_all Default is FALSE. If TRUE: Returns p-value, standard deviation, and method (and diagnostics).
#' @export
#' @return If \code{return_all= FALSE} the p-value is returned. Otherwise list of elements (see \code{return_all}) is returned.
#' @details The method of importance sampling described in Tvedebrink et al (2018), Section 2.3 is implemented.
#' It relies on exponential tilting of the proposal distribution using in the importance sampling.

exponent_tilt <- function(x0, x1, n, p_limit = 0.1, B = 500, return_all = FALSE){
  zs <- x0+x1 ## sufficient stat under H0
  l <- length(zs) ## L: number of loci
  h <- simpars(x0,zs,n) ## call function above with profile, suff stat and n (number of typed alleles)
  if(h$low) result <- list(pval=1, sd = 0, method="Exact") ## if low==TRUE the score is lowest possible
  if(h$hi) result <- list(pval=0, sd = 0, method="Exact") ## if hi == TRUE, the score is the highest possible
  if(h$pnval>p_limit) result <- list(pval=h$pnval,method="Normal approximation")
  if(any(c(h$low, h$hi, h$pnval>p_limit))){
    if(return_all) return(result)
    else return(result$pval)
  }
  ## If approximate p-value is above 0.1 (default) return p-value from normal approximation
  stat0 <- h$tobs ## obs score
  krit <- 0 ## set up number of critical events
  ws <- rep.int(NA, B) ## placeholder for accepted importance weights
  stats_imp <- rep.int(NA, B)
  stats_mc <- rep.int(NA, B)
  for(b in 1:B){ ## loop... make 'B' important samples
    w <- 1 ## Initial weight
    stat <- 0 ## Initial score
    stat_ <- 0
    ## psim = P_\theta. Draw one obs from each locus using this distribution
    y0 <- apply(h$psim,2,function(x) sample(x = 0:2, size = 1, replace = F, prob = x)) ## 
    Y0 <- apply(h$p0,2,function(x) sample(x = 0:2, size = 1, replace = F, prob = x)) ## 
    for(j in 1:l){
        i <- y0[j]+1 ## extract y0 at locus j (add one to exact weight below)
        ii <- Y0[j]+1 ## extract y0 at locus j (add one to exact weight below)
        w <- w*h$w[i,j] ## Update the weight according to the important weights computed in above fct.
        stat <- stat+h$matt[i,j]/sqrt(h$var_t) ## Accumulate the score
        stat_ <- stat_+h$matt[ii,j]/sqrt(h$var_t) ## Accumulate the score
    }
    stats_imp[b] <- stat
    stats_mc[b] <- stat_
    ws[b] <- (stat0<=stat)*w ## If the score is greater than t(obs) resurn 0, else return weight.
  } ## return the p-value as the mean of the samples.
  result <- list(pval=mean(ws),sd=sd(ws)/sqrt(B),method="Importance sampling", 
                 theta = h$theta, weights = ws, stats_mc = stats_mc, stats_imp = stats_imp) 
  if(return_all) return(result)
  result$pval
}    
