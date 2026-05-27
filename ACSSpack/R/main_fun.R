# <R package ACSSpack, providing ACSS, Corresponding ACSS, and GLP Algorithm>
#       Copyright (C) <2024>  <Ziqian Yang>

#' @importFrom stats rbeta rgamma rnorm runif var
NULL

#' ACSS algorithm
#' 
#' @description
#' Adaptive Correlated  Spike and Slab (ACSS) algorithm with/without adaptive burn-in Gibbs sampler. See paper of Yang, Z., Khare, K., & Michailidis, G. (2024) for details.
#' @export
#' @param Y A (centered) vector.
#' @param X A (centered in column) matrix.
#' @param para Parameter pre-set. Options include "1sqrtp" (default), "11", "4sqrtp", "Unif", and "customize". See details below.
#' \itemize{
#'  \item "1sqrtp": a=1, b=1, c=1, s=sqrt(p) (default)
#'  \item "11": a=1, b=1, c=1, s=1
#'  \item "4sqrtp": a=1, b=1, c=4, s=sqrt(p)
#'  \item "Unif": a=1, b=1, c=0.8, s=p/1.2
#'  \item "customize": user-defined a,b,c and s. If choose this option, please provide values for a,b,c and s.
#' }
#' If no parameter pre-set is provided, the default "1sqrtp" will be used.
#' @param a shape parameter for marginal of q; default=1.
#' @param b shape parameter for marginal of q; default=1.
#' @param c shape parameter for marginal of lambda^2; larger c introduce more shrinkage and stronger correlation. default=1.
#' @param s scale (inversed) parameter for marginal of lambda^2; larger s introduce more shrinkage; default=sqrt(p).
#' @param Max_burnin Maximum burn-in (in 100 steps) for adaptive burn-in Gibbs sampler. Minimum value is 10, corresponding to 1000 hard burn-insteps. Default=10.
#' @param nmc Number of MCMC samples. Default=5000.
#' @param adaptive_burn_in Logical. If TRUE, use adaptive burn-in Gibbs sampler; If false, use fixed burn-in with burn-in = Max_burnin. Default=TRUE.
#' @returns A list with betahat: predicted beta hat from majority voting, and Gibbs_res: 5000 samples of beta, q and lambda^2 from Gibbs sampler.
#' @examples
#' ## A toy example is given below to save time. The full example can be run to get better results
#' ## by letting nmc=5000 (default).
#' 
#' n = 30;
#' p = n;
#' 
#' beta1 = rep(0.1, p);
#' beta2 = c(rep(0.2, p / 2), rep(0, p / 2));
#' beta3 = c(rep(0.15, 3 * p / 4), rep(0, ceiling(p / 4)));
#' beta4 = c(rep(1, p / 4), rep(0, ceiling(3 * p / 4)));
#' beta5 = c(rep(3, ceiling(p / 20)), rep(0 , 19 * p / 20));
#' betas = list(beta1, beta3, beta2, beta4, beta5);
#' 
#' set.seed(123);
#' X = matrix(rnorm(n * p), n, p);
#' Y = c(X %*% betas[[1]] + rnorm(n));
#' 
#' ## A toy example with p=30, total Gibbs steps=1100, takes ~0.6s
#' system.time({mod = ACSS_gs(Y, X, para = "1sqrtp", nmc = 100);})
#' 
#' mod$beta; ## estimated beta after the Majority voting
#' hist(mod$Gibbs_res$betamat[1,]); ## histogram of the beta_1
#' hist(mod$Gibbs_res$q); ## histogram of the q
#' hist(log(mod$Gibbs_res$lambdasq)); ## histogram of the log(lambda^2)
#' hist(mod$Gibbs_res$R_2); ## histogram of the R^2 for each iteration in Gibbs sampler
#' plot(mod$Gibbs_res$q, type = "l"); ## trace plot of the q
#' ## joint posterior of model density and shrinkage
#' plot(log(mod$Gibbs_res$q / (1 - mod$Gibbs_res$q)), -log(mod$Gibbs_res$lambdasq),
#'     xlab = "logit(q)", ylab = "-log(lambda^2)",
#'     main = "Joint Posterior of Model Density and Shrinkage"); 
ACSS_gs <- function(Y, X, para=c("1sqrtp","11","4sqrtp","Unif","customize"), a=1, b=1, c=1, s=NA, Max_burnin=10, nmc=5000, adaptive_burn_in=TRUE) {
  if(missing(para)) {
    para="1sqrtp"
    print("No parameter pre-set provided. Use default parameter pre-set 'ACSS(1,1,1,sqrt p)'. If want to customize parameters, please set 'para' as 'customize' and provide values for a,b,c and s.")
  }
  if(!(para %in% c("1sqrtp","11","4sqrtp","Unif","customize"))) stop("Please provide a valid parameter pre-set. If want to customize parameters, please set 'para' as 'customize' and provide values for a,b,c and s.")
  
  if(para=="customize" & (is.na(s) | s<=0)) stop("Please provide a valid customize s value.")
  if(para=="1sqrtp") {a=1; b=1; c=1; s=sqrt(ncol(X))}
  else if(para=="11") {a=1; b=1; c=1; s=1}
  else if(para=="Unif") {a=1; b=1; c=0.8; s=ncol(X)/1.2}
  else if(para=="4sqrtp") {a=1; b=1; c=4; s=sqrt(ncol(X))}
  
  if(Max_burnin<10) stop("Max_burnin should be at least 10")
  if(length(Y)!=length(X[,1])) stop("The dimension of Y and X should fit.")
  if(length(Y)<10) stop("The sample size should be at least 10 to let initialization work.")
  if(max(abs(colMeans(X)),abs(mean(Y)))>1e-2) print("The model considered here assumes zero intercept. Center Y and X if needed.")
  print(paste0("Running ACSS(",a,",",b,",",c,",",round(s,2),") ..."))
  paralist=list(a=a, b=b, c=c, s=s)
  if(adaptive_burn_in) q_thresh=0.1 
  else q_thresh=Inf
  res=Gib_samp_adap_burnin(method=ACSS,Y=Y, X=X, paralist=paralist, MAX_STEPS=Max_burnin-9, nmc=nmc, q_thresh=q_thresh,sampleR2=TRUE)
  betahat=Maj_Vot(res,0.5)
  return(list(betahat=betahat, Gibbs_res=res))
}

#' INSS algorithm
#' 
#' @description
#' INdependent Spike and Slab (INSS) algorithm with/without adaptive burn-in Gibbs sampler. See paper of Yang, Z., Khare, K., & Michailidis, G. (2024) for details.
#' @export
#' @param Y A (centered) vector.
#' @param X A (centered in column) matrix.
#' @param para Parameter pre-set. Options include "1sqrtp" (default), "11", "4sqrtp", "Unif", and "customize". See details below.
#' \itemize{
#'  \item "1sqrtp": a=1, b=1, c=1, s=sqrt(p) (default)
#'  \item "11": a=1, b=1, c=1, s=1
#'  \item "4sqrtp": a=1, b=1, c=4, s=sqrt(p)
#'  \item "Unif": a=1, b=1, c=0.8, s=p/1.2
#'  \item "customize": user-defined a,b,c and s. If choose this option, please provide values for a,b,c and s.
#' }
#' If no parameter pre-set is provided, the default "1sqrtp" will be used.
#' @param a shape parameter for marginal of q; default=1.
#' @param b shape parameter for marginal of q; default=1.
#' @param c shape parameter for marginal of lambda^2; larger c introduce more shrinkage and stronger correlation. default=1.
#' @param s scale (inversed) parameter for marginal of lambda^2; larger s introduce more shrinkage; default=sqrt(p).
#' @param Max_burnin Maximum burn-in (in 100 steps) for adaptive burn-in Gibbs sampler. Minimum value is 10, corresponding to 1000 hard burn-insteps. Default=10.
#' @param nmc Number of MCMC samples. Default=5000.
#' @param adaptive_burn_in Logical. If TRUE, use adaptive burn-in Gibbs sampler; If false, use fixed burn-in with burn-in = Max_burnin. Default=TRUE.
#' @returns A list with betahat: predicted beta hat from majority voting, and Gibbs_res: 5000 samples of beta, q and lambda^2 from Gibbs sampler.
#' @examples
#' ## A toy example is given below to save time. The full example can be run to get better results
#' ## by letting nmc=5000 (default).
#' 
#' n = 30;
#' p = n;
#' 
#' beta1 = rep(0.1, p);
#' beta2 = c(rep(0.2, p / 2), rep(0, p / 2));
#' beta3 = c(rep(0.15, 3 * p / 4), rep(0, ceiling(p / 4)));
#' beta4 = c(rep(1, p / 4), rep(0, ceiling(3 * p / 4)));
#' beta5 = c(rep(3, ceiling(p / 20)), rep(0 , 19 * p / 20));
#' betas = list(beta1, beta3, beta2, beta4, beta5);
#' 
#' set.seed(123);
#' X = matrix(rnorm(n * p), n, p);
#' Y = c(X %*% betas[[1]] + rnorm(n));
#' 
#' ## A toy example with p=30, total Gibbs steps=1100, takes ~0.6s
#' system.time({mod = INSS_gs(Y, X, para = "1sqrtp", nmc = 100);})
#' 
#' mod$beta; ## estimated beta after the Majority voting
#' hist(mod$Gibbs_res$betamat[1,]); ## histogram of the beta_1
#' hist(mod$Gibbs_res$q); ## histogram of the q
#' hist(log(mod$Gibbs_res$lambdasq)); ## histogram of the log(lambda^2)
#' hist(mod$Gibbs_res$R_2); ## histogram of the R^2 for each iteration in Gibbs sampler
#' plot(mod$Gibbs_res$q, type = "l"); ## trace plot of the q
#' ## joint posterior of model density and shrinkage
#' plot(log(mod$Gibbs_res$q / (1 - mod$Gibbs_res$q)), -log(mod$Gibbs_res$lambdasq),
#'     xlab = "logit(q)", ylab = "-log(lambda^2)",
#'     main = "Joint Posterior of Model Density and Shrinkage"); 
INSS_gs <- function(Y, X, para=c("1sqrtp","11","4sqrtp","Unif","customize"), a=1, b=1, c=1, s=NA, Max_burnin=10, nmc=5000, adaptive_burn_in=TRUE) {
  if(missing(para)) {
    para="1sqrtp"
    print("No parameter pre-set provided. Use default parameter pre-set 'INSS(1,1,1,sqrt p)'. If want to customize parameters, please set 'para' as 'customize' and provide values for a,b,c and s.")
  }
  if(!(para %in% c("1sqrtp","11","4sqrtp","Unif","customize"))) stop("Please provide a valid parameter pre-set. If want to customize parameters, please set 'para' as 'customize' and provide values for a,b,c and s.")
  
  if(para=="customize" & (is.na(s) | s<=0)) stop("Please provide a valid customize s value.")
  if(para=="1sqrtp") {a=1; b=1; c=1; s=sqrt(ncol(X))}
  else if(para=="11") {a=1; b=1; c=1; s=1}
  else if(para=="Unif") {a=1; b=1; c=0.8; s=ncol(X)/1.2}
  else if(para=="4sqrtp") {a=1; b=1; c=4; s=sqrt(ncol(X))}
  
  if(Max_burnin<10) stop("Max_burnin should be at least 10")
  if(length(Y)!=length(X[,1])) stop("The dimension of Y and X should fit.")
  if(length(Y)<10) stop("The sample size should be at least 10 to let initialization work.")
  if(max(abs(colMeans(X)),abs(mean(Y)))>1e-2) print("The model considered here assumes zero intercept. Center Y and X if needed.")
  print(paste0("Running INSS(",a,",",b,",",c,",",round(s,2),") ..."))
  paralist=list(a=a, b=b, c=c, s=s)
  if(adaptive_burn_in) q_thresh=0.1 
  else q_thresh=Inf
  res=Gib_samp_adap_burnin(method=INSS,Y=Y, X=X, paralist=paralist, MAX_STEPS=Max_burnin-9, nmc=nmc, q_thresh=q_thresh,sampleR2=TRUE)
  betahat=Maj_Vot(res,0.5)
  return(list(betahat=betahat, Gibbs_res=res))
}

#' GLP algorithm
#' 
#' @description
#' Giannone, Lenza and Primiceri (GLP) algorithm with/without adaptive burn-in Gibbs sampler. See paper Giannone, D., Lenza, M., & Primiceri, G. E. (2021) and Yang, Z., Khare, K., & Michailidis, G. (2024) for details.
#' 
#' Most of the codes are from https://github.com/bfava/IllusionOfIllusion with our modification to make it have adaptive burn-in Gibbs sampler, and some debugs.
#' 
#' @export
#' @useDynLib ACSSpack, .registration=TRUE
#' @param Y A (centered) vector.
#' @param X A (centered in column) matrix.
#' @param a shape parameter for marginal of q; default=1.
#' @param b shape parameter for marginal of q; default=1.
#' @param A shape parameter for marginal of R^2; default=1.
#' @param B shape parameter for marginal of R^2; default=1.
#' @param Max_burnin Maximum burn-in (in 100 steps) for adaptive burn-in Gibbs sampler. Minimum value is 10, corresponding to 1000 hard burn-insteps. Default=10.
#' @param nmc Number of MCMC samples. Default=5000.
#' @param adaptive_burn_in Logical. If TRUE, use adaptive burn-in Gibbs sampler; If false, use fixed burn-in with burn-in = Max_burnin. Default=TRUE.
#' @returns A list with betahat: predicted beta hat from majority voting, and Gibbs_res: 5000 samples of beta, q and lambda^2 from Gibbs sampler.
#' @examples
#' ## A toy example is given below to save your time, which will still take ~10s. 
#' ## The full example can be run to get BETTER results, which will take more than 80s, 
#' ## by letting nmc=5000 (default).
#' 
#' n = 30;
#' p = n;
#' 
#' beta1 = rep(0.1, p);
#' beta2 = c(rep(0.2, p / 2), rep(0, p / 2));
#' beta3 = c(rep(0.15, 3 * p / 4), rep(0, ceiling(p / 4)));
#' beta4 = c(rep(1, p / 4), rep(0, ceiling(3 * p / 4)));
#' beta5 = c(rep(3, ceiling(p / 20)), rep(0 , 19 * p / 20));
#' betas = list(beta1, beta3, beta2, beta4, beta5);
#' 
#' set.seed(123);
#' X = matrix(rnorm(n * p), n, p);
#' Y = c(X %*% betas[[1]] + rnorm(n));
#' \donttest{
#' ## A toy example with p=30, total Gibbs steps=1100
#' system.time({mod = GLP_gs(Y, X, nmc = 100);})
#' 
#' mod$beta; ## estimated beta after the Majority voting
#' hist(mod$Gibbs_res$betamat[1,]); ## histogram of the beta_1
#' hist(mod$Gibbs_res$q); ## histogram of the q
#' hist(log(mod$Gibbs_res$lambdasq)); ## histogram of the log(lambda^2)
#' hist(mod$Gibbs_res$R_2); ## histogram of the R^2 for each iteration in Gibbs sampler
#' plot(mod$Gibbs_res$q, type = "l"); ## trace plot of the q
#' ## joint posterior of model density and shrinkage
#' plot(log(mod$Gibbs_res$q / (1 - mod$Gibbs_res$q)), -log(mod$Gibbs_res$lambdasq),
#'     xlab = "logit(q)", ylab = "-log(lambda^2)",
#'     main = "Joint Posterior of Model Density and Shrinkage"); }
GLP_gs <- function(Y, X, a=1, b=1, A=1, B=1, Max_burnin=10, nmc=5000, adaptive_burn_in=TRUE) {
  if(Max_burnin<10) stop("Max_burnin should be at least 10")
  if(length(Y)!=length(X[,1])) stop("The dimension of Y and X should fit.")
  if(length(Y)<10) stop("The sample size should be at least 10 to let initialization work.")
  if(max(abs(colMeans(X)),abs(mean(Y)))>1e-2) print("The model considered here assumes zero intercept. Center Y and X if needed.")
  print(paste0("Running GLP(",a,",",b,",",A,",",B,") ..."))
  paralist=list(a=a, b=b, A=A, B=B)
  if(adaptive_burn_in) q_thresh=0.1 
  else q_thresh=Inf
  res=Gib_samp_adap_burnin(method=GLP,Y=Y, X=X, paralist=paralist, MAX_STEPS=Max_burnin-9, nmc=nmc, q_thresh=q_thresh,sampleR2=TRUE)
  betahat=Maj_Vot(res,0.5)
  return(list(betahat=betahat, Gibbs_res=res))
}
