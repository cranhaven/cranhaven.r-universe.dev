#' @title Fit Cox Proportional Hazard Regression Model under dependent right censoring via MPL and Archimedean Copulas
#'
#' @description Simultaneously estimate the regression coefficients and the baseline hazard function of proportional
#' hazard Cox models under dependent right censoring using maximum penalised likelihood (MPL) and Archimedean Copulas
#' @usage coxph_mpl_dc(surv, cova, control,...)
#'
#' @param surv the outcome of survival data, with the first column X (observed time), second column
#' del (event indicator) and third column eta (dependent right censoring indicator).
#' @param cova the covariate matrix, with dimension of n rows and p columns, where 'n' is the sample size and 'p' is the number of covariates
#' Default is \code{cova=matrix(0,n,1)}, which the covariates are not considered.
#' @param control object of class \code{coxph_mpl_dc.control} specifying control options like basis choice,
#' refer to \code{coxph_mpl_dc.control} to see the defaults.
#' @param ... other arguments. In \code{coxph_mpl_dc}, these elements, will be passed to \code{\link{coxph_mpl_dc.control}}.
#'
#'
#' @details coxph_mpl_dc allows to simultaneously estimate the regression coefficients and baseline hazard function of Cox proportional hazard models,
#' with dependent and independent right censored data, by maximizing a penalized likelihood,
#' in which a penalty function is used to smooth the baseline hazard estimates.
#' Note that the dependence between event and censoring times is modelled by an Archimedean copula.
#'
#' Optimization is achieved using an iterative algorithm, which combines Newton’s method and the multiplicative iterative algorithm proposed by Ma (2010),
#' and respects the non-negativity constraints on the baseline hazard estimate (refer to Ma et al (2014) and Xu et al (2018)).
#'
#' The centered covariate matrix \eqn{Z} is used in the optimization process to get a better shaped (penalized) log-likelihood.
#' Baseline hazard parameter estimates and covariance matrix are then respectively corrected using a correction factor and the delta method.
#'
#' The estimates of zero are possible for baseline hazard parameters (e.g., when number of knots is relatively large to sample size) and
#' will correspond to active constraints as defined by Moore and Sadler (2008). Inference, as described by Ma et al (2014) or Xu et al (2018),
#' is then corrected accordingly (refer to Moore and Sadler (2008)) by adequately cutting the corresponding covariance matrix.
#'
#' There are currently three ways to perform inference on model parameters: Let \eqn{H} denote the negative of Hessian matrix of the non-penalized likelihood,
#' \eqn{Q} denote the product of the first order derivative of the penalized likelihood by its transpose,
#' and \eqn{M_2} denote the negative of the second order derivative of the penalized likelihood.
#' Then the three estimated covariance matrices for the MPL estimates are \eqn{M_{2}^{-1}}, \eqn{M_{2}^{-1}HM_{2}^{-1}} and \eqn{M_{2}^{-1}QM_{2}^{-1}}.
#'
#' Simulation studies the coverage levels of confidence intervals for the regression parameters seem to indicate
#' \eqn{M_{2}^{-1}HM_{2}^{-1}} performs better when using the piecewise constant basis,
#' and that \eqn{M_{2}^{-1}QM_{2}^{-1}}  performs better when using other bases.
#'
#'
#' @return \item{mpl_theta}{MPL estimates of the regression coefficient for the basis functions of the baseline hazard of T, i.e. theta}
#' \item{mpl_gamma}{MPL estimates of the regression coefficient for the basis functions of the baseline hazard of C, i.e. gamma}
#' \item{mpl_h0t}{MPL estimates of the baseline hazard for T, i.e. h_{0T}(x_i)}
#' \item{mpl_h0c}{MPL estimates of the baseline hazard for C, i.e. h_{0C}(x_i)}
#' \item{mpl_H0t}{MPL estimates of the baseline cumulative hazard for T, i.e. H_{0T}(x_i)}
#' \item{mpl_H0c}{MPL estimates of the baseline cumulative hazard for C, i.e. H_{0c}(x_i)}
#' \item{mpl_S0t}{MPL estimates of the baseline survival for T, i.e. S_{0T}(x_i)}
#' \item{mpl_S0c}{MPL estimates of the baseline survival for C, i.e. S_{0C}(x_i)}
#' \item{mpl_beta}{MPL estimates of beta}
#' \item{mpl_phi}{MPL estimates of phi}
#' \item{penloglik}{the penalized log-likelihood function given the MPL estimates}
#' \item{mpl_Ubeta}{the first derivative of penalized log-likelihood function with respect to beta given the MPL estimates}
#' \item{mpl_Uphi}{the first derivative of penalized log-likelihood unction with respect to phi given the MPL estimates}
#' \item{mpl_Utheta}{the first derivative of penalized log-likelihood function with respect to theta given the MPL estimates}
#' \item{mpl_Ugamma}{the first derivative of penalized log-likelihood function with respect to gamma given the MPL estimates}
#' \item{iteration}{a vector of length 3 indicating the number of iterations used to estimate the smoothing parameter
#' (first value, equal to 1 when the user specified a chosen value), the beta, phi, theta and gamma parameters during the entire process (second value),
#' and beta, phi, theta and gamma parameters during the last smoothing parameter iteration (third value)}
#' \item{mpl_cvl}{the cross validation value given the MPL estimates}
#' \item{mpl_aic}{the AIC value given the MPL estimates}
#' \item{mpl_beta_sd}{the asymptotic standard deviation of the MPL estimated beta}
#' \item{mpl_phi_sd}{the asymptotic standard deviation of the MPL estimated phi}
#' \item{mpl_h0t_sd}{the asymptotic standard deviation of the MPL estimates for the baseline hazard coefficient of T, i.e. theta}
#' \item{mpl_h0c_sd}{the asymptotic standard deviation of the MPL estimates for the baseline hazard coefficient of C, i.e. gamma}
#'
#' \item{mpl_ht0_sd}{the asymptotic standard deviation of the MPL estimates for the baseline hazard of T}
#' \item{mpl_hc0_sd}{the asymptotic standard deviation of the MPL estimates for the baseline hazard of C}
#' \item{mpl_Ht0_sd}{the asymptotic standard deviation of the MPL estimates for the cumulative baseline hazard of T}
#' \item{mpl_Hc0_sd}{the asymptotic standard deviation of the MPL estimates for the cumulative baseline hazard of C}
#' \item{mpl_St0_sd}{the asymptotic standard deviation of the MPL estimates for the baseline survival of T}
#' \item{mpl_Sc0_sd}{the asymptotic standard deviation of the MPL estimates for the baseline survival of C}
#'
#' \item{mpl_est_cov}{the asymptotic covariance matrix of the MPL estimates}
#' \item{mpl_beta_phi_zp}{the MPL estimates for regression coefficient with their corresponding standard deviations, z scores and p-values}
#' \item{binwv}{the width of each discretized bin of the observed times when piecewise constant approximation applied to the baseline hazards}
#' \item{ID}{the bin ID for each individual of the sample when piecewise constant approximation applied to the baseline hazards}
#' \item{binedg}{the edge for each discretized bin among the observed time vector X, which are the internal knots and boundaries}
#' \item{psix}{basis function matrix psi(x_i) with dimension of n by m for baseline hazard, where m=number of internal knots+ordSp}
#' \item{Psix}{basis function matrix Psi(x_i) with dimension of n by m for baseline cumulative hazard}
#' Inputs defined in coxph_mpl_dc.control
#'
#' @author Jing Xu, Jun Ma, Thomas Fung
#'
#' @references Ma, J. (2010). \emph{"Positively constrained multiplicative iterative algorithm for maximum penalised likelihood tomographic reconstruction"}.
#' IEEE Transactions On Signal Processing 57, 181-192.
#'
#' Ma, J. and Heritier, S. and Lo, S. (2014). \emph{"On the Maximum Penalised Likelihood Approach forProportional Hazard Models with Right Censored Survival Data"}.
#' Computational Statistics and Data Analysis 74, 142-156.
#'
#' Xu J, Ma J, Connors MH, Brodaty H. (2018). \emph{"Proportional hazard model estimation under dependent censoring using copulas and penalized likelihood"}.
#' Statistics in Medicine 37, 2238–2251.
#'
#'
#' @seealso \code{\link{plot.coxph_mpl_dc}}, \code{\link{coxph_mpl_dc.control}}, \code{\link{coef.coxph_mpl_dc}}
#'
#' @importFrom copula indepCopula claytonCopula gumbelCopula frankCopula iTau rCopula
#' @importFrom survival coxph Surv
#' @importFrom splines2 iSpline mSpline
#' @importFrom matrixcalc is.positive.definite
#'
#' @examples
#' \donttest{
#'  ##-- Copula types
#'  copula3 <- 'frank'
#'
#'  ##-- Marginal distribution for T, C, and A
#'  a <- 2
#'  lambda <- 2
#'  cons7 <- 0.2
#'  cons9 <- 10
#'  tau <- 0.8
#'  betas <- c(-0.5, 0.1)
#'  phis <- c(0.3, 0.2)
#'  distr.ev <- 'weibull'
#'  distr.ce <- 'exponential'
#'
#'  ##-- Sample size
#'  n <- 200
#'
#'  ##-- One sample Monte Carlo dataset
#'  cova <- cbind(rbinom(n, 1, 0.5), runif(n, min=-10, max=10))
#'  surv <- surv_data_dc(n, a, cova, lambda, betas, phis, cons7, cons9, tau, copula3,
#'                      distr.ev, distr.ce)
#'  n <- nrow(cova)
#'  p <- ncol(cova)
#'  ##-- event and dependent censoring proportions
#'  colSums(surv)[c(2,3)]/n
#'  X <- surv[,1] # Observed time
#'  del<-surv[,2] # failure status
#'  eta<-surv[,3] # dependent censoring status
#'
#'  ##-- control inputs for the coxph_mpl_dc function
#'  control <- coxph_mpl_dc.control(ordSp = 4,
#'                              binCount = 100,
#'                              tau = 0.8, copula = copula3,
#'                              pent = 'penalty_mspl', smpart = 'REML',
#'                              penc = 'penalty_mspl', smparc = 'REML',
#'                              cat.smpar = 'No' )
#'
#'  ##-- Fitting cox ph hazard model for T using MPL and an correct copula
#'  #with REML smoothing parameters
#'  coxMPLests5 <- coxph_mpl_dc(surv, cova, control, )
#'  mpl_beta_phi_zp5 <- coxMPLests5$mpl_beta_phi_zp
#'  mpl_h0t5 <- coxMPLests5$mpl_h0t
#'  mpl_h0Ti5 <- approx( X, mpl_h0t5, xout = seq(0, 5.4, 0.01),
#'                     method="constant", rule = 2, ties = mean)$y
#'
#'  ##-- Real marginal baseline hazard for T
#'  ht0b <- a * (seq(0, 5.4, 0.01) ^ (a - 1)) / (lambda ^ a)
#'
#'
#'  ##-- Fitting cox ph hazard model for T using MPL and an correct copula
#'  #with zero smoothing parameters
#'  coxMPLests3 <- coxph_mpl_dc(surv, cova,
#'                           ordSp=4, binCount=100,
#'                           tau=0.8, copula=copula3,
#'                           pent='penalty_mspl', smpart=0, penc='penalty_mspl', smparc=0,
#'                           cat.smpar = 'No')
#'  mpl_beta_phi_zp3 <- coxMPLests3$mpl_beta_phi_zp
#'  mpl_h0t3 <- coxMPLests3$mpl_h0t
#'  mpl_h0Ti3 <- approx( X, mpl_h0t3, xout = seq(0, 5.4, 0.01),
#'  method="constant", rule = 2, ties = mean)$y
#'
#'  ##-- Plot the true and estimated baseline hazards for T
#'  t_up <- 3.5
#'  y_uplim <- 2
#'  Ti<-seq(0, 5.4, 0.01)[seq(0, 5.4, 0.01)<=t_up]
#'  h0Ti<-ht0b[seq(0, 5.4, 0.01)<=t_up]
#'  h0Ti5<-mpl_h0Ti5[seq(0, 5.4, 0.01)<=t_up]
#'  h0Ti3<-mpl_h0Ti3[seq(0, 5.4, 0.01)<=t_up]
#'
#'  plot( x = Ti, y = h0Ti5,
#'       type="l", col="grey", lty=4, lwd=3, cex.axis=1.6, cex.lab=1.6, ylim=c(0, y_uplim),
#'       xlab='Time', ylab='Hazard')
#'  lines(x = Ti, y = h0Ti,
#'       col="green",
#'       lty=1, lwd=3, cex.axis=1.6, cex.lab=1.6, ylim=c(0, y_uplim)
#'       )
#'  lines(x = Ti, y = h0Ti3,
#'       col="blue",
#'       lty=4, lwd=3, cex.axis=1.6, cex.lab=1.6, ylim=c(0, y_uplim)
#'       )
#'
#'}
#'
#'
#'
#' @export

coxph_mpl_dc<-function(surv, cova, control,...)
{

  if(missing(control)){
    control<-coxph_mpl_dc.control(...)}

  ordSp<-control$ordSp
  binCount<-control$binCount
  tie<-control$tie
  tau<-control$tau
  copula<-control$copula
  pent<-control$pent
  smpart<-control$smpart
  penc<-control$penc
  smparc<-control$smparc
  maxit2<-control$maxit2
  maxit<-control$maxit
  mid<-control$mid
  asy<-control$asy
  ac<-control$ac
  cv<-control$cv
  ac.theta<-control$ac.theta
  ac.gamma<-control$ac.gamma
  ac.Utheta<-control$ac.Utheta
  ac.Ugamma<-control$ac.Ugamma
  min.theta<-control$min.theta
  min.gamma<-control$min.gamma
  min.ht<-control$min.ht
  min.hc<-control$min.hc
  min.St<-control$min.St
  min.Sc<-control$min.Sc
  min.C<-control$min.C
  min.dC<-control$min.dC
  eps<-control$eps
  tol.thga<-control$tol.thga
  tol.bph<-control$tol.bph
  tol.smpar<-control$tol.smpar
  cat.smpar<-control$cat.smpar

  if(missing(cova)){cova<-matrix(0, dim(surv)[1],1)}
  if(missing(ordSp)){ordSp<-1}
  if(missing(binCount)){binCount<-1}
  if(missing(tie)){tie<-'No'}
  if(missing(tau)){tau<-0}
  if(missing(copula)){copula<-'independent'}
  if(missing(pent)){pent<-'mat1'}
  if(missing(penc)){penc<-'mat1'}
  if(missing(smpart)){smpart<-0}
  if(missing(smparc)){smparc<-0}
  if(missing(maxit2)){maxit2<-50}
  if(missing(maxit)){maxit<-5000}
  if(missing(mid)){mid<-1}
  if(missing(asy)){asy<-1}
  if(missing(ac)){ac<-0}
  if(missing(cv)){cv<-0}

  if(missing(ac.theta)){ac.theta<-1e-5}
  if(missing(ac.gamma)){ac.gamma<-1e-5}
  if(missing(ac.Utheta)){ac.Utheta<--1e-2}
  if(missing(ac.Ugamma)){ac.Ugamma<--1e-2}
  if(missing(min.theta)){min.theta<-1e-7}
  if(missing(min.gamma)){min.gamma<-1e-7}
  if(missing(min.ht)){min.ht<-1e-7}
  if(missing(min.hc)){min.hc<-1e-7}
  if(missing(min.St)){min.St<-1e-7}
  if(missing(min.Sc)){min.Sc<-1e-7}
  if(missing(min.C)){min.C<-1e-7}
  if(missing(min.dC)){min.dC<-1e-7}
  if(missing(eps)){eps<-1e-5}
  if(missing(tol.thga)){tol.thga<-1e-5}
  if(missing(tol.bph)){tol.bph<-1e-5}
  if(missing(cat.smpar)){cat.smpar<-'Yes'}
  if(missing(tol.smpar)){tol.smpar<-1e-2}

  smpart_init<-smpart
  smparc_init<-smparc

  if(smpart=='REML'){smpart<-0}
  if(smparc=='REML'){smparc<-0}

  {
    if(copula=='independent'){alpha<-0}
    else if(copula=='clayton'){alpha<-iTau(claytonCopula(100), tau)} #calculate the alpha value of Clayton copula give the tau value
    else if(copula=='gumbel'){alpha<-iTau(gumbelCopula(100), tau)}
    else if (copula=='frank'){alpha<- -iTau(frankCopula(100), tau)}
    else (stop("Error! Define a proper copula function"))
  }

  del<-surv[,2]
  eta<-surv[,3]
  X<-surv[,1]

  discrsurvdat<-discrBinNA(surv, binCount, tie)
  groupsurvdat <- discrsurvdat$discretize
  binwv<-groupsurvdat[,1] #the bin widths
  ID<-discrsurvdat$ID  #the bin ID of each subject
  binedg<-discrsurvdat$binedg
  n<-nrow(cova)       #sample size
  p<-ncol(cova)#number of covariates for each subject

  numIntKnt<-length(binedg)-2
  IntKnt<-binedg[2:(numIntKnt+1)]
  bryKnt<-c(0, max(binedg))
  numSp<-numIntKnt+ordSp
  m<-numSp

  tcova<-cova #regression covariates
  cova <- cova - t(matrix(apply(cova,2,mean),p,n))

  if(ordSp==1)
  {

    psix<-psi(ID);
    Psix<-Psi(ID, binwv)
    {
      if(pent=='mat1') Rt<-mat1(psix, X, eps)
      else if (pent=='mat2') Rt<-mat2(psix, X, eps)
      else (stop("Error! Define a proper Penalty function for T"))
    }
    {
      if(penc=='mat1') Rc<-mat1(psix, X, eps)
      else if (penc=='mat2') Rc<-mat2(psix, X, eps)
      else (stop("Error! Define a proper Penalty function for C"))
    }

  }
  else
  {
    psix<-mSpline(X, knots = IntKnt, degree = ordSp-1, intercept = TRUE, derivs=0, Boundary.knots=bryKnt )
    Psix<-iSpline(X, knots = IntKnt, degree = ordSp-1, intercept = TRUE, derivs=0, Boundary.knots=bryKnt )
    {
      if(pent=='penalty_mspl') Rt<-penalty_mspl(numSp, ordSp, IntKnt, bryKnt)
      else (stop("Error! Define a proper Penalty function for T"))
    }
    {
      if(penc=='penalty_mspl') Rc<-penalty_mspl(numSp, ordSp, IntKnt, bryKnt)
      else (stop("Error! Define a proper Penalty function for C"))
    }

  }

  if( identical( cova, matrix( 0, n, 1 ) ) )
  {
    beta0 <- 0
    phi0 <- 0
  }
  else
  {
    coxpht<-coxph( Surv(surv[,1], surv[,2])~ tcova )
    beta0<-coxpht$coef
    beta0 <- rep( 0, dim(tcova)[2] )
    coxphc<-coxph( Surv(surv[,1], surv[,3]) ~ tcova )
    phi0<-coxphc$coef
    phi0 <- rep( 0, dim(tcova)[2] )
  }

  beta_old<-beta0 # initial estimates of the regression coefficient of the proportional hazard for failure time
  phi_old<-phi0 # initial estimates of the regression coefficient of the proportional hazard for dependent censoring time

  theta0<-theta_initial(del, psix, Psix, beta0, tcova) #initial estimates of theta's assumed independent censoring
  gamma0<-gamma_initial(eta, psix, Psix, phi0, tcova) #initial estimates of gamma's assumed independent censoring



  theta_old<-theta0
  gamma_old<-gamma0
  theta_old[theta_old<min.theta]<-min.theta
  gamma_old[gamma_old<min.gamma]<-min.gamma

  h0t0<-baseHaz(theta_old, psix);
  h0c0<-baseHaz(gamma_old, psix)
  h0t_old<-h0t0 #intital estimates of the baseline hazards for failure time
  h0c_old<-h0c0 #intital estimates of the baseline hazards for dependent censoring time
  h0t_old[h0t_old<min.ht]<-min.ht
  h0c_old[h0c_old<min.hc]<-min.hc

  H0t_old<-baseCumm(theta_old, Psix)
  H0c_old<-baseCumm(gamma_old, Psix)
  S0t_old<-exp(-H0t_old)
  S0c_old<-exp(-H0c_old)
  regt_old<-cova%*%beta_old
  regc_old<-cova%*%phi_old
  eregt_old<-exp(regt_old)
  eregc_old<-exp(regc_old)

  Hcoxt_old<-H0t_old*eregt_old
  Hcoxc_old<-H0c_old*eregc_old
  Scoxt_old<-exp(-Hcoxt_old)      #initial estimates of the survival function of failure based on Cox proportional hazard
  Scoxc_old<-exp(-Hcoxc_old) #initial estimates of the survival function of dependent censoring based on Cox proportional hazard
  Scoxt_old[Scoxt_old<min.St]<-min.St
  Scoxc_old[Scoxc_old<min.Sc]<-min.Sc

  {
    if(copula=='independent')
    {
      C_old<-IndependentCopula(Scoxt_old, Scoxc_old)  # independent copula function value
      dC_old<-dC_ind(Scoxt_old, Scoxc_old)# the first and second derivate of the independent copula function
      dC3_old<-dC3_ind(Scoxt_old, Scoxc_old)#the third derivative of the independent copula functiuon
    }
    else if(copula=='clayton')
    {
      C_old<-ClaytonCopula(Scoxt_old, Scoxc_old, alpha)
      dC_old<-dC_clay(Scoxt_old, Scoxc_old, alpha)
      dC3_old<-dC3_clay(Scoxt_old, Scoxc_old, alpha)
    }
    else if(copula=='gumbel')
    {
      C_old<-GumbelCopula(Scoxt_old, Scoxc_old, alpha)
      dC_old<-dC_gumbel(Scoxt_old, Scoxc_old, alpha)
      dC3_old<-dC3_gumbel(Scoxt_old, Scoxc_old, alpha)
    }
    else if (copula=='frank')
    {
      C_old<-FrankCopula(Scoxt_old, Scoxc_old, alpha)
      dC_old<-dC_frank(Scoxt_old, Scoxc_old, alpha)
      dC3_old<-dC3_frank(Scoxt_old, Scoxc_old, alpha)
    }
    else (stop("Error! Define a proper copula function"))
  }
  C_old[ C_old == Inf ] <- 1
  C_old[ C_old == -Inf ] <- 0
  dC_old[,1:3][dC_old[,1:3]<min.dC]<-min.dC
  C_old[C_old<min.C]<-min.C


  Rt_star <- rbind( matrix(0, 2*p, 2*p + 2*m), cbind( matrix(0, m, 2*p), Rt, matrix(0,m,m) ), matrix(0, m, 2*p+2*m) )
  Rc_star <- rbind( matrix(0, 2*p, 2*p + 2*m), matrix(0,m,2*p+2*m), cbind( matrix(0, m, 2*p+m), Rc ) )

  full.iter <- 0
  dftold <- m
  dfcold <- m

  for(k2 in 1: maxit2)
  {
    pllik<-0       #penalized log likelihood function
    pllik_old<-penlogreg_dep(del, eta, theta_old, gamma_old, h0t_old, h0c_old, regt_old, regc_old, Hcoxt_old, Hcoxc_old, C_old, dC_old, Rt, Rc, smpart, smparc)
    pllik[1]<-pllik_old

    for(k in 1:maxit)
    {
      coef_old<-c(beta_old, phi_old)
      Ubeta<-U_beta(p, cova, Hcoxt_old, Scoxt_old, del, eta, dC_old, C_old) #score function respect to beta
      Uphi<-U_phi(p, cova, Hcoxc_old, Scoxc_old, del, eta, dC_old, C_old) #score function respect to phi
      Ucoef<-c(Ubeta, Uphi)
      Hbb<-Hess_beta(p, C_old, dC_old, dC3_old, cova, Hcoxt_old, Scoxt_old, del, eta) #hessian matrix of beta
      Hpp<-Hess_phi(p, C_old, dC_old, dC3_old, cova, Hcoxc_old, Scoxc_old, del, eta) #hessian matrix of phi
      Hpb<-Hess_phi_beta(p, C_old, dC_old, dC3_old, cova, Hcoxt_old, Hcoxc_old, Scoxt_old, Scoxc_old, del, eta)
      Hbp<-t(Hpb)
      Hcoef<- -rbind(cbind(Hbb, Hbp), cbind(Hpb, Hpp))

      if(is.positive.definite( round(Hcoef, digits = 4) )==TRUE){
        Hcoef<-Hcoef
      }else{
        Ubet_i<-dplcox_dbeta_i( p, cova, Hcoxt_old, Scoxt_old, del, eta, dC_old, C_old )
        Uph_i<-dplcox_dphi_i( p, cova, Hcoxc_old, Scoxc_old, del, eta, dC_old, C_old )
        U_i<-cbind(Ubet_i, Uph_i)
        Hcoef<-t(U_i)%*%U_i
      }

      diag(Hcoef)<-diag(Hcoef)+eps
      inccoef<-solve(Hcoef)%*%Ucoef
      coef_new<-coef_old+inccoef  #updating beta using Newton algorithm
      beta_new<-coef_new[1:p]
      phi_new<-coef_new[(p+1):(2*p)]
      regt_new<-cova%*%beta_new
      regc_new<-cova%*%phi_new
      eregt_new<-exp(regt_new)
      eregc_new<-exp(regc_new)

      #eregt_new[eregt_new==Inf]=max( eregt_new[eregt_new<Inf] )
      #eregc_new[eregc_new==Inf]=max( eregc_new[eregc_new<Inf] )

      Hcoxt_new1<-H0t_old*eregt_new
      Hcoxc_new1<-H0c_old*eregc_new
      Scoxt_new1<-exp(-Hcoxt_new1)
      Scoxc_new1<-exp(-Hcoxc_new1)
      Scoxt_new1[Scoxt_new1<min.St]<-min.St
      Scoxc_new1[Scoxc_new1<min.Sc]<-min.Sc
      {
        if(copula=='independent')
        {
          C_new1<-IndependentCopula(Scoxt_new1, Scoxc_new1)
          dC_new1<-dC_ind(Scoxt_new1, Scoxc_new1)
        }
        else if(copula=='clayton')
        {
          C_new1<-ClaytonCopula(Scoxt_new1, Scoxc_new1, alpha)
          dC_new1<-dC_clay(Scoxt_new1, Scoxc_new1, alpha)
        }
        else if(copula=='gumbel')
        {
          C_new1<-GumbelCopula(Scoxt_new1, Scoxc_new1, alpha)
          dC_new1<-dC_gumbel(Scoxt_new1, Scoxc_new1, alpha)
        }
        else if (copula=='frank')
        {
          C_new1<-FrankCopula(Scoxt_new1, Scoxc_new1, alpha)
          dC_new1<-dC_frank(Scoxt_new1, Scoxc_new1, alpha)
        }
        else (stop("Error! Define a proper copula function"))
      }
      C_new1[ C_new1 == Inf ] <- 1
      C_new1[ C_new1 == -Inf ] <- 0
      dC_new1[,1:3][dC_new1[,1:3]<min.dC]<-min.dC
      C_new1[C_new1<min.C]<-min.C
      pllik_new1<-penlogreg_dep(del, eta, theta_old, gamma_old, h0t_old, h0c_old, regt_new, regc_new, Hcoxt_new1, Hcoxc_new1, C_new1, dC_new1, Rt, Rc, smpart, smparc)

      Ubeta_new1<-U_beta(p, cova, Hcoxt_new1, Scoxt_new1, del, eta, dC_new1, C_new1) #score function respect to beta
      Uphi_new1<-U_phi(p, cova, Hcoxc_new1, Scoxc_new1, del, eta, dC_new1, C_new1) #score function respect to phi

      ome<-0.6


      while( pllik_new1<pllik_old | min( c( Ubeta_new1/Ubeta, Uphi_new1/Uphi ) )<0  )
      {
        coef_new<-coef_old+ome*inccoef
        beta_new<-coef_new[1:p]
        phi_new<-coef_new[(p+1):(2*p)]
        regt_new<-cova%*%beta_new
        regc_new<-cova%*%phi_new
        eregt_new<-exp(regt_new)
        eregc_new<-exp(regc_new)

        Hcoxt_new1<-H0t_old*eregt_new
        Hcoxc_new1<-H0c_old*eregc_new
        Scoxt_new1<-exp(-Hcoxt_new1)
        Scoxc_new1<-exp(-Hcoxc_new1)
        Scoxt_new1[Scoxt_new1<min.St]<-min.St
        Scoxc_new1[Scoxc_new1<min.Sc]<-min.Sc
        {
          if(copula=='independent')
          {
            C_new1<-IndependentCopula(Scoxt_new1, Scoxc_new1)
            dC_new1<-dC_ind(Scoxt_new1, Scoxc_new1)
          }
          else if(copula=='clayton')
          {
            C_new1<-ClaytonCopula(Scoxt_new1, Scoxc_new1, alpha)
            dC_new1<-dC_clay(Scoxt_new1, Scoxc_new1, alpha)
          }
          else if(copula=='gumbel')
          {
            C_new1<-GumbelCopula(Scoxt_new1, Scoxc_new1, alpha)
            dC_new1<-dC_gumbel(Scoxt_new1, Scoxc_new1, alpha)
          }
          else if (copula=='frank')
          {
            C_new1<-FrankCopula(Scoxt_new1, Scoxc_new1, alpha)
            dC_new1<-dC_frank(Scoxt_new1, Scoxc_new1, alpha)
          }
          else (stop("Error! Define a proper copula function"))
        }
        C_new1[ C_new1 == Inf ] <- 1
        C_new1[ C_new1 == -Inf ] <- 0
        dC_new1[,1:3][dC_new1[,1:3]<min.dC]<-min.dC
        C_new1[C_new1<min.C]<-min.C
        pllik_new1<-penlogreg_dep(del, eta, theta_old, gamma_old, h0t_old, h0c_old, regt_new, regc_new, Hcoxt_new1, Hcoxc_new1,
                                  C_new1, dC_new1, Rt, Rc, smpart, smparc)

        Ubeta_new1<-U_beta(p, cova, Hcoxt_new1, Scoxt_new1, del, eta, dC_new1, C_new1) #score function respect to beta
        Uphi_new1<-U_phi(p, cova, Hcoxc_new1, Scoxc_new1, del, eta, dC_new1, C_new1) #score function respect to phi

        if(ome >= 1e-2) ome=ome*0.6
        else if(ome < 1e-2 & ome >= 1e-5) ome=ome*5e-2
        else if(ome < 1e-5 & ome >= 1e-20) ome=ome*1e-5
        else
        {
          print("Iterations do not move coeficients")
          break
        }
      }

      denomt<-denomtreg(Rt, psix, Psix, theta_old, dC_new1, C_new1, Scoxt_new1, eregt_new, del, eta, smpart)
      st<-theta_old/(denomt+1e-5)   #step of function to update theta
      Uth<-U_theta(psix, Psix, theta_old, h0t_old, eregt_new, Scoxt_new1, del, eta, dC_new1, C_new1, smpart, Rt) #score function respect to theta
      theta_new<-theta_old+st*Uth#updating theta using MI algorithm
      denomc<-denomcreg(Rc, psix, Psix, gamma_old, dC_new1, C_new1, Scoxc_new1, eregc_new, del, eta, smparc)
      sc<-gamma_old/(denomc++1e-5)
      Uga<-U_gamma(psix, Psix, gamma_old, h0c_old, eregc_new, Scoxc_new1, del, eta, dC_new1, C_new1, smparc, Rc)
      gamma_new<-gamma_old+sc*Uga
      theta_new[theta_new<min.theta]<-min.theta
      gamma_new[gamma_new<min.gamma]<-min.gamma

      h0t_new<-baseHaz(theta_new, psix)
      h0c_new<-baseHaz(gamma_new, psix)
      h0t_new[h0t_new<min.ht]<-min.ht
      h0c_new[h0c_new<min.hc]<-min.hc

      H0t_new<-baseCumm(theta_new,Psix)
      H0c_new<-baseCumm(gamma_new,Psix)
      S0t_new<-exp(-H0t_new)
      S0c_new<-exp(-H0c_new)
      Hcoxt_new2<-H0t_new*eregt_new
      Hcoxc_new2<-H0c_new*eregc_new
      Scoxt_new2<-exp(-Hcoxt_new2)
      Scoxc_new2<-exp(-Hcoxc_new2)
      Scoxt_new2[Scoxt_new2<min.St]<-min.St
      Scoxc_new2[Scoxc_new2<min.Sc]<-min.Sc
      {
        if(copula=='independent')
        {
          C_new2<-IndependentCopula(Scoxt_new2, Scoxc_new2)
          dC_new2<-dC_ind(Scoxt_new2, Scoxc_new2)
          dC3_new2<-dC3_ind(Scoxt_new2, Scoxc_new2)
        }
        else if(copula=='clayton')
        {
          C_new2<-ClaytonCopula(Scoxt_new2, Scoxc_new2, alpha)
          dC_new2<-dC_clay(Scoxt_new2, Scoxc_new2, alpha)
          dC3_new2<-dC3_clay(Scoxt_new2, Scoxc_new2, alpha)
        }
        else if(copula=='gumbel')
        {
          C_new2<-GumbelCopula(Scoxt_new2, Scoxc_new2, alpha)
          dC_new2<-dC_gumbel(Scoxt_new2, Scoxc_new2, alpha)
          dC3_new2<-dC3_gumbel(Scoxt_new2, Scoxc_new2, alpha)
        }
        else if (copula=='frank')
        {
          C_new2<-FrankCopula(Scoxt_new2, Scoxc_new2, alpha)
          dC_new2<-dC_frank(Scoxt_new2, Scoxc_new2, alpha)
          dC3_new2<-dC3_frank(Scoxt_new2, Scoxc_new2, alpha)
        }
        else (stop("Error! Define a proper copula function"))
      }
      C_new2[ C_new2 == Inf ] <- 1
      C_new2[ C_new2 == -Inf ] <- 0
      dC_new2[,1:3][dC_new2[,1:3]<min.dC]<-min.dC
      C_new2[C_new2<min.C]<-min.C

      pllik_new2<-penlogreg_dep(del, eta, theta_new, gamma_new, h0t_new, h0c_new, regt_new, regc_new, Hcoxt_new2, Hcoxc_new2, C_new2, dC_new2, Rt, Rc, smpart, smparc)

      incth<-theta_new-theta_old
      incga<-gamma_new-gamma_old
      ome<-0.6
      while(pllik_new2<pllik_new1)
      {
        theta_new<-theta_old+ome*incth
        gamma_new<-gamma_old+ome*incga
        theta_new[theta_new<min.theta]<-min.theta
        gamma_new[gamma_new<min.gamma]<-min.gamma

        h0t_new<-baseHaz(theta_new, psix)
        h0c_new<-baseHaz(gamma_new, psix)
        h0t_new[h0t_new<min.ht]<-min.ht
        h0c_new[h0c_new<min.hc]<-min.hc

        H0t_new<-baseCumm(theta_new,Psix)
        H0c_new<-baseCumm(gamma_new,Psix)

        S0t_new<-exp(-H0t_new)
        S0c_new<-exp(-H0c_new)
        Hcoxt_new2<-H0t_new*eregt_new
        Hcoxc_new2<-H0c_new*eregc_new
        Scoxt_new2<-exp(-Hcoxt_new2)
        Scoxc_new2<-exp(-Hcoxc_new2)
        Scoxt_new2[Scoxt_new2<min.St]<-min.St
        Scoxc_new2[Scoxc_new2<min.Sc]<-min.Sc
        {
          if(copula=='independent')
          {
            C_new2<-IndependentCopula(Scoxt_new2, Scoxc_new2)
            dC_new2<-dC_ind(Scoxt_new2, Scoxc_new2)
            dC3_new2<-dC3_ind(Scoxt_new2, Scoxc_new2)
          }
          else if(copula=='clayton')
          {
            C_new2<-ClaytonCopula(Scoxt_new2, Scoxc_new2, alpha)
            dC_new2<-dC_clay(Scoxt_new2, Scoxc_new2, alpha)
            dC3_new2<-dC3_clay(Scoxt_new2, Scoxc_new2, alpha)
          }
          else if(copula=='gumbel')
          {
            C_new2<-GumbelCopula(Scoxt_new2, Scoxc_new2, alpha)
            dC_new2<-dC_gumbel(Scoxt_new2, Scoxc_new2, alpha)
            dC3_new2<-dC3_gumbel(Scoxt_new2, Scoxc_new2, alpha)
          }
          else if (copula=='frank')
          {
            C_new2<-FrankCopula(Scoxt_new2, Scoxc_new2, alpha)
            dC_new2<-dC_frank(Scoxt_new2, Scoxc_new2, alpha)
            dC3_new2<-dC3_frank(Scoxt_new2, Scoxc_new2, alpha)
          }
          else (stop("Error! Define a proper copula function"))
        }
        C_new2[ C_new2 == Inf ] <- 1
        C_new2[ C_new2 == -Inf ] <- 0
        dC_new2[,1:3][dC_new2[,1:3]<min.dC]<-min.dC
        C_new2[C_new2<min.C]<-min.C
        pllik_new2<-penlogreg_dep(del, eta, theta_new, gamma_new, h0t_new, h0c_new, regt_new, regc_new, Hcoxt_new2, Hcoxc_new2, C_new2, dC_new2, Rt, Rc, smpart, smparc)
        if(ome >= 1e-2) ome=ome*0.6
        else if(ome < 1e-2 & ome >= 1e-5) ome=ome*5e-2
        else if(ome < 1e-5 & ome >= 1e-30) ome=ome*1e-5
        else
        {
          print("Iterations do not move baseline hazards")
          break
        }
      }

      h0_old<-c(theta_old, gamma_old)
      h0_new<-c(theta_new, gamma_new)
      pllik[k+1]<-pllik_new2
      #print(k)
      if (max(abs(h0_new-h0_old))<tol.thga & max(abs(coef_new-coef_old))<tol.bph)
      {
        break
      }
      else
      {
        theta_old<-theta_new
        gamma_old<-gamma_new
        h0t_old<-h0t_new
        h0c_old<-h0c_new
        beta_old<-beta_new
        phi_old<-phi_new
        H0t_old<-H0t_new
        H0c_old<-H0c_new
        S0t_old<-S0t_new
        S0c_old<-S0c_new
        regt_old<-regt_new
        regc_old<-regc_new
        eregt_old<-eregt_new
        eregc_old<-eregc_new
        Hcoxt_old<-Hcoxt_new2
        Hcoxc_old<-Hcoxc_new2
        Scoxt_old<-Scoxt_new2
        Scoxc_old<-Scoxc_new2
        C_old<-C_new2
        dC_old<-dC_new2
        dC3_old<-dC3_new2
        pllik_old<-pllik_new2
      }
    }

    fb<-apply(tcova, 2, mean)%*%beta_new
    fph<-apply(tcova, 2, mean)%*%phi_new
    theta_cop<-theta_new%*%exp(-fb)
    gamma_cop<-gamma_new%*%exp(-fph)

    theta_cop[theta_cop<min.theta]<-min.theta
    gamma_cop[gamma_cop<min.gamma]<-min.gamma

    h0t_cop<-baseHaz(theta_cop, psix)
    h0c_cop<-baseHaz(gamma_cop, psix)

    h0t_cop[h0t_cop<min.ht]<-min.ht
    h0c_cop[h0c_cop<min.hc]<-min.hc

    beta_cop<-beta_new
    phi_cop<-phi_new
    H0t_cop<-baseCumm(theta_cop,Psix)
    H0c_cop<-baseCumm(gamma_cop,Psix)
    S0t_cop<-exp(-H0t_cop) #the corresonding MPL estimates of S_{0T}(x)
    S0c_cop<-exp(-H0c_cop) #the corresonding MPL estimates of S_{0C}(x)
    regt<-tcova%*%beta_cop
    regc<-tcova%*%phi_cop
    eregt<-exp(regt)
    eregc<-exp(regc)

    hcoxt<-h0t_cop*eregt #the corresonding MPL estimates of h_{T}(x)
    hcoxc<-h0c_cop*eregc #the corresonding MPL estimates of h_{C}(x)
    Hcoxt<-H0t_cop*eregt #the corresonding MPL estimates of H_{T}(x)
    Hcoxc<-H0c_cop*eregc #the corresonding MPL estimates of H_{C}(x)
    Scoxt<-exp(-Hcoxt)
    Scoxc<-exp(-Hcoxc)
    Scoxt[Scoxt<min.St]<-min.St
    Scoxc[Scoxc<min.Sc]<-min.Sc
    {
      if(copula=='independent')
      {
        dC3 <- dC3_ind(Scoxt, Scoxc)
        dC <- dC_ind(Scoxt, Scoxc)
        Co <- IndependentCopula(Scoxt, Scoxc)
      }
      else if(copula=='clayton')
      {
        dC3 <- dC3_clay(Scoxt, Scoxc, alpha)
        dC <- dC_clay(Scoxt, Scoxc, alpha)
        Co <- ClaytonCopula(Scoxt, Scoxc, alpha)
      }
      else if(copula=='gumbel')
      {
        dC3 <- dC3_gumbel(Scoxt, Scoxc, alpha)
        dC <- dC_gumbel(Scoxt, Scoxc, alpha)
        Co <- GumbelCopula(Scoxt, Scoxc, alpha)
      }
      else if (copula=='frank')
      {
        dC3 <- dC3_frank(Scoxt, Scoxc, alpha)
        dC <- dC_frank(Scoxt, Scoxc, alpha)
        Co <- FrankCopula(Scoxt, Scoxc, alpha)
      }
      else (stop("Error! Define a proper copula function"))
      Co[ Co == Inf ] <- 1
      Co[ Co == -Inf ] <- 0
      dC[,1:3][dC[,1:3]<min.dC]<-min.dC
      Co[Co<min.C]<-min.C
    }

    Ubeta_cop<-U_beta(p, cova, Hcoxt, Scoxt, del, eta, dC, Co) #score function respect to beta
    Uphi_cop<-U_phi(p, cova, Hcoxc, Scoxc, del, eta, dC, Co) #score function respect to phi
    Uth_cop<-U_theta(psix, Psix, theta_cop, h0t_cop, eregt, Scoxt, del, eta, dC, Co, smpart, Rt) #score function respect to theta
    Uga_cop<-U_gamma(psix, Psix, gamma_cop, h0c_cop, eregc, Scoxc, del, eta, dC, Co, smparc, Rc)

    if(asy==0) #without compute the asymptotic standard deviation of the MPL estimates
    {
      std_b_asy<-0;std_ph_asy<-0;std_th_asy<-0;std_ga_asy<-0;
      cov_asy<-0
      zp_b_ph<-0
      std_ht0_asy<-0
      std_hc0_asy<-0
      std_Ht0_asy<-0
      std_Hc0_asy<-0
      std_St0_asy<-0
      std_Sc0_asy<-0

    }
    else
    {
      asymstd<-asycoxphdepcencopmpl(Co, dC, dC3,
                                   tcova,
                                   psix, Psix,
                                   theta_cop, gamma_cop, Uth_cop, Uga_cop,
                                   h0t_cop, h0c_cop,
                                   eregt, eregc,
                                   Hcoxt, Scoxt, Hcoxc, Scoxc,
                                   del, eta,
                                   smpart, Rt, smparc, Rc,
                                   eps, mid,
                                   ac.theta, ac.gamma, ac.Utheta, ac.Ugamma)
      std_b_asy<-asymstd$asympstd_beta #asymptotic std of beta
      std_ph_asy<-asymstd$asympstd_phi #asymptotic std of phi
      std_th_asy<-asymstd$asympstd_theta #asymptotic std of theta
      std_ga_asy<-asymstd$asympstd_gamma #asymptotic std of gamma
      cov_asy<-asymstd$asympcov_est #asymptotic covaraince of the estimates
      z_b_asy<-beta_cop/std_b_asy #z score of beta
      z_ph_asy<-phi_cop/std_ph_asy #z score of phi
      p_b_asy<-2*pnorm(-abs(z_b_asy)) #p-value of beta
      p_ph_asy<-2*pnorm(-abs(z_ph_asy))  #p-value of phi
      zp_b_ph<-cbind(
        c( beta_cop, phi_cop ),
        c( std_b_asy, std_ph_asy ),
        c( z_b_asy, z_ph_asy ),
        c( p_b_asy, p_ph_asy )
      ) #regression ooefficient estimates with the corresponding standard deviations, z scores and p values
      colnames(zp_b_ph)<-c("Est", "SD", "Z score", "P value")
      rownames(zp_b_ph)<-rep(colnames(cova), 2)
      std_ht0_asy<-sqrt( diag( psix%*%( cov_asy[(2*p+1):(2*p+m),(2*p+1):(2*p+m)] )%*%t( psix ) ) )
      std_hc0_asy<-sqrt( diag( psix%*%( cov_asy[(2*p+m+1):(2*p+2*m),(2*p+m+1):(2*p+2*m)] )%*%t( psix ) ) )

      std_Ht0_asy<-sqrt( diag( Psix%*%( cov_asy[(2*p+1):(2*p+m),(2*p+1):(2*p+m)] )%*%t( Psix ) ) )
      std_Hc0_asy<-sqrt( diag( Psix%*%( cov_asy[(2*p+m+1):(2*p+2*m),(2*p+m+1):(2*p+2*m)] )%*%t( Psix ) ) )

      std_St0_asy<-S0t_cop*std_Ht0_asy
      std_Sc0_asy<-S0c_cop*std_Hc0_asy
    }
    if(ac==0){aic<-0}
    else
    {maxpl<-max(pllik)
    aic<-2*2*(m+p)-2*maxpl}

    if( identical(cova, matrix(0, n, 1)) ){noX <- TRUE}else{noX <- FALSE}
    pos<-c( if(noX){rep(FALSE, 2*p)}else{rep(TRUE, 2*p)}, c(theta_cop>=ac.theta, gamma_cop>=ac.gamma) | c(Uth_cop>=ac.Utheta, Uga_cop>=ac.Ugamma) )
    H<-hesscoxphcopmpl(p, Co, dC, dC3, tcova, h0t_cop, h0c_cop, eregt, eregc, Hcoxt, Scoxt, Hcoxc, Scoxc, psix, Psix, del, eta, 0, Rt, 0, Rc)
    Hp<-hesscoxphcopmpl(p, Co, dC, dC3, tcova, h0t_cop, h0c_cop, eregt, eregc, Hcoxt, Scoxt, Hcoxc, Scoxc, psix, Psix, del, eta, smpart, Rt, smparc, Rc)
    diag(Hp)<-diag(Hp)+eps
    l<-penlogreg_dep(del, eta, theta_cop, gamma_cop, h0t_cop, h0c_cop, regt, regc, Hcoxt, Hcoxc, Co, dC, Rt, Rc, 0, 0)
    if(cv==0){cvl<-0}
    else
    {
      Hpinv<-matrix(0, (2*p+2*m), (2*p+2*m))
      Hpinv[pos,pos]<-solve(Hp[pos, pos])
      cvl<-l-sum(diag(Hpinv%*%H))
    }

    if(smpart_init=='REML')
    {
      HtRinv <- matrix(0, 2*p + 2*m, 2*p + 2*m)
      sigmat2_old <- 1/( smpart)
      sigmac2_old <- 1/( smparc)
      diag(H)<-diag(H)+eps
      HtRinv[pos, pos]<-solve( -H[pos, pos] + ( 1/sigmat2_old )*Rt_star[pos, pos] + ( 1/sigmac2_old )*Rc_star[pos, pos] )
      dft<-m - ( sum( diag( HtRinv%*%Rt_star ) )/sigmat2_old )
      sigmat2<-c( t( theta_cop )%*%Rt%*%theta_old/( dft ) )
      smpart_old <- smpart
      smpart <- 1/( sigmat2 )
    }
    else
    {
      sigmat2_old <- 1/( smpart)
      sigmat2<-sigmat2_old
      smpart_old <- smpart
      smpart <- 1/( sigmat2 )
      dft=m
    }

    if(smparc_init=='REML')
    {
      HcRinv <- matrix(0, 2*p + 2*m, 2*p + 2*m)
      sigmat2_old <- 1/( smpart)
      sigmac2_old <- 1/( smparc)
      HcRinv[pos, pos] <- solve( -H[pos, pos] + ( 1/sigmat2_old )*Rt_star[pos, pos] + ( 1/sigmac2_old )*Rc_star[pos, pos] )
      dfc <- m - ( sum( diag( HcRinv%*%Rc_star ) )/sigmac2_old )
      sigmac2 <- c( t( gamma_cop )%*%Rc%*%gamma_old/( dfc ) )
      smparc_old <- smparc
      smparc <- 1/( sigmac2 )
    }
    else
    {
      sigmac2_old <- 1/( smparc)
      sigmac2 <- sigmac2_old
      smparc_old <- smparc
      smparc <- 1/( sigmac2 )
      dfc <- m
    }

    full.iter <- full.iter + k

    if(cat.smpar=='Yes'){
      cat(k2,"\tsmpart =",smpart,"\tsmparc =",smparc,"\tdft=",dft,"\tdfc=",dfc,"\titer =", k, "\n")
    }

    #print(k2)
    if( abs( smpart - smpart_old )<( tol.smpar ) & abs( smparc - smparc_old ) < ( tol.smpar ) )
    {
      break
    }
    else{
      dftold=dft
      dfcold=dfc
      smpart_old=smpart
      smparc_old=smparc
      sigmat2_old = sigmat2
      sigmac2_old = sigmac2
    }

  }
  iter <- c(k2, full.iter, k)
  if(k2==1)
  {
    smpart <- smpart_old
    smparc <- smparc_old
  }

  fit <- list(
      mpl_theta=theta_cop, mpl_gamma=gamma_cop,
      mpl_Utheta=Uth_cop, mpl_Ugamma=Uga_cop,
      mpl_h0t=h0t_cop, mpl_h0c=h0c_cop,
      mpl_H0t=H0t_cop, mpl_H0c=H0c_cop,
      mpl_S0t=S0t_cop, mpl_S0c=S0c_cop,
      mpl_beta=beta_cop, mpl_phi=phi_cop,
      mpl_Ubeta=Ubeta_cop, mpl_Uphi=Uphi_cop,
      mpl_ht=hcoxt, mpl_hc=hcoxc,
      mpl_Ht=Hcoxt, mpl_Hc=Hcoxc,
      mpl_St=Scoxt, mpl_Sc=Scoxc,
      penloglik=pllik, iteration=iter,
      smootht=smpart, smoothc=smparc, sigmat2=sigmat2, sigmac2=sigmac2,
      mpl_cvl=cvl, mpl_aic=aic,
      mpl_beta_sd=std_b_asy, mpl_phi_sd=std_ph_asy, mpl_h0t_sd=std_th_asy, mpl_h0c_sd=std_ga_asy,
      mpl_ht0_sd=std_ht0_asy, mpl_hc0_sd=std_hc0_asy,
      mpl_Ht0_sd=std_Ht0_asy, mpl_Hc0_sd=std_Hc0_asy,
      mpl_St0_sd=std_St0_asy, mpl_Sc0_sd=std_Sc0_asy,

      mpl_est_cov=cov_asy, mpl_beta_phi_zp=zp_b_ph,
      binedg=binedg, binwv=binwv,ID=ID, psix=psix, Psix=Psix,

      numIntKnt = numIntKnt, #length(binedg)-2
      IntKnt = IntKnt, #binedg[2:(numIntKnt+1)]
      bryKnt = bryKnt, #c(0, max(binedg))
      numSp = numSp, #numIntKnt+ordSp

      surv=surv,
      cova=cova,

      ordSp=ordSp,
      binCount=binCount,
      tie=tie,
      tau=tau,
      copula=copula,
      pent=pent,
      smpart_init=smpart_init,
      penc=penc,
      smparc_init=smparc_init,
      maxit2=maxit2,
      maxit=maxit,
      mid=mid,
      asy=asy,
      ac=ac,
      cv=cv,
      ac.theta=ac.theta,
      ac.gamma=ac.gamma,
      ac.Utheta=ac.Utheta,
      ac.Ugamma=ac.Ugamma,
      min.theta=min.theta,
      min.gamma=min.gamma,
      min.ht=min.ht,
      min.hc=min.hc,
      min.St=min.St,
      min.Sc=min.Sc,
      min.C=min.C,
      min.dC=min.dC,
      eps=eps,
      tol.thga=tol.thga,
      tol.bph=tol.bph,
      tol.smpar=tol.smpar,
      cat.smpar=cat.smpar
    )
class(fit) = "coxph_mpl_dc"
return( fit )
}

