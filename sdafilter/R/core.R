#' Symmetrized Data Aggregation
#'
#' This is the core function for the paper posted in arXiv preprint arXiv:2002.11992
#'
#'
#' @param dat a n by p data matrix
#' @param alpha the FDR level
#' @param Omega the inverse covariance matrix; if missing, it will be estimated
#' by the glasso package
#' @param nonsparse if TRUE, the covariance matrix will be estimated by the POET
#' package
#' @param stable if TRUE, the sample will be randomly splitted B=10 times for stability
#' performance; otherwise, only single sample splitting is used.
#'
#' @return the indices of the hypotheses rejected
#' @export
#' @examples
#' n = 50
#' p = 100
#' dat = matrix(rnorm(n*p), nrow=n)
#' mu = rep(0, p)
#' mu[1:as.integer(0.1*p)]=0.3
#' dat = dat+rep(1, n)%*%t(mu)
#' alpha = 0.2
#' out = SDA_M(dat, alpha, diag(p))
#' print(out)
#'
SDA_M <- function(dat, alpha, Omega, nonsparse = FALSE, stable=TRUE){
  # The core functions for our SDA methods
  # The inverse of Sigma
  INV <- function(Sigma){
    EgSig0 = eigen(Sigma)
    EgVec = EgSig0$vectors
    Omega = EgVec%*%diag( 1/EgSig0$values )%*%t(EgVec)
    return( Omega )
  }

  # Omega^{1/2}
  Sqrt <- function(Omega_1){
    EgSig0 = eigen(Omega_1)
    EgVec = EgSig0$vectors
    Gamma = EgVec%*%diag( sqrt(EgSig0$values) )%*%t(EgVec)
    return( Gamma )
  }

  # Glasso for estimation the inverse covariance matrix
  Omega_est <- function(dat, lambda){

    p = ncol(dat)
    n = nrow(dat)
    Q = diag(n)-1/n*rep(1, n)%*%t(rep(1, n))
    Sigma_est = t(dat)%*%Q%*%dat/(n-1)
    # glasso packages
    GG = glasso::glasso(Sigma_est, lambda)
    Omega_1 = GG$wi
    #Sigma_1 = GG$w
    #Sigma_1 = GG$w-diag(p)*lambda # bias correction on the diagonal
    #Omega_1 = INV(Sigma_1)

    return(Omega_1)
  }

  # POET for Sigma/Omega
  POET_sig <- function(dat, K_hat){
    n = nrow(dat)
    Q = diag(n)-1/n*rep(1, n)%*%t(rep(1, n))
    Y = t(dat)%*%Q
    PP = POET::POET(Y, K_hat, 0.5, thres='soft', matrix = 'cor')
    Sigma_1 = PP$SigmaY
    Omega_1 = INV(Sigma_1)
    return(Omega_1)
  }


  # LASSO for screening
  Model_Select <- function(Y, X){
    # Method I: AIC
    fit <- glmnet::glmnet(X, Y, family = "gaussian")
    k <- fit$df
    AIC <- stats::deviance(fit)+2*k
    i_min = which.min(AIC)
    lambda_select = fit$lambda[i_min]
    fit_AIC = glmnet::glmnet(X, Y, family = "gaussian", lambda = lambda_select)
    w1 = fit_AIC$beta[,1]
    sv = as.vector( which(w1!=0) )

    # method II: upper bound p/3
    k= ceiling(p/3)
    if( length(sv)>k ){
      wv = which(fit$df==max(fit$df[fit$df<k]))[1]
      sv = which(fit$beta[, wv]!=0)
      w1 = fit$beta[, wv]
    }

    return( list(index_S=sv, w1 = w1))
  }

  # generate W_j: one splitting
  W_value <- function(Gamma, dat, index){
    n=nrow(dat)
    p=ncol(dat)
    n_1 = length(index)
    n_2 = n-n_1
    dat1 = apply(dat[index,], 2, mean)
    dat2 = apply(dat[-index,],2, mean)


    X= sqrt(n_1)*Gamma
    Xt = sqrt(n_2)*Gamma
    Y = as.vector(sqrt(n_1)*dat1%*%Gamma)
    Yt = as.vector(sqrt(n_2)*dat2%*%Gamma)
    #
    MS = Model_Select(Y, X)
    sv = MS$index_S
    w1 = MS$w1
    ####refit with Z1 part#####
    if ( length(sv)>0&&length(sv)<p ){
      bt2 = stats::lm(Yt~Xt[,sv]-1)$coefficients
      w2 = rep(0,p)
      w2[sv]=bt2
      sigma_w = rep(1, p)
      DIAG = diag( solve( t(Xt[,sv])%*%Xt[,sv] ) )
      sigma_w[sv] = DIAG
      W = w1*w2/sigma_w
    }else{
      W = rep(0, p)
    }
    return(W)
  }

  # FDR control based mirror
  W_det <- function(Wj, alpha, options){
    t = sort(abs(Wj))

    if(options=='+'){
      Ta = sapply(t,function(x){(1+length(Wj[Wj<=(-x)]))/max(1,length(Wj[Wj>=x]))})
    }else{
      Ta = sapply(t,function(x){(length(Wj[Wj<=(-x)]))/max(1,length(Wj[Wj>=x]))})
    }

    bestlam = min(t[which(Ta<=alpha)])
    det=which(Wj>=bestlam)
    aa = rep(0, length(Wj))
    aa[det] = 1
    return(aa)
  }

  ##########################################
  # create K=10 sample splits
  # multiple splitting
  p = ncol(dat)
  n = nrow(dat)
  m = as.integer(2/3*n)

  # stable option: single or multiple splitting
  if(stable){
    K=10
  }else{K=1}
  #---------------------------------

  # Need to estimate Omega
  if ( missing(Omega) ){

    if(nonsparse){
      # poet
      n = nrow(dat)
      Q = diag(n)-1/n*rep(1, n)%*%t(rep(1, n))
      Y = t(dat)%*%Q
      K_poet = POET::POETKhat(Y)$K1BN

      Index_K = lapply(1:K, function(x){sample(1:n, m)})
      Omega_K = lapply(1:K, function(x){POET_sig(dat[Index_K[[x]], ], K_poet)})
      Gamma_K = lapply(1:K, function(x){Sqrt(Omega_K[[x]])})
    }else{
      # glasso
      # lambda: tuning parameter for glasso
      out.glasso = huge::huge(dat, method = "glasso")
      out.select = huge::huge.select(out.glasso, criterion = "stars")
      lambda = out.select$opt.lambda
      #
      Index_K = lapply(1:K, function(x){sample(1:n, m)})
      Omega_K = lapply(1:K, function(x){Omega_est(dat[Index_K[[x]], ], lambda)})
      Gamma_K = lapply(1:K, function(x){Sqrt(Omega_K[[x]])})
    }

  }else{
    Index_K = lapply(1:K, function(x){sample(1:n, m)})
    Gamma = Sqrt(Omega)
    Gamma_K = lapply(1:K, function(x){Gamma})
  }


  Wa = sapply(1:K, function(x){ W_value(Gamma_K[[x]], dat, Index_K[[x]]) })
  deta = sapply(1:K, function(x){ W_det(Wa[, x], alpha, '-') })
  deta = as.matrix(deta)
  mdet = apply(deta, 1, sum)
  det1 = which(mdet>=as.integer(0.5*K)+1)
  # the majority vote
  aa=rep(0, p)
  if(length(det1)>0) aa[det1]=1

  # pick the best one
  k_hat = which.min(sapply(1:K,function(x){sum(abs(aa-deta[,x]))}))
  det1=(1:p)[deta[, k_hat]==1]
  if (length(det1)==0){
    out = 'no rejection'
  }else{
    out = det1
  }
  return(out)
}
