

#' @title MSB-test procedure for the cointegration rank
#' @description Performs the MSB-test procedure for the rank of cointegration in a single VAR model.
#'   The \eqn{p}-values are calculated from the simulated distributions, which the cited paper provides.
#' @describeIn coint MSB-procedure.
#' @param eit Matrix. The time series data.
#' @param type_MSB Character. The conventional case of the deterministic term.
#' @param lag_max Vector of integers. The maximum lag order that is considered 
#'   by the \emph{modified information criterion} for the VAR model in levels.
#' @param MIC Character. The modified information criterion to use for choosing the lag-order \eqn{p}.
#' 
#' @references Carrion-i-Silvestre, J. L., and Surdeanu, L. (2011): 
#'   "Panel Cointegration Rank Testing with Cross-Section Dependence", 
#'   \emph{Studies in Nonlinear Dynamics & Econometrics}, 15, pp. 1-43.
#' @return A list of class \code{coint}, which contains elements of length \eqn{K} for each \eqn{r_{H0}=0,\ldots,K-1}:
#' \item{r_H0}{Rank under each null hypothesis.}
#' \item{m_H0}{Stochastic trends under each null hypothesis.}
#' \item{lags}{Lag-order as suggested by the modified information criterion.}
#' \item{stats}{MSB test statistics.}
#' \item{pvals}{\eqn{p}-values of the MSB tests.}
#' \item{args_coint}{List of characters and integers indicating the cointegration test and specifications that have been used.}
#' 
#'  @family cointegration rank tests
#'  @export
#' 
coint.MSB <- function(eit, type_MSB=c("MSB_mean", "MSB_trend"), lag_max, MIC=c("AIC", "SIC", "HQC")){
  ### see also Bai,Silvestre (2013) for the univariate cointegration test / Stock 1999:147 for MSB-tests
  # define
  eit = as.matrix(eit)
  if(ncol(eit) < nrow(eit)){ eit = t(eit) }  # matrix is supposed to be KxT regardless of input
  dim_T = ncol(eit)  # number of total observations incl. presample
  dim_K = nrow(eit)  # number of endogenous variables
  ### TODO function: scale and detrend/demean "eit" here for PCA ala Stock,Watson (1988) "Testing for Common Trends"?
  
  # MSB-test statistics under each m_H0, from Silvestre,Surdeanu 2011:6
  A = eigen( eit%*%t(eit)/dim_T^2 )$vectors  # orthogonal (K x K) matrix for rotating e_it into I(0)- and I(1)-series
  MSB.stats = lags = NULL
  for(m_H0 in dim_K:1){
    A2 = A[ , m_H0:1, drop=FALSE]  # eigenvectors (K x m) corresponding to the m_H0 largest eigenvalues, from Silvestre,Surdeanu 2011:32
    e2 = t(A2) %*% eit  # rotated series (m X T) that are I(1) under H0
    
    # determine lag-order by modified information criterion from Qu,Perron (2007)
    R.MIC = NULL
    for(p in 1:lag_max){
      idx_t = (lag_max+1-p):dim_T  # use the same sample for all lags p = 1,...,p_max, from Qu,Perron 2007:644
      def_p = aux_stackRRR(e2[ ,idx_t, drop=FALSE], dim_p=p)  # no deterministic term in idiosyncratic components, from Silvestre,Surdeanu 2011:7
      RRR_p = aux_RRR(def_p$Z0, def_p$Z1, def_p$Z2)
      VEC_p = aux_VECM(beta=RRR_p$V[ , 0, drop=FALSE], RRR=RRR_p)  # m_H0 stochastic trends in e_it under H0 corresponds to r_H0 = 0 in m_H0 time series
      MIC_p = aux_MIC(Ucov=VEC_p$OMEGA, COEF=VEC_p$GAMMA, dim_T=dim_T-lag_max, lambda_H0=RRR_p$lambda) 
      ### MIC under r_H0 = 0  =>  (1) do not count coefficients in \Pi=0 and (2) use all lambdas for trace statistic.
      
      R.MIC = cbind(R.MIC, MIC_p)
      rm(idx_t, def_p, RRR_p, VEC_p, MIC_p)
    }
    dim_p = which.min(R.MIC[MIC, ]) # lag-order by modified information criterion 
    lags  = c(lags, dim_p)
    
    # auxiliary VECM estimation for the long-run covariance matrix
    def_e2 = aux_stackRRR(e2, dim_p=dim_p)  # no deterministic term in idiosyncratic components, from Silvestre,Surdeanu 2011:7
    RRR_e2 = aux_RRR(def_e2$Z0, def_e2$Z1, def_e2$Z2)
    VEC_e2 = aux_VECM(beta=RRR_e2$V[ , 0, drop=FALSE], RRR=RRR_e2)  # m_H0 stochastic trends in e_it under H0 corresponds to r_H0 = 0 in m_H0 time series
    XI1_e2 = aux_vec2vma(GAMMA=VEC_e2$GAMMA, alpha_oc=diag(m_H0), beta_oc=diag(m_H0), dim_p=dim_p, n.ahead="XI")  # \Xi(1) = (I_m - \Gamma_p(1))^{-1}
    
    # calculate MSB-test statistics
    OMEGA2inv = solve( t(XI1_e2) %*% VEC_e2$SIGMA %*% XI1_e2 )  # from Silvestre,Surdeanu 2011:7; see also Ng,Perron 2001:1521, Eq.5
    Q_e2      = e2 %*% t(e2) / dim_T  # covariance matrix (m x m) of stochastic trends
    MSB_eigen = eigen( Q_e2 %*% OMEGA2inv / dim_T )  # from Silvestre,Surdeanu 2011:6, Eq.6
    MSB.stats = c(MSB.stats, min(MSB_eigen$values))
    rm(A2, e2, R.MIC, def_e2, RRR_e2, VEC_e2, XI1_e2, OMEGA2inv, Q_e2, MSB_eigen)
  }
  
  # p-values
  ### left-tailed test, from Silvestre,Surdeanu 2011:7(5.)
  MSB.pvals = NA  # TODO use simulated look-up table, from Silvestre,Surdeanu 2011:11
  ### p-values depend on test statistic, sample size dim_T, number of stochastic trends under H0 and type_MSB
  #if(dim_T < 50){ idx_T = 50 }else{
  #  idx_T = max(sizes[ dim_T >= sizes ]) }  # conservative choice of moments
  ##if test_msb>=pval[i,k+1] and test_msb<pval[i+1,k+1];
  #p1=pval[i,1]; p2=pval[i+1,1]; t1=pval[i,k+1]; t2=pval[i+1,k+1];
  #b=(p2-p1)/(t2-t1);
  #c=p1-b*t1;
  #pv=c+b*test_msb;
  
  # return result
  result = data.frame(r_H0=0:(dim_K-1), m_H0=dim_K:1, lags=lags, stats=MSB.stats, pvals=MSB.pvals, row.names=NULL)
  class(result) = "coint"
  result$args_coint = list(method="MSB-Procedure", eit=eit, type=type_MSB, MIC=MIC)
  return(result)
}
### TODO MSB always uses data "e_it" whose first-differences are centered and scaled!
# R.MSBrank = coint.MSB(eit=sjd, lag_max = 5, MIC="AIC", type_MSB="MSB_trend")


