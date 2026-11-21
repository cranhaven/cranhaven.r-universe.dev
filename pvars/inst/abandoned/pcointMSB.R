

#' @title Panel cointegration rank tests based on the MSB-procedure.
#' @inherit pcoint description
#' @inheritParams pcoint
#' @describeIn pcoint based on the MSB-procedure.
#' @param lag_max Integer or vector of integers. The maximum lag order that is considered 
#'   by the \emph{modified Akaike information criterion} for each individual VAR model in levels.
#' @references Carrion-i-Silvestre, J. L., and Surdeanu, L. (2011): 
#'   "Panel Cointegration Rank Testing with Cross-Section Dependence", 
#'   \emph{Studies in Nonlinear Dynamics & Econometrics}, 15, pp. 1-43.
#' 
#' @examples
#' ### reproduce Silvestre,Surdeanu 2011:27, Ch.6,Tab.11(A) ###
#' data("MDEM")
#' names_k = c("m1", "gdp", "R") # variable names
#' names_i = levels(MDEM$id_i)   # country names
#' L.data  = sapply(names_i, FUN=function(i) 
#'    ts(MDEM[MDEM$id_i==i, names_k], start=1957, frequency=1), 
#'    simplify=FALSE)
#' 
#' R.pcsl = pcoint.SL(L.data,  lags=3, n.factors=3, type="SL_trend")
#' R.pcjo = pcoint.JO(L.data,  lags=3, n.factors=3, type="Case4")  # three factors by panel BIC 
#' R.pmsb = pvars:::pcoint.MSB(L.data, lag_max=6, n.factors=3, type="MSB_trend")
#' rbind(R.pmsb$panel$pvals, R.pcsl$panel$pvals, R.pcjo$panel$pvals)
#' 
#'  @family panel cointegration rank tests
#'  @export
#' 
pcoint.MSB <- function(L.data, lag_max, type=c("MSB_mean", "MSB_trend"), n.factors=FALSE){
  # define and check
  L.dim_T = sapply(L.data, FUN=function(x) nrow(x))
  L.dim_K = sapply(L.data, FUN=function(x) ncol(x))
  dim_N = length(L.data)
  dim_T = L.dim_T[1]
  dim_K = L.dim_K[1]
  
  if(!all(dim_K == L.dim_K)){ stop("The number of variables 'K' must be the same for all individuals!") }
  if(length(lag_max) == dim_N){ lag_max = lag_max }else{ lag_max = rep(lag_max, dim_N) }
  
  names_H0 = paste0("r_H0 = ", 0:(dim_K-1))
  names_pt = c("PMSB_Z", "PMSB_F", "PMSB_C", "Choi_Z") # inverse normal test is not given in Bai,Silvestre (2013)!
  names_i  = names(L.data)
  names(lag_max) = names_i
  
  # defactor the time series for PANIC, from Silvestre,Surdeanu 2011:5
  trend = switch(type, "MSB_mean"=FALSE, "MSB_trend"=TRUE)
  Yit = do.call("cbind", L.data)  # data matrix of dimension (T x K*N)
  if(n.factors>0){
    PCA = aux_ComFact(X=Yit, trend=trend, n.factors=n.factors)  # PANIC (2004) decomposition
    eit = PCA$eit  # panel of idiosyncratic components without deterministic term
    L.data = lapply(1:dim_N, FUN=function(i) eit[ ,dim_K*(i-1) + 1:dim_K])  # overwrite for subsequent VECM estimation
  }else{
    PCA = NULL
    zit = scale(diff(Yit), center=trend, scale=TRUE)  # MSB always uses data whose first-differences are centered and scaled!
    eit = apply(rbind(0, zit), MARGIN=2, FUN=function(x) cumsum(x))
    L.data = lapply(1:dim_N, FUN=function(i) eit[ ,dim_K*(i-1) + 1:dim_K])
  }
  
  # apply MSB-test procedure to each individual
  L.MSBrank = lapply(1:dim_N,   FUN=function(i) coint.MSB(L.data[[i]], type_MSB=type, lag_max=lag_max[i], MIC="AIC"))
  MSB.stats = sapply(L.MSBrank, FUN=function(x) x$stats)  # matrix of trace statistics (K x N) with ordering r_H0 = 0,...,K-1
  MSB.pvals = sapply(L.MSBrank, FUN=function(x) x$pvals)  # matrix of p-values (K x N) with ordering r_H0 = 0,...,K-1
  MSB.lags  = sapply(L.MSBrank, FUN=function(x) x$lags)   # matrix of lags (K x N) in auxiliary VECM for each r_H0 = 0,...,K-1
  
  # apply panel tests
  sizes = c(50, 100, 200, 500, 1000)  # available sample sizes of simulated MSB moments
  if(dim_T < 50){ idx_T = 50 }else{
    idx_T = max(sizes[ dim_T >= sizes ]) }  # conservative choice of moments
  idx_MSB = paste0(type, idx_T)  # choose moments for MSB according to deterministic type and sample size
  moments = coint_moments[[idx_MSB]][dim_K:1, , drop=FALSE]
  PMSB_Z = ptest.STATSbar(STATS=MSB.stats, distribution="theoretical", moments=moments) # from Silvestre,Surdeanu 2011:11, Eq.8
  PMSB_C = ptest.METApval(PVALS=MSB.pvals, distribution="theoretical")  # from Silvestre,Surdeanu 2011:11, Eq.9
  
  # return result
  indiv = list(stats=t(MSB.stats), pvals=t(MSB.pvals), lags=t(MSB.lags))
  panel = list(stats=t(cbind(PMSB_Z$stats, PMSB_C$stats)), pvals=t(
    cbind(1-PMSB_Z$pvals, PMSB_C$pvals))) # PMSB_Z is left-tailed, while ptest.STATSbar() is a rigth-tailed test applied to the symmetric normal distribution!
  
  dimnames(indiv$stats) = dimnames(indiv$pvals) = dimnames(indiv$lags) = list(names_i, names_H0)
  dimnames(panel$stats) = dimnames(panel$pvals) = list(names_pt, names_H0)
  
  argues = list(method="MSB-Test Procedure", type=type, n.factors=n.factors)
  result = list(panel=panel, individual=indiv, CSD=PCA, args_pcoint=argues)
  class(result) = "pcoint"
  return(result)
}


