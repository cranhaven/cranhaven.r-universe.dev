

#' @title Identification of SVAR models by means of proxy variables
#' @description Given an estimated VAR model, this function uses proxy variables
#'   to partially identify the structural impact matrix \eqn{B} 
#'   of the corresponding SVAR model
#'   \deqn{y_{t} = c_{t} + A_{1} y_{t-1} + ... + A_{p} y_{t-p} + u_{t}}
#'   \deqn{      = c_{t} + A_{1} y_{t-1} + ... + A_{p} y_{t-p} + B \epsilon_{t}.}
#'   In general, identification procedures determine \eqn{B} up to 
#'   column ordering, scale, and sign. For a unique solution, \code{id.iv} 
#'   follows the literature on proxy SVAR. 
#'   The \eqn{S} columns in \eqn{B = [B_1 : B_2]} of the identified shocks 
#'   \eqn{\epsilon_{ts}, s=1,\ldots,S,} are ordered first, and the variance 
#'   \eqn{\sigma^2_{\epsilon,s} = 1} is normalized to unity (see e.g. Lunsford 
#'   2015:6, Eq. 9). Further, the sign is fixed to a positive correlation 
#'   between proxy and shock series. A normalization of the impulsed shock 
#'   that may fix the size of the impact response in the IRF can be imposed 
#'   subsequently via '\code{normf}' in \code{\link{irf.varx}} and 
#'   \code{\link{sboot.mb}}.
#'   
#' @param x VAR object of class '\code{varx}' or any other 
#'   that will be \link[=as.varx]{coerced} to '\code{varx}'. 
#' @param iv Matrix. A \eqn{(L \times T)} data matrix of the \eqn{L} proxy 
#'   time series \eqn{m_t}. 
#' @param S2 Character. Identification within multiple proxies \eqn{m_t} 
#'   via '\code{MR}' for lower-triangular \eqn{[I_S : -B_{11} B_{12}^{-1} ] B_{1}} by Mertens, Ravn (2013), 
#'   via '\code{JL}' for chol\eqn{(\Sigma_{mu} \Sigma_{u}^{-1} \Sigma_{um})} by Jentsch, Lunsford (2021), or
#'   via '\code{NQ}' for the nearest orthogonal matrix from \code{\link[base]{svd}} decomposition by Empting et al. (2025). 
#'   In case of \eqn{S=L=1} proxy, all three choices provide identical results on \eqn{B_1}.
#' @param cov_u Character. Selection of the estimated residual covariance matrix \eqn{\hat{\Sigma}_{u}} 
#'   to be used in the identification procedure. 
#'   Either \code{'OMEGA'} (the default) for \eqn{\hat{U} \hat{U}'/T_i} as used in Mertens, Ravn (2013) and Jentsch, Lunsford (2021)
#'   or \code{'SIGMA'} for \eqn{\hat{U}\hat{U}'/(T-n_{z})}, which corrects for the number of regressors \eqn{n_z}. 
#'   Both character options refer to the name of the respective estimate in the '\code{varx}' object.
#' @param R0 Matrix. A \eqn{(L \times S)} selection matrix for '\code{NQ}' that 
#'   governs the attribution of the \eqn{L} proxies to their specific \eqn{S} 
#'   structural shock series. If \code{NULL} (the default), \code{R0} 
#'   \eqn{= I_S} will be used such that the \eqn{S=L} columns of \eqn{B_1} are 
#'   one-by-one estimated from the \eqn{S=L} proxy series '\code{iv}' available. 
#' 
#' @return List of class '\code{\link[=as.varx]{id}}'.
#' 
#' @references Mertens, K., and Ravn, M. O. (2013):
#'   "The Dynamic Effects of Personal and Corporate Income Tax Changes in the 
#'   United States", \emph{American Economic Review}, 103, pp. 1212-1247.
#' @references Lunsford, K. G. (2015):
#'   "Identifying Structural VARs with a Proxy Variable and a Test for a Weak Proxy",
#'   Working Paper, No 15-28, Federal Reserve Bank of Cleveland.
#' @references Jentsch, C., and Lunsford, K. G. (2019):
#'   "The Dynamic Effects of Personal and Corporate Income Tax Changes in the 
#'   United States: Comment", \emph{American Economic Review}, 109, pp. 2655-2678.
#' @references Jentsch, C., and Lunsford, K. G. (2021):
#'   "Asymptotically Valid Bootstrap Inference for Proxy SVARs",
#'   \emph{Journal of Business and Economic Statistics}, 40, pp. 1876-1891.
#' @references Empting, L. F. T., Maxand, S., Oeztuerk, S., and Wagner, K. (2025): 
#'   "Inference in Panel SVARs with Cross-Sectional Dependence of Unknown Form".
#' @seealso \ldots the individual identification approaches 
#'   by Lange et al. (2021) in \strong{svars}.
#' @family identification functions
#' @example inst/examples/id_iv.R
#' @export
#' 
id.iv <- function(x, iv, S2=c("MR", "JL", "NQ"), cov_u="OMEGA", R0=NULL){
  # define
  x = as.varx(x)
  
  # identify via proxy variables
  R.id = aux_idIV(u=x$resid, m=iv, S2=S2, SIGMA_uu=x[[cov_u]], R0=R0)
  
  # return result
  x$B = R.id$B1  # estimated B matrix (partially identified by "JL" and "MR")
  x$Q = R.id$Q   # estimated orthogonal matrix (only by "NQ")
  x$udv = R.id$udv   # SVD results (only by "NQ")
  x$F_stats = R.id$F_stats  # F-statistic on proxy relevance
  x$args_id = list(method="Proxy", iv=R.id$m, S2=S2, cov_u=cov_u, R0=R0)
  class(x)  = c("id", "varx")
  return(x)
}


#' @title Identification of SVEC models by imposing long- and short-run restrictions
#' @description Identifies an SVEC model by utilizing a scoring algorithm 
#'   to impose long- and short-run restrictions. 
#'   See the details of \code{\link[vars]{SVEC}} in \strong{vars}.
#' @param x VAR object of class '\code{varx}' estimated under rank-restriction 
#'   or any other that will be \link[=as.varx]{coerced} to '\code{varx}'.
#' @param LR Matrix. The restricted long-run impact matrix.
#' @param SR Matrix. The restricted contemporaneous impact matrix.
#' @param start Vector. The starting values for \eqn{\gamma}, 
#'   set by \code{\link[stats]{rnorm}} if \code{NULL} (the default).
#' @param max.iter Integer. The maximum number of iterations.
#' @param conv.crit Real number. Convergence value of algorithm.
#' @param maxls Real number. Maximum movement of the parameters between two iterations of the scoring algorithm.
#' 
#' @return List of class '\code{\link[=as.varx]{id}}'.
#' 
#' @references Amisano, G. and Giannini, C. (1997): 
#'   \emph{Topics in Structural VAR Econometrics}, 
#'   Springer, 2nd ed.
#' @references Breitung, J., Brueggemann R., and Luetkepohl, H. (2004): 
#'   "Structural Vector Autoregressive Modeling and Impulse Responses", 
#'   in \emph{Applied Time Series Econometrics}, 
#'   ed. by H. Luetkepohl and M. Kraetzig, 
#'   Cambridge University Press, Cambridge.
#' @references Johansen, S. (1996): 
#'   \emph{Likelihood-Based Inference in Cointegrated Vector Autoregressive Models}, 
#'   Advanced Texts in Econometrics, Oxford University Press, USA.
#' @references Luetkepohl, H. (2005): 
#'   \emph{New Introduction to Multiple Time Series Analysis}, 
#'   Springer, 2nd ed.
#' @references Pfaff, B. (2008):
#'   "VAR, SVAR and SVEC Models: Implementation within R Package \strong{vars}",
#'   \emph{Journal of Statistical Software}, 27, pp. 1-32.
#' @seealso \ldots the original \code{\link[vars]{SVEC}} by Pfaff (2008) in \strong{vars}. 
#'   Note that \code{\link{id.grt}} is just a graftage, but allows for the additional 
#'   model specifications in \code{\link{VECM}} and for the bootstrap procedures 
#'   in \code{\link{sboot.mb}}, both provided by the \strong{pvars} package.
#' 
#' @examples
#' ### reproduce basic example in "vars" ###
#' library(vars)
#' data("Canada")
#' names_k = c("prod", "e", "U", "rw")  # variable names
#' names_s = NULL  # optional shock names
#' 
#' # colnames of the restriction matrices are passed as shock names #
#' SR = matrix(NA, nrow=4, ncol=4, dimnames=list(names_k, names_s))
#' SR[4, 2] = 0
#' LR = matrix(NA, nrow=4, ncol=4, dimnames=list(names_k, names_s))
#' LR[1, 2:4] = 0
#' LR[2:4, 4] = 0
#' 
#' # estimate and identify SVECM #
#' R.vecm = VECM(y=Canada[ , names_k], dim_p=3, dim_r=1, type="Case4")
#' R.grt  = id.grt(R.vecm, LR=LR, SR=SR)
#' 
#' @family identification functions
#' @export
#' 
id.grt <- function(x, LR=NULL, SR=NULL, start=NULL, max.iter=100, conv.crit=1e-07, maxls=1.0){
  # define
  x = as.varx(x)
  dim_K = x$dim_K  # number of endogenous variables
  dim_p = x$dim_p  # number of lags
  dim_T = x$dim_T  # number of observations without presample
  
  # calculate long-run multiplier matrix
  alpha_oc = MASS::Null(x$VECM$alpha)
  beta_oc  = MASS::Null(x$beta[1:dim_K, ])
  XI = aux_vec2vma(GAMMA=x$VECM$GAMMA, alpha_oc=alpha_oc, beta_oc=beta_oc, dim_p=dim_p, B=NULL, n.ahead="XI")
  
  # identify via FIML scoring algorithm
  GRT = aux_idGRT(OMEGA=x$VECM$OMEGA, XI=XI, dim_T=dim_T, LR=LR, SR=SR, 
                  start=start, max.iter=max.iter, conv.crit=conv.crit, maxls=maxls)
  
  # return result
  x$B  = GRT$SR  # estimated B matrix (unique decomposition of the covariance matrix)
  x$SR = GRT$SR  # structural effects on impact
  x$LR = GRT$LR  # structural long-run effects
  x$args_id = list(method="SVECM", SR=SR, LR=LR, start=start, max.iter=max.iter, conv.crit=conv.crit, maxls=maxls)
  class(x)  = c("id", "varx")
  return(x)
}


