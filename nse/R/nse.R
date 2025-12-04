#' @name nse
#' @docType package
#' @title nse: Computation of numerical standard errors in R
#' @description \code{nse} (Ardia and Bluteau, 2017) is an \R package for computing the numerical standard error (NSE), an estimate
#' of the standard deviation of a simulation result, if the simulation experiment were to be repeated
#' many times. The package provides a set of wrappers around several R packages, which give access to
#' more than thirty NSE estimators, including batch means
#' estimators (Geyer, 1992, Section 3.2), initial sequence estimators Geyer (1992, Equation 3.3),
#' spectrum at zero estimators (Heidelberger and Welch, 1981), heteroskedasticity
#' and autocorrelation consistent (HAC) kernel estimators (Newey and West, 1987; Andrews, 1991; Andrews and
#' Monahan, 1992; Newey and West, 1994; Hirukawa, 2010), and bootstrap estimators Politis and
#' Romano (1992, 1994); Politis and White (2004). The full set of estimators is described in
#' Ardia et al. (2018).
#'
#' @section Functions:
#' \itemize{
#' \item \code{\link{nse.geyer}}: Geyer NSE estimator.
#' \item \code{\link{nse.spec0}}: Spectral density at zero NSE estimator.
#' \item \code{\link{nse.nw}}: Newey-West NSE estimator.
#' \item \code{\link{nse.andrews}}: Andrews NSE estimator.
#' \item \code{\link{nse.hiruk}}: Hirukawa NSE estimator.
#' \item \code{\link{nse.boot}}: Bootstrap NSE estimator.
#' }
#' @author David Ardia and Keven Bluteau
#' @note Functions rely on the packages \code{coda}, \code{mcmc},\code{mcmcse}, \code{np}, and \code{sandwich}.
#' @note Please cite the package in publications. Use \code{citation("nse")}.
#' @references
#' Andrews, D.W.K. (1991).
#' Heteroskedasticity and autocorrelation consistent covariance matrix estimation.
#' \emph{Econometrica} \bold{59}(3),  817-858.
#'
#' Andrews, D.W.K, Monahan, J.C. (1992).
#' An improved heteroskedasticity and autocorrelation consistent covariance matrix estimator.
#' \emph{Econometrica} \bold{60}(4),  953-966.
#'
#' Ardia, D., Bluteau, K., Hoogerheide, L. (2018).
#' Methods for computing numerical standard errors: Review and application to Value-at-Risk estimation.
#' \emph{Journal of Time Series Econometrics} \bold{10}(2), 1-9.
#' \doi{10.1515/jtse-2017-0011}
#' \doi{10.2139/ssrn.2741587}
#'
#' Ardia, D., Bluteau, K. (2017).
#' nse: Computation of numerical standard errors in R.
#' \emph{Journal of Open Source Software} \bold{10}(2).
#' \doi{10.21105/joss.00172}
#'
#' Geyer, C.J. (1992).
#' Practical Markov chain Monte Carlo.
#' \emph{Statistical Science} \bold{7}(4),  473-483.
#'
#' Heidelberger, P., Welch, Peter D. (1981).
#' A spectral method for confidence interval generation and run length control in simulations.
#' \emph{Communications of the ACM} \bold{24}(4),  233-245.
#'
#' Hirukawa, M. (2010).
#' A two-stage plug-in bandwidth selection and its implementation for covariance estimation.
#' \emph{Econometric Theory} \bold{26}(3),  710-743.
#'
#' Newey, W.K., West, K.D. (1987).
#' A simple, positive semi-definite, heteroskedasticity and autocorrelationconsistent covariance matrix.
#' \emph{Econometrica} \bold{55}(3),  703-708.
#'
#' Newey, W.K., West, K.D. (1994) .
#' Automatic lag selection in covariance matrix estimation.
#' \emph{Review of Economic Studies} \bold{61}(4), 631-653.
#'
#' Politis, D.N., Romano, and J.P. (1992).
#' A circular block-resampling procedure for stationary data.
#' In \emph{Exploring the limits of bootstrap}, John Wiley & Sons, 263-270.
#'
#' Politis, D.N., Romano, and J.P. (1994).
#' The stationary bootstrap.
#' \emph{Journal of the American Statistical Association} \bold{89}(428), 1303-1313.
#'
#' Politis, D.N., White, H. (2004).
#' Automatic block-length selection for the dependent bootstrap.
#' \emph{Econometric Reviews} \bold{23}(1), 53-70.
#' @import coda mcmc mcmcse np sandwich
#' @useDynLib nse, .registration = TRUE
"_PACKAGE"

#' @name nse.geyer
#' @title Geyer estimator
#' @description Function which calculates the numerical standard error with the method of Geyer (1992).
#' @param x A numeric vector.
#' @param type The type which can be either \code{"iseq"}, \code{"bm"}, \code{"obm"} or \code{"iseq.bm"}.
#' See *Details*. Default is \code{type = "iseq"}.
#' @param nbatch Number of batches when \code{type = "bm"} and \code{type = "iseq.bm"}. Default is \code{nbatch = 30}.
#' @param iseq.type Constraints on function: \code{"pos"} for nonnegative, \code{"dec"} for nonnegative
#' and nonincreasing, and \code{"con"} for nonnegative, nonincreasing, and convex. Default is \code{iseq.type = "pos"}.
#' @details The type \code{"iseq"} gives the positive intial sequence estimator, \code{"bm"} is the batch mean estimator,
#' \code{"obm"} is the overlapping batch mean estimator and \code{"iseq.bm"} is a combination of \code{"iseq"} and \code{"bm"}.
#' @return  The NSE estimator.
#' @note \code{nse.geyer} relies on the packages \code{\link{mcmc}} and \code{\link{mcmcse}}; see
#' the documentation of these packages for more details.
#' @author David Ardia and Keven Bluteau
#' @references
#' Geyer, C.J. (1992).
#' Practical Markov chain Monte Carlo.
#' \emph{Statistical Science} \bold{7}(4),  .473-483.
#' @export
#' @import mcmc mcmcse
#' @examples
#' \dontrun{
#' n    = 1000
#' ar   = 0.9
#' mean = 1
#' sd   = 1
#
#' set.seed(1234)
#' x = c(arima.sim(n = n, list(ar = ar), sd = sd) + mean)
#
#' nse.geyer(x = x, type = "bm", nbatch = 30)
#' nse.geyer(x = x, type = "obm", nbatch = 30)
#' nse.geyer(x = x, type = "iseq", iseq.type = "pos")
#' nse.geyer(x = x, type = "iseq.bm", iseq.type = "con")
#' }
nse.geyer = function(x,
                     type = c("iseq", "bm", "obm", "iseq.bm"),
                     nbatch = 30,
                     iseq.type = c("pos", "dec", "con")) {
  if (is.vector(x)) {
    x = matrix(data = x, ncol = 1)
  }
  f.error.multivariate(x)
  n = dim(x)[1]
  
  type = type[1]
  if (type == "iseq") {
    f.error.multivariate(x)
    
    if (iseq.type == "pos") {
      var.iseq = mcmc::initseq(x = x)$var.pos
    } else if (iseq.type == "dec") {
      var.iseq = mcmc::initseq(x = x)$var.dec
    } else if (iseq.type == "con") {
      var.iseq = mcmc::initseq(x = x)$var.con
    } else {
      stop("Invalid iseq.type : must be one of c('pos','dec','con')")
    }
    
    iseq = var.iseq / n # Intial sqequence Geyer (1992)
    out = iseq
    
  } else if (type == "bm") {
    ncol  = dim(x)[2]
    x     = as.data.frame(x)
    batch = matrix(unlist(lapply(
      split(x, ceiling(seq_along(x[, 1]) / (n / nbatch))),
      FUN = function(x)
        colMeans(x)
    )), ncol = ncol, byrow = TRUE)
    out   = stats::var(x = batch) / (nbatch - 1)
    
    if (is.matrix(out) && dim(out) == c(1, 1)) {
      out = as.numeric(out)
    }
    
  } else if (type == "obm") {
    out = as.numeric(mcmcse::mcse(x, method = "obm")$se ^ 2)
    
  } else if (type == "iseq.bm") {
    f.error.multivariate(x)
    batch = unlist(lapply(split(x, ceiling(
      seq_along(x) / (n / nbatch)
    )), FUN = mean))
    
    iseq.type = iseq.type[1]
    if (iseq.type == "pos") {
      var.iseq = mcmc::initseq(batch)$var.pos
    } else if (iseq.type == "dec") {
      var.iseq = mcmc::initseq(x = batch)$var.dec
    } else if (iseq.type == "con") {
      var.iseq = mcmc::initseq(x = batch)$var.con
    } else {
      stop("Invalid iseq.type : must be one of c('pos','dec','con')")
    }
    
    iseq.bm = var.iseq / nbatch
    out = iseq.bm
    
  } else {
    stop("Invalid type : must be of type c('iseq','bm','iseq.bm')")
  }
  out = unname(out)
  # nse from variance
  out = sqrt(out)
  out = as.numeric(out)
  return(out)
}

#' @name nse.spec0
#' @title Spectral density at zero estimator
#' @description Function which calculates the numerical standard error with the spectrum at zero estimator.
#' @param x A numeric vector.
#' @param type Method to use in estimating the spectral density function, among \code{"ar"}, \code{"glm"}, \code{"daniell"},
#'  \code{"modified.daniell"}, \code{"tukey-hanning"},
#'   \code{"parzen"}, \code{"triweight"},
#'    \code{"bartlett-priestley"}, \code{"triangular"}, and \code{"qs"}. See *Details*.
#' Default is \code{type = "ar"}.
#' @param lag.prewhite Prewhite the series before analysis (integer or \code{NULL}). When \code{lag.prewhite = NULL} this performs automatic lag selection. Default is \code{lag.prewhite = 0} that is no prewhitening.
#' @param welch Use Welch's method (Welsh, 1967) to estimate the spectral density.
#' @param steep Use steep or sharp version of the kernel (Phillips et al., 2006) (only available for type: \code{"qs"},\code{"triangular"}, and \code{"parzen"}). \code{lag.prewhite} must be set to 0 to use steep version.
#' @details Welsh's method use 50\% overlap and 8 sub-samples.
#' The method \code{"ar"} estimates the spectral density using an autoregressive model,
#' \code{"glm"} using a generalized linear model Heidelberger & Welch (1981), 
#'  \code{"daniell"} uses daniell window from the \R kernel function,
#'  \code{"modified.daniell"} uses daniell window the \R kernel function,
#'  \code{"tukey-hanning"} uses the tukey-hanning window,
#'  \code{"parzen"} uses the parzen window,
#'  \code{"triweight"} uses the triweight window,
#'  \code{"bartlett-priestley"} uses the Bartlett-Priestley window,
#'  \code{"triangular"} uses the triangular window, and
#'  \code{"qs"} uses the quadratic-spectral window,
#' @note \code{nse.spec0} relies on the packages \code{coda}; see the documentation of this package for more details.
#' @return The NSE estimator.
#' @references
#' Heidelberger, P., Welch, Peter D. (1981).
#' A spectral method for confidence interval generation and run length control in simulations.
#' \emph{Communications of the ACM} \bold{24}(4), 233-245.
#' 
#' Phillips, P. C., Sun, Y., & Jin, S. (2006).
#'Spectral density estimation and robust hypothesis testing using steep origin kernels without truncation.
#'\emph{International Economic Review}, \bold{47}(3), 837-894.
#'  
#' Welch, P. D. (1967),
#' The use of Fast Fourier Transform for the estimation of power spectra: A method based on time averaging over short, modified periodograms.
#' \emph{IEEE Transactions on Audio and Electroacoustics}, \bold{AU-15}(2): 70-73,
#' 
#' Hurvich, C. M. (1985).
#'  Data-driven choice of a spectrum estimate: extending the applicability of cross-validation methods.
#'  \emph{Journal of the American Statistical Association}, \bold{80}(392), 933-940.
#' 
#' @details This kernel based variance estimator apply weights to smooth out the spectral density using a kernel and takes the spectral density at frequency zero which is equivalent to the variance of the serie. Bandwidth for the kernel is automatically selected using cross-validatory methods (Hurvich, 1985).
#' @author David Ardia and Keven Bluteau
#' @import coda
#' @export
#' @examples
#' \dontrun{
#' n    = 1000
#' ar   = 0.9
#' mean = 1
#' sd   = 1
#' set.seed(1234)
#' x = c(arima.sim(n = n, list(ar = ar), sd = sd) + mean)
#'
#' nse.spec0(x = x, type = "parzen", lag.prewhite = 0, welch = TRUE, steep = TRUE)
#' }
nse.spec0 = function(x,
                     type = c(
                       "ar",
                       "glm",
                       "daniell",
                       "modified.daniell",
                       "tukey-hanning",
                       "parzen",
                       "triweight",
                       "bartlett-priestley",
                       "triangular",
                       "qs"
                     ),
                     lag.prewhite = 0,
                     welch = FALSE,
                     steep = FALSE) {
  scale = 1
  if (is.vector(x)) {
    x = matrix(data = x, ncol = 1)
  }
  # DA for simplicity, we currently consider univariate time series but could extend later
  f.error.multivariate(x)
  if ((isTRUE(steep)) && (lag.prewhite != 0)) {
    warning(
      "setting lag.prewhite to 0 as steep kernel is not compatible with prewhitening since it is taken care directly by the kernel"
    )
  }
  n = dim(x)[1]
  tmp   = f.prewhite(x, ar.order = lag.prewhite)
  x     = tmp$ar.resid
  scale = tmp$scale
  
  type = type[1]
  x = as.ts(x)
  if (isTRUE(welch)) {
    spec0 = f.welch(
      y = x,
      blocksize = NULL,
      overlap = 0.5,
      type = type,
      steep = steep
    )
  } else if (type == "ar") {
    spec0 = coda::spectrum0.ar(x = x)$spec
  } else if (type == "glm") {
    spec0 = coda::spectrum0(x = x)$spec
  } else {
    m = f.optimal_h(x, type = type)
    kern = f.kernel_addon(type = type,
                                m,
                                steep = steep,
                                y = x)
    spec0 = spectrum(x,
                     kernel = kern,
                     taper = 0.5,
                     plot = FALSE)[[2]][1]
  }
  spec0 = spec0 * scale
  out   = spec0 / n
  out   = unname(out)
  # nse
  out = sqrt(out)
  out = as.numeric(out)
  return(out)
}

#' @name nse.nw
#' @title Newey-West estimator
#' @description Function which calculates the numerical standard error with the Newey West (1987, 1994) HAC estimator.
#' @param x A numeric vector
#' @param lag.prewhite Prewhite the series before analysis (integer or \code{NULL}). When \code{lag.prewhite = NULL} this performs automatic lag selection. Default is \code{lag.prewhite = 0} that is no prewhitening.
#' @return The NSE estimator.
#' @note \code{nse.nw} is a wrapper around \code{\link[sandwich]{lrvar}} from
#' the \code{\link{sandwich}} package. See the documentation of \code{\link{sandwich}} for details.
#' @author David Ardia and Keven Bluteau
#' @references
#' Newey, W.K., West, K.D. (1987).
#' A simple, positive semi-definite, heteroskedasticity and autocorrelationconsistent covariance matrix.
#' \emph{Econometrica} \bold{55}(3),  .703-708.
#'
#' Newey, W.K., West, K.D. (1994) .
#' Automatic lag selection in covariance matrix estimation.
#' \emph{Review of Economic Studies} \bold{61}(4), .631-653.
#' @import sandwich
#' @export
#' @examples
#' \dontrun{
#' n    = 1000
#' ar   = 0.9
#' mean = 1
#' sd   = 1
#'
#' set.seed(1234)
#' x = c(arima.sim(n = n, list(ar = ar), sd = sd) + mean)
#'
#' nse.nw(x = x, lag.prewhite = 0)
#' nse.nw(x = x, lag.prewhite = 1)
#' nse.nw(x = x, lag.prewhite = NULL)
#' }
nse.nw <- function(x, lag.prewhite = 0) {
  f.error.multivariate(x)
  tmp = f.prewhite(x, ar.order = lag.prewhite)
  lag.prewhite = tmp$ar.order
  out = sandwich::lrvar(
    x = x,
    type = "Newey-West",
    prewhite = lag.prewhite,
    adjust = TRUE
  )
  out = unname(out)
  # nse
  out = sqrt(out)
  out = as.numeric(out)
  return(out)
}

#' @name nse.andrews
#' @title Andrews estimator
#' @description Function which calculates the numerical standard error with the kernel
#' based variance estimator by Andrews (1991).
#' @details This kernel based variance estimation apply weight to the auto-covariance function with a kernel and sums up the value.
#' @param x A numeric vector.
#' @param type The type of kernel used among which \code{"bartlett"}, \code{"parzen"}, \code{"qs"}, \code{"trunc"} and \code{"tukey"}. Default is \code{type = "bartlett"}.
#' @param lag.prewhite Prewhite the series before analysis (integer or \code{NULL}). When \code{lag.prewhite = NULL} this performs automatic lag selection. Default is \code{lag.prewhite = 0} that is no prewhitening.
#' @param approx Andrews approximation, either \code{"AR(1)"} or \code{"ARMA(1,1)"}. Default is \code{approx = "AR(1)"}.
#' @return The NSE estimator.
#' @note \code{nse.andrews} is a wrapper around \code{\link[sandwich]{lrvar}} from the \code{\link{sandwich}} package and uses Andrews (1991) automatic bandwidth estimator. See the documentation of \code{\link{sandwich}} for details.
#' @author David Ardia and Keven Bluteau
#' @references
#' Andrews, D.W.K. (1991).
#' Heteroskedasticity and autocorrelation consistent covariance matrix estimation.
#' \emph{Econometrica} \bold{59}(3),  817-858.
#'
#' Andrews, D.W.K, Monahan, J.C. (1992).
#' An improved heteroskedasticity and autocorrelation consistent covariance matrix estimator.
#' \emph{Econometrica} \bold{60}(4),  953-966.
#'
#' Newey, W.K., West, K.D. (1987).
#' A simple, positive semi-definite, heteroskedasticity and autocorrelationconsistent covariance matrix.
#' \emph{Econometrica} \bold{55}(3),  703-708.
#'
#' Newey, W.K., West, K.D. (1994) .
#' Automatic lag selection in covariance matrix estimation.
#' \emph{Review of Economic Studies} \bold{61}(4),  631-653.
#' @import sandwich
#' @export
#' @examples
#' \dontrun{
#' n    = 1000
#' ar   = 0.9
#' mean = 1
#' sd   = 1
#'
#' set.seed(1234)
#' x = c(arima.sim(n = n, list(ar = ar), sd = sd) + mean)
#'
#'nse.andrews(x = x, type = "parzen", lag.prewhite = 0)
#'nse.andrews(x = x, type = "tukey", lag.prewhite = 1)
#'nse.andrews(x = x, type = "qs", lag.prewhite = NULL)
#'}
nse.andrews <-
  function(x,
           type = c("bartlett", "parzen", "tukey", "qs", "trunc"),
           lag.prewhite = 0,
           approx = c("AR(1)", "ARMA(1,1)")) {
    f.error.multivariate(x)
    tmp = f.prewhite(x, ar.order = lag.prewhite)
    lag.prewhite = tmp$ar.order
    type.sandwich = f.type.sandwich(type.in = type)
    out = sandwich::lrvar(
      x = x,
      type = "Andrews",
      prewhite = lag.prewhite,
      adjust = TRUE,
      kernel = type.sandwich,
      approx = approx
    )
    out = unname(out)
    # nse
    out = sqrt(out)
    out = as.numeric(out)
    return(out)
  }

#' @name nse.hiruk
#' @title Hirukawa estimator
#' @description Function which calculates the numerical standard error with the kernel based variance estimator
#' by Andrews (1991) using Hirukawa (2010) automatic bandwidth estimator.
#' @param x A numeric vector.
#' @param lag.prewhite Prewhite the series before analysis (integer or \code{NULL}). When \code{lag.prewhite = NULL} this performs automatic lag selection. Default is \code{lag.prewhite = 0} that is no prewhitening.
#' @param type The type of kernel used among \code{"bartlett"} and \code{"parzen"}. Default is \code{type = "Bartlett"}.
#' @return The NSE estimator.
#' @note \code{nse.hiruk} is a wrapper around \code{\link[sandwich]{lrvar}} from
#' the \code{\link{sandwich}} package and uses Hirukawa (2010) bandwidth estimator.
#' See the documentation of \code{\link{sandwich}} for details.
#' @author David Ardia and Keven Bluteau
#' @references
#' Hirukawa, M. (2010).
#' A two-stage plug-in bandwidth selection and its implementation for covariance estimation.
#' \emph{Econometric Theory} \bold{26}(3),  710-743.
#' @import sandwich
#' @export
#' @examples
#' \dontrun{
#' n    = 1000
#' ar   = 0.9
#' mean = 1
#' sd   = 1
#'
#' set.seed(1234)
#' x = c(arima.sim(n = n, list(ar = ar), sd = sd) + mean)
#' nse.hiruk(x = x, type = "parzen", lag.prewhite = 0)
#' nse.hiruk(x = x, type = "bartlett", lag.prewhite = NULL)
#' }
nse.hiruk <-
  function(x,
           type = c("bartlett", "parzen"),
           lag.prewhite = 0) {
    f.error.multivariate(x)
    tmp = f.prewhite(x, ar.order = lag.prewhite)
    lag.prewhite = tmp$ar.order
    bandwidth = f.hiruk.bandwidth.solve(x, type = type, lag.prewhite = lag.prewhite)
    type.sandwich = f.type.sandwich(type.in = type)
    out = sandwich::lrvar(
      x = x,
      type = "Andrews",
      prewhite = lag.prewhite,
      adjust = TRUE,
      kernel = type.sandwich,
      bw = bandwidth
    )
    out = unname(out)
    # nse
    out = sqrt(out)
    out = as.numeric(out)
    return(out)
  }

#' @name nse.boot
#' @title Bootstrap estimator
#' @description Function which calculates the numerical standard error with bootstrap estimator.
#' @param x  A numeric vector.
#' @param nb The number of bootstrap replications.
#' @param type The bootstrap scheme used, among \code{"stationary"} and \code{"circular"}. Default is \code{type = "stationary"}.
#' @param b The block length for the block bootstrap. If \code{NULL} automatic block length selection. Default is \code{b = NULL}.
#' @param lag.prewhite Prewhite the series before analysis (integer or \code{NULL}). When \code{lag.prewhite = NULL} this performs automatic lag selection. Default is \code{lag.prewhite = 0} that is no prewhitening.
#' @return The NSE estimator.
#' @note \code{nse.boot} uses \code{\link[np]{b.star}} of the \code{\link{np}} package
#' for the optimal block length selection.
#' @author David Ardia and Keven Bluteau
#' @references
#' Politis, D.N., Romano, and J.P. (1992).
#' A circular block-resampling procedure for stationary data.
#' In \emph{Exploring the limits of bootstrap}, John Wiley & Sons,  263-270.
#'
#' Politis, D.N., Romano, and J.P. (1994).
#' The stationary bootstrap.
#' \emph{Journal of the American Statistical Association} \bold{89}(428),  1303-1313.
#'
#' Politis, D.N., White, H. (2004).
#' Automatic block-length selection for the dependent bootstrap.
#' \emph{Econometric Reviews} \bold{23}(1),  53-70.
#' @import np Rcpp stats
#' @importFrom Rcpp evalCpp
#' @export
#' @examples
#' \dontrun{
#' n    = 1000
#' ar   = 0.9
#' mean = 1
#' sd   = 1
#'
#' set.seed(1234)
#' x = c(arima.sim(n = n, list(ar = ar), sd = sd) + mean)
#'
#' set.seed(1234)
#' nse.boot(x = x, nb = 1000, type = "stationary", b = NULL, lag.prewhite = 0)
#' nse.boot(x = x, nb = 1000, type = "circular", b = NULL, lag.prewhite = NULL)
#' nse.boot(x = x, nb = 1000, type = "circular", b = 10, lag.prewhite = NULL)
#' }
nse.boot <-
  function(x,
           nb,
           type = c("stationary", "circular"),
           b = NULL,
           lag.prewhite = 0) {
    scale = 1
    f.error.multivariate(x)
    x = as.numeric(x)
    # prewhiteneing
    tmp   = f.prewhite(x, ar.order = lag.prewhite)
    x     = tmp$ar.resid
    scale = tmp$scale
    
    # optimal block length selection
    if (is.null(b)) {
      blockSize = np::b.star(data = x, round = TRUE)
      if (type == "stationary") {
        b = as.numeric(blockSize[1, 1])
      } else if (type == "circular") {
        b = as.numeric(blockSize[1, 2])
      }
    }
    
    out = scale * stats::var(f.bootstrap(
      x = x,
      nb = nb,
      statistic = colMeans,
      b = b,
      type = type
    )$statistic)
    out = as.numeric(out)
    out = unname(out)
    # nse
    out = sqrt(out)
    out = as.numeric(out)
    return(out)
  }
#' @name nse.cos
#' @title Long-run variance estimation using low-frequency cosine series.
#' @description Function which calculates the numerical standard error with low-frequency cosine weighted averages of the original serie.
#' @param x A numeric vector.
#' @param q Number of consine series.
#' @param lag.prewhite Prewhite the series before analysis (integer or \code{NULL}). When \code{lag.prewhite = NULL} this performs automatic lag selection. Default is \code{lag.prewhite = 0} that is no prewhitening.
#' @details The method estimate the series with a linear regression using cosine low frequency series. It than derived the NSE from the coefficient of the cosine series (Ulrich and Watson, 2017).
#' @return The NSE estimator.
#' @references
#' Muller, Ulrich K., and Mark W. Watson. (2015)
#' Low-frequency econometrics.
#' \emph{National Bureau of Economic Research}, No. w21564.
#' @author David Ardia and Keven Bluteau
#' @export
#' @examples
#' \dontrun{
#' n    = 1000
#' ar   = 0.9
#' mean = 1
#' sd   = 1
#' set.seed(1234)
#' x = c(arima.sim(n = n, list(ar = ar), sd = sd) + mean)
#'
#' nse.cos(x = x, q = 12, lag.prewhite = 0)
#' nse.cos(x = x, q = 12, lag.prewhite = NULL)
#' }
nse.cos = function(x, q = 12, lag.prewhite = 0) {
  f.error.multivariate(x)
  x = as.numeric(x)
  tmp   = f.prewhite(x, ar.order = lag.prewhite)
  x     = tmp$ar.resid
  scale = tmp$scale
  N = length(x)
  X = cos.basis(q, N)
  coef = lm(x ~ 1 + X)$coef[2:(q + 1)]
  SSX = N * sum(coef * coef)
  out = (scale * SSX / q / N)
  out = as.numeric(out)
  out = sqrt(out)
  out = as.numeric(out)
  return(out)
}
