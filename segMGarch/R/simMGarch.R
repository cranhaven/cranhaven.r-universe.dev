#' An S4 class for a nonstationary CCC model.
#' @name simMGarch-class
#' @rdname simMGarch-class
#' @description A specification class to create an object of a simulated piecewise constant conditional correlation (CCC) model 
#' denoted by \eqn{r_t = (r_{1, t}, \ldots, r_{n, t})^T}, \eqn{t=1, \ldots, n} with 
#' \eqn{r_{i, t}= \sqrt{h_{i, t}}\epsilon_{i, t}} where \eqn{h_{i, t}= \omega_i(t) + \sum_{j=1}^p \alpha_{i, j}(t)r_{i, t-j}^2 + \sum_{k=1}^q \beta_{i, k}(t)h_{i, t-k}}.
#' In this package, we assume a piecewise constant CCC with \eqn{p=q=1}.
#' @slot y The \eqn{n \times d} time series.
#' @slot cor_errors The \eqn{n \times d} matrix of the errors.
#' @slot h The \eqn{n \times d} matrix of the time-varying variances.
#' @slot n Size of the time series.
#' @slot d The number of variables (assets).
#' @slot r A sparsity parameter to conrol the impact of changepoint across the series.
#' @slot multp A parameter to control the covariance of errors.
#' @slot changepoints The vector with the location of the changepoints.
#' @slot pw A logical parameter to allow for changepoints in the error covariance matrix.
#' @slot a0 The vector of the parameters a0 in the individual GARCH processes denoted by \eqn{\omega_i(t)} in the above formula.
#' @slot a1 The vector of the parameters a1 in the individual GARCH processes denoted by \eqn{\alpha_i(t)} in the above formula.
#' @slot b1 The vector of the parameters b1 in the individual GARCH processes denoted by \eqn{\beta_i(t)} in the above formula.
#' @slot BurnIn The size of the burn-in sample. Note that this only applies at the first simulated segment. Default is 50.
#' @references
#' Cho, Haeran, and Karolos Korkas. "High-dimensional GARCH process segmentation with an application to Value-at-Risk." arXiv preprint arXiv:1706.01155 (2017).
#' @examples
#' pw.CCC.obj <- new("simMGarch")
#' pw.CCC.obj <- pc_cccsim(pw.CCC.obj)
#' par(mfrow=c(2,2))
#' ts.plot(pw.CCC.obj@y[1,]);ts.plot(pw.CCC.obj@y[2,])
#' ts.plot(pw.CCC.obj@h[1,]);ts.plot(pw.CCC.obj@h[1,])
#' @import Rcpp foreach doParallel parallel iterators methods
#' @importFrom  corpcor cor.shrink
#' @useDynLib segMGarch, .registration = TRUE
#' @export
setClass("simMGarch", 
         slots = c(
           y = "matrix",
           cor_errors = "matrix",
           h = "matrix",
           n = "numeric", 
           d = "numeric", 
           r = "numeric", 
           multp = "numeric", 
           changepoints = "numeric", 
           pw = "logical",
           a0 = "numeric",
           a1 = "numeric",
           b1 = "numeric",
           BurnIn = "numeric"
         ), 
         prototype = list(
           n=2000,
           d=50,
           r=0.5,
           multp=1,
           changepoints=c(341,614,838),
           pw=TRUE,
           a0=c(0.4,0.8),
           a1 = c(.1,.2),
           b1 = c(.1,.3,.6),
           BurnIn=50
         )
)