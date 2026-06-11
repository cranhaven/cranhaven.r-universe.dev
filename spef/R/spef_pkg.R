#' spef: Semiparametric Estimating Functions
#'
#' \code{spef} is an R package that consists of a collection of functions for fitting
#' semiparametric regression models for panel count survival data.
#' Estimating procedures include: Wang-Yan’s augmented estimating equations (AEE, AEEX),
#' Huang-Wang-Zhang’s method (HWZ), Zhang’s maximum pseudolikelihood (MPL),
#' Maximum pseudolikelihood with I-Splines (MPLs), Maximum likelihood with I-Splines (MLs),
#' Sun-Wei’s method (`EE.SWa`, `EE.SWb`, `EE.SWc`), Hu-Sun-Wei's method (`EE.HSWc`, `EE.HSWm`),
#' and accelerated mean model (`AMM`).
#'
#' @aliases spef-packages
#' @references Chiou, S., Xu, G., Yan, J., and Huang, C.-Y. (2017).
#' Semiparametric estimation of the accelerated mean model with panel count data under
#' informative examination times. \emph{Biometrics}, to appear.
#' <doi: 10.1111/biom.12840>.
#' @references Huang, C.-Y., Wang, M., and Zhang, Y. (2006).
#' Analysing panel count data with informative observation times.
#' \emph{Biometrika}, \bold{93}(4), 763--776.
#' @references Hu, X. J., Sun, J. and Wei, L. J. (2003).
#' Regression parameter estimation from panel counts.
#' \emph{Scandinavian Journal of Statistics}, \bold{30}, 25--43.
#' @references Lu, M., Zhang, Y., and Huang, J. (2007).
#' Estimation of the mean function with panel count data using monotone polynomial splines.
#' \emph{Biometrika}, \bold{94}(3), 705--718.
#' @references Sun, J. and Wei, L. J. (2000). Regression analysis of panel count
#' data with covariates-dependent observation and censoring times.
#' \emph{Journal of the Royal Statistical Society, Series B: Statistical Methodology},
#' \bold{62}(2), 293--302.
#' @references Wang, X. and Yan, J. (2011). Fitting semiparametric regressions for panel
#' count survival data with an R package spef.
#' \emph{Computer methods and programs in biomedicine} \bold{104}(2), 278--285.
#' @references Wang, X. and Yan, J. (2013). Augmented estimating equations for
#' semiparametric panel count regression with informative observation
#' times and censoring time. \emph{Statistica Sinica}, \bold{23}(1), 359--381.
#' @references Zhang, Y. (2002). A Semiparametric pseudolikelihood estimation method
#' for panel count data. \emph{Biometrika}, \bold{89}(1), 39--48.
#'
#' @importFrom graphics abline plot points title boxplot
#' @importFrom methods getClass new
#' @importFrom stats as.formula model.matrix optim pchisq printCoefmat quantile rbinom approx
#' @importFrom stats rexp rgamma rmultinom rnorm rpois runif sd stepfun time uniroot var aggregate
#' @importFrom utils head tail
#' @importFrom survival coxph survfit Surv cluster
#' @importFrom nleqslv nleqslv
#' @importFrom plyr ddply count
#' @importFrom ggplot2 ggplot geom_tile aes aes_string theme_bw scale_fill_gradient
#' @importFrom splines bs
#' @importFrom BB dfsane BBoptim
#' @importFrom SQUAREM squarem
#' @importFrom sm sm.regression
#'
#' @useDynLib spef, .registration = TRUE
#' @docType package
"_PACKAGE"
NULL

#' Semiparametric Panel Count Regression Object
#'
#' This S3 class of objects is returned by the \code{panelReg} class of
#'   functions to represent a fitted semiparametric panel count regression
#'   model. Objects of this class have methods for the functions
#'   \code{print} and \code{plot}.
#'
#' @name panelReg.object
#' @return
#' \describe{
#' \item{beta}{a vector, estimated coefficients of the linear predictor.}
#'   \item{baseline}{a step function or spline function, estimated
#'     cumulative baseline mean.}
#'   \item{timeGrid}{a vector, ordered unique set of observation times.}
#'   \item{lambda}{a vector, estimated baseline mean for each interval.}
#'   \item{convergence}{an integer code, \code{0} indicates successful
#'     convergence, error code \code{1} indicates that the iteration limit
#'     \code{maxIter} had been reached.}
#'   \item{iter}{an integer value, number of interactions when stopped.}
#'   \item{betaSE}{a vector, estimated standard errors of beta.}
#'   \item{betaVar}{a matrix, estimated covariance matrix of beta.}
#'   \item{baselineSE}{a vector, estimated standard error of
#'     cumulative baseline mean.}
#' }
#' @seealso \code{\link{panelReg}}
#' @export panelReg
NULL
