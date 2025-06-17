\name{gma}
\alias{gma}
\title{ Granger Mediation Analysis of Time Series
}
\description{ This function performs Granger Mediation Analysis (GMA) for time series data.
}
\usage{
gma(dat, model.type = c("single", "twolevel"), method = c("HL", "TS", "HL-TS"), 
  delta = NULL, p = 1, single.var.asmp = TRUE, sens.plot = FALSE, 
  sens.delta = seq(-1, 1, by = 0.01), legend.pos = "topright", 
  xlab = expression(delta), ylab = expression(hat(AB)), cex.lab = 1, 
  cex.axis = 1, lgd.cex = 1, lgd.pt.cex = 1, plot.delta0 = TRUE, 
  interval = c(-0.9, 0.9), tol = 1e-04, max.itr = 500, conf.level = 0.95, 
  error.indep = TRUE, error.var.equal = FALSE, 
  Sigma.update = TRUE, var.constraint = TRUE, ...)
}
\arguments{
  \item{dat}{ a data frame or a list of data. When it is a data frame, it contains \code{Z} as the treatment/exposure assignment, \code{M} as the mediator and \code{R} as the interested outcome and \code{model.type} should be "single". \code{Z}, \code{M} and \code{R} are all in one column. When it is a list, the list length is the number of subjects. For a two-level dataset, each list contains one data frame with \code{Z}, \code{M} and \code{R}, and \code{model.type} should be "twolevel".
}
  \item{model.type}{ a character of model type, "single" for single level model, "twolevel" for two-level model.
}
  \item{method}{ a character of method that is used for the two-level GMA model. When \code{delta} is given, the method can be either "HL" (full likelihood) or "TS" (two-stage); when \code{delta} is not given, the method can be "HL", "TS" or "HL-TS". The "HL-TS" method estimates \code{delta} by the "HL" method first and uses the "TS" method to estimate the rest parameters.
}
  \item{delta}{ a number gives the correlation between the Gaussian white noise processes. Default value is \code{NULL}. When \code{model.type = "single"}, the default will be \code{0}. For two-level model, if \code{delta = NULL}, the value of \code{delta} will be estimated.
}
  \item{p}{ a number gives the order of the vector autoregressive model. Default value is \code{1}. 
}
  \item{single.var.asmp}{ a logic value indicates if in the single level model, the asymptotic variance will be used. Default value is \code{TRUE}.
}
  \item{sens.plot}{ a logic value. Default is \code{FALSE}. This is used only for single level model. When \code{sens.plot = TRUE}, the sensitivity analysis will be performed and plotted.
}
  \item{sens.delta}{ a sequence of \code{delta} values under which the sensitivity analysis is performed. Default is a sequence from -1 to 1 with increment 0.01. The elements with absolute value 1 will be excluded from the analysis.
}
  \item{legend.pos}{ a character indicates the location of the legend when \code{sens.plot = TRUE}. This is used for single level model.
}
  \item{xlab}{ a title for \code{x} axis in the sensitivity plot.
}
  \item{ylab}{ a title for \code{y} axis in the sensitivity plot.
}
  \item{cex.lab}{ the magnification to be used for \code{x} and \code{y} labels relative to the current setting of \code{cex}.
}
  \item{cex.axis}{ the magnification to be used for axis annotation relative to the current setting of \code{cex}.
}
  \item{lgd.cex}{ the magnification to be used for legend relative to the current setting of \code{cex}.
}
  \item{lgd.pt.cex}{ the magnification to be used for the points in legend relative to the current setting of \code{cex}.
}
  \item{plot.delta0}{ a logic value. Default is \code{TRUE}. When \code{plot.delta0 = TRUE}, the estimates when \eqn{\delta = 0} is plotted.
}
  \item{interval}{ a vector of length two indicates the searching interval when estimating \code{delta}. Default is (-0.9,0.9).
}
  \item{tol}{ a number indicates the tolerance of convergence for the "HL" method. Default is \code{1e-4}.
}
  \item{max.itr}{ an integer indicates the maximum number of iteration for the "HL" method. Default is 500.
}
  \item{conf.level}{ a number indicates the significance level of the confidence intervals. Default is 0.95.
}
  \item{error.indep}{ a logic value. Default is \code{TRUE}. This is used for \code{model.type = "twolevel"}. When \code{error.indep = TRUE}, the error terms in the linear models for \eqn{A}, \eqn{B} and \eqn{C} are independent.
}
  \item{error.var.equal}{ a logic value. Default is \code{FALSE}. This is used for \code{model.type = "twolevel"}. When \code{error.var.equal = TRUE}, the variances of the error terms in the linear models for \eqn{A}, \eqn{B} and \eqn{C} are assumed to be identical.
}
  \item{Sigma.update}{ a logic value. Default is \code{TRUE}, and the estimated variances of the Gaussian white noise processes in the single level model will be updated in each iteration when running a two-level GMA model.
}
  \item{var.constraint}{ a logic value. Default is \code{TRUE}, and an interval constraint is added on the variance components in the higher level regression model of the two-level GMA model.
}
  \item{\dots}{ additional arguments to be passed.
}
}
\details{ The single level GMA model is
  \deqn{M_{t}=Z_{t}A+E_{1t},}
  \deqn{R_{t}=Z_{t}C+M_{t}B+E_{2t},}
  and for stochastic processes \eqn{(E_{1t},E_{2t})},
  \deqn{E_{1t}=\sum_{j=1}^{p}\omega_{11_{j}}E_{1,t-j}+\sum_{j=1}^{p}\omega_{21_{j}}E_{2,t-j}+\epsilon_{1t},}
  \deqn{E_{2t}=\sum_{j=1}^{p}\omega_{12_{j}}E_{1,t-j}+\sum_{j=1}^{p}\omega_{22_{j}}E_{2,t-j}+\epsilon_{2t}.}
  A correlation between the Gaussian white noise \eqn{(\epsilon_{1t},\epsilon_{2t})} is assumed to be \eqn{\delta}. The coefficients, as well as the transition matrix, are estimated by maximizing the conditional log-likelihood function. The confidence intervals of the coefficients are calculated based on the asymptotic joint distribution. The variance of \eqn{AB} estimator based on either the product method or the difference method is obtained from the Delta method. Under this single level model, \eqn{\delta} is not identifiable. Sensitivity analysis for the indirect effect (\eqn{AB}) can be used to assess the deviation of the findings from assuming \eqn{\delta = 0}, when the independence assumption is violated.

  The two-level GMA model is introduced to estimate \eqn{\delta} from data without sensitivity analysis. It addresses the individual variation issue for datasets with hierarchically nested structure. For simplicity, we refer to the two levels of data by time series and subjects. Under the two-level GMA model, the data consists of \eqn{N} independent subjects and a time series of length \eqn{n_{i}}. The single level GMA model is first applied on the time series from a single subject. The coefficients then follow a linear model. Here we enforce the assumption that \eqn{\delta} is a constant across subjects. The parameters are estimated through a full likelihood or a two-stage method.
}
\value{ When \code{model.type = "single"},
  \item{Coefficients}{point estimate of the coefficients, as well as the corresponding standard error and confidence interval. The indirect effect is estimated by both the produce (\code{ABp}) and the difference (\code{ABd}) methods.}
  \item{D}{point estimate of the causal coefficients in matrix form.}
  \item{Sigma}{estimate covariance matrix of the Gaussian white noise.}
  \item{delta}{the \eqn{\delta} value used to estimate the rest parameters.}
  \item{W}{estimate of the transition matrix in the VAR(p) model.}
  \item{LL}{the conditional log-likelihood value.}
  \item{time}{the CPU time used, see \code{\link[base]{system.time}}.}

  When \code{model.type = "twolevel"}
  \item{delta}{the specified or estimated value of correlation parameter \eqn{\delta}.}
  \item{Coefficients}{the estimated population level effect in the regression models.}
  \item{Lambda}{the estimated covariance matrix of the model errors in the higher level coefficient regression models.}
  \item{Sigma}{the estimated variances of \eqn{\epsilon_{1t}} and \eqn{\epsilon_{2t}} for each subject.}
  \item{W}{the estimated population level transition matrix.}
  \item{HL}{the value of full likelihood.}
  \item{convergence}{the logic value indicating if the method converges.}
  \item{var.constraint}{the interval constraints used for the variances in the higher level coefficient regression models.}
  \item{time}{the CPU time used, see \code{\link[base]{system.time}}.}
}
\references{Zhao, Y., & Luo, X. (2017). \emph{Granger Mediation Analysis of Multiple Time Series with an Application to fMRI.} arXiv preprint arXiv:1709.05328.
}
\author{ Yi Zhao, Brown University, \email{zhaoyi1026@gmail.com};
  Xi Luo, Brown University, \email{xi.rossi.luo@gmail.com}.
}
\examples{
# Example with simulated data

##############################################################################
# Single level GMA model
# Data was generated with 500 time points.
# The correlation between Gaussian white noise is 0.5.
data(env.single)
data.SL<-get("data1",env.single)

## Example 1: Given delta is 0.5.
gma(data.SL,model.type="single",delta=0.5)
# $Coefficients
#          Estimate         SE         LB         UB
# A     0.519090451 0.06048910  0.4005340  0.6376469
# C     0.487396067 0.12650909  0.2394428  0.7353493
# B    -0.951262962 0.07693595 -1.1020547 -0.8004713
# C2   -0.006395453 0.12125003 -0.2440411  0.2312502
# AB.p -0.493791520 0.07004222 -0.6310717 -0.3565113
# AB.d -0.493791520 0.17523161 -0.8372392 -0.1503439

## Example 2: Assume the white noise are independent.
gma(data.SL,model.type="single",delta=0)
# $Coefficients
#          Estimate         SE          LB         UB
# A     0.519090451 0.06048910  0.40053400 0.63764690
# C    -0.027668910 0.11136493 -0.24594015 0.19060234
# B     0.040982178 0.07693595 -0.10980952 0.19177387
# C2   -0.006395453 0.12125003 -0.24404115 0.23125024
# AB.p  0.021273457 0.04001358 -0.05715172 0.09969864
# AB.d  0.021273457 0.16463207 -0.30139946 0.34394638

## Example 3: Sensitivity analysis (given delta is 0.5)
# We comment out the example due to the computation time.
# gma(data.SL,model.type="single",delta=0.5,sens.plot=TRUE)
##############################################################################

##############################################################################
# Two-level GMA model
# Data was generated with 50 subjects.
# The correlation between white noise in the single level model is 0.5.
# The time series is generate from a VAR(1) model.
# We comment out our examples due to the computation time.
data(env.two)
data.TL<-get("data2",env.two)

## Example 1: Correlation is unknown and to be estimated.
# Assume errors in the coefficients model are independent.
# Add an interval constraint on the variance components.

# "HL" method
# gma(data.TL,model.type="twolevel",method="HL",p=1)
# $delta
# [1] 0.5176206
# 
# $Coefficients
#           Estimate
# A        0.5587349
# C        0.7129338
# B       -1.0453097
# C2       0.1213349
# AB.prod -0.5840510
# AB.diff -0.5915989
# 
# $time
#   user  system elapsed 
# 12.285   0.381  12.684 

# "TS" method
# gma(data.TL,model.type="twolevel",method="TS",p=1)
# $delta
# [1] 0.4993492
# 
# $Coefficients
#           Estimate
# A        0.5569101
# C        0.6799228
# B       -0.9940383
# C2       0.1213349
# AB.prod -0.5535900
# AB.diff -0.5585879
# 
# $time
#  user  system elapsed 
# 7.745   0.175   7.934 

## Example 2: Given the correlation is 0.5.
# Assume errors in the coefficients model are independent.
# Add an interval constraint on the variance components.

# "HL" method
# gma(data.TL,model.type="twolevel",method="HL",delta=0.5,p=1)
# $delta
# [1] 0.5
# 
# $Coefficients
#           Estimate
# A        0.5586761
# C        0.6881703
# B       -0.9997898
# C2       0.1213349
# AB.prod -0.5585587
# AB.diff -0.5668355
# 
# $time
#  user  system elapsed 
# 0.889   0.023   0.913
##############################################################################
}
\keyword{ models }
