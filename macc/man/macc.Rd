\name{macc}
\alias{macc}
\title{ Mediation Analysis of Causality under Confounding
}
\description{ This function performs causal mediation analysis under confounding or correlated errors for single level, two-level, and three-level mediation models.
}
\usage{
macc(dat, model.type = c("single", "multilevel", "twolevel"), 
  method = c("HL", "TS", "HL-TS"), delta = NULL, interval = c(-0.9, 0.9), 
  tol = 0.001, max.itr = 500, conf.level = 0.95, 
  optimizer = c("optimx", "bobyqa", "Nelder_Mead"), mix.pkg = c("nlme", "lme4"), 
  random.indep = TRUE, random.var.equal = FALSE, u.int = FALSE, Sigma.update = TRUE, 
  var.constraint = TRUE, random.var.update = TRUE, logLik.type = c("logLik", "HL"), 
  error.indep = TRUE, error.var.equal = FALSE, 
  sens.plot = FALSE, sens.interval = seq(-1, 1, by = 0.01), legend.pos = "topright", 
  xlab = expression(delta), ylab = expression(hat(AB)), cex.lab = 1, cex.axis = 1, 
  lgd.cex = 1, lgd.pt.cex = 1, plot.delta0 = TRUE, ...)
}
\arguments{
  \item{dat}{ a data frame or a list of data. When it is a data frame, it contains \code{Z} as the treatment/exposure assignment, \code{M} as the mediator and \code{R} as the interested outcome and \code{model.type} should be "single". \code{Z}, \code{M} and \code{R} are all in one column. When it is a list, the list length is the number of subjects. For a two-level dataset, each list contains one data frame with \code{Z}, \code{M} and \code{R}, and \code{model.type} should be "twolevel"; for a three-level dataset, each subject list consists of \code{K} lists of data frame, and \code{model.type} should be "multilevel".
}
  \item{model.type}{a character of model type, "single" for single level model, "multilevel" for three-level model and "twolevel" for two-level model.
}
  \item{method}{ a character of method that is used for the two/three-level mediation model. When \code{delta} is given, the method can be either "HL" (hierarchical-likelihood) or "TS" (two-stage); when \code{delta} is not given, the method can be "HL", "TS" or "HL-TS". The "HL-TS" method estimates \code{delta} by the "HL" method first and uses the "TS" method to estimate the rest parameters. For three-level model, when \code{method = "HL"} and \code{u.int = TRUE}, the parameters are estimated through a marginal-likelihood method.
}
  \item{delta}{ a number gives the correlation between the model errors. Default value is \code{NULL}. When \code{model.type = "single"}, the default will be \code{0}. For two/three-level model, if \code{delta = NULL}, the value of \code{delta} will be estimated.
}
  \item{interval}{ a vector of length two indicates the searching interval when estimating \code{delta}. Default is (-0.9,0.9).
}
  \item{tol}{ a number indicates the tolerence of convergence for the "HL" method. Default is 0.001.
}
  \item{max.itr}{ an integer indicates the maximum number of interation for the "HL" method. Default is 500.
}
  \item{conf.level}{ a number indicates the significance level of the confidence intervals. Default is 0.95.
}
  \item{optimizer}{ a character of the name of optimizing function(s) in the mixed effects model. This is used only for three-level model. For details, see \code{\link[lme4]{lmerControl}}.
}
  \item{mix.pkg}{ a character of the package used for the mixed effects model in a three-level mediation model. 
}
  \item{random.indep}{ a logic value indicates if the random effects in the mixed effects model are independent. Default is \code{TRUE} assuming the random effects are independent. This is used for \code{model.type = "multilevel"} only.
}
  \item{random.var.equal}{ a logic value indicates if the variances of the random effects are identical. Default is \code{FALSE} assuming the variances are not identical. This is used for \code{model.type = "multilevel"} only.
}
  \item{u.int}{ a logic value. Default is \code{FALSE}. When \code{u.int = TRUE}, a marginal-likelihood method, which integrates out the random effects in the mixed effects model, is used to estimate the parameters. This is used when \code{model.type = "multilevel"} and \code{method = "HL"} or \code{method = "HL-TS"}.
}
  \item{Sigma.update}{ a logic value. Default is \code{TRUE}, and the estimated variances of the errors in the single level model will be updated in each iteration when running a two/three-level mediation model.
}
  \item{var.constraint}{ a logic value. Default is \code{TRUE}, and an interval constraint is added on the variance components in the two/three-level mediation model.
}
  \item{random.var.update}{ a logic value. Default is \code{TRUE}, and the estimates of the variance of the random effects in the mixed effects model are updated in each iteration. This is used when \code{model.type = "multilevel"} and \code{method = "HL"} or \code{method = "HL-TS"}.
}
  \item{logLik.type}{ a character value indicating the type of likelihood value returned. It is used for "TS" method. When \code{logLik.type = "logLik"}, the log-likelihood of the mixed effects model is maximized; when \code{logLik.type = "HL"}, the summation of log-likelihood of the single level model and the mixed effects model is maximized. This is used for \code{model.type = "multilevel"}. 
}
  \item{error.indep}{ a logic value. Default is \code{TRUE}. This is used for \code{model.type = "twolevel"}. When \code{error.indep = TRUE}, the error terms in the three linear models for \eqn{A}, \eqn{B} and \eqn{C} are independent.
}
  \item{error.var.equal}{ a logic value, Default is \code{FALSE}, This is used for \code{model.type = "twolevel"}. When \code{error.var.equal = TRUE}, the variances of the error terms in the three linear models for \eqn{A}, \eqn{B} and \eqn{C} are assumed to be identical.
}
  \item{sens.plot}{ a logic value. Default is \code{FALSE}. This is used only for single level model. When \code{sens.plot = TRUE}, the sensitivity analysis will be performed and plotted. 
}
  \item{sens.interval}{ a sequence of \code{delta} values under which the sensitivity analysis is performed. Default is a sequence from -1 to 1 with increment 0.01. The elements with absolute value 1 will be excluded from the analysis.
}
  \item{legend.pos}{ a character indicates the location of the legend when \code{sens.plot = TRUE}. This is used for single level model.
}
  \item{xlab}{ a title for \code{x} axis in the sensitivity plot. 
}
  \item{ylab}{ a title for \code{y} axis in the sensitivity plot.
}
  \item{cex.lab}{ the magnigication to be used for \code{x} and \code{y} labels relative to the current setting of \code{cex}.
}
  \item{cex.axis}{ the magnification to be used for axis annotation relative to the current setting of \code{cex}.
}
  \item{lgd.cex}{ the magnification to be used for legend relative to the current setting of \code{cex}.
}
  \item{lgd.pt.cex}{ the magnification to be used for the points in legend relative to the current setting of \code{cex}.
}
  \item{plot.delta0}{ a logic value. Default is \code{TRUE}. When \code{plot.delta0 = TRUE}, the estimates when \eqn{\delta=0} is plotted.
}
  \item{\dots}{ additional argument to be passed.
}
}
\details{ The single level mediation model is
  \deqn{M=ZA+E_1,}
  \deqn{R=ZC+MB+E_2.}
  A correlation between the model errors \eqn{E_1} and \eqn{E_2} is assumed to be \eqn{\delta}. The coefficients are estimated by maximizing the log-likelihood function. The confidence intervals of the coefficients are calculated based on the asymptotic joint distribution. The variance of \eqn{AB} estimator based on either the product method or the difference method is obtained from the Delta method. Under this single level model, \eqn{\delta} is not identifiable. Sensitivity analysis for the indirect effect (\eqn{AB}) can be used to assess the deviation of the findings, when assuming \eqn{\delta=0} violates the independence assumption. 
  
  The two/three-level mediation models are proposed to estimate \eqn{\delta} from data without sensitivity analysis. They address the within/between-subject variation issue for datasets with hierarchical structure. For simplicity, we refer to the three levels of data by trials, sessions and subjects, respectively. See reference for more details. Under the two-level mediation model, the data consists of \eqn{N} independent subjects and \eqn{n_i} trials for subject \eqn{i}; under the three-level mediation model, the data consists of \eqn{N} independent subjects, \eqn{K} sessions for each and \eqn{n_{ik}} trials. Under the two-level (three-level) models, the single level mediation model is first applied on the trials from (the same session of) a single subject. The coefficients then follow a linear (mixed effects) model. Here we enforce the assumption that \eqn{\delta} is a constant across (sessions and) subjects. The parameters are estimated through a hierarchical-likelihood (or marginal likelihood) or a two-stage method.
}
\value{ When \code{model.type = "single"},
  \item{Coefficients}{point estimate of the coefficients, as well as the corresponding standard error and confidence interval. The indirect effect is estimated by both the product (\code{ABp}) and the difference (\code{ABd}) methods.}
  \item{D}{point estimate of the regression coefficients in matrix form.}
  \item{Sigma}{estimated covariance matrix of the model errors.}
  \item{delta}{the \eqn{\delta} value used to estimate the rest parameters.}
  \item{time}{the CPU time used, see \code{\link[base]{system.time}}.}
  
  When \code{model.type = "multilevel"},
  \item{delta}{the specified or estimated value of correlation.}
  \item{Coefficients}{the estimated fixed effects in the mixed effects model for the coefficients, as well as the corresponding confidence intervals and standard errors. Here confidence intervals and standard errors are the estimates directly from the mixed effects model, the variation of estimating these parameters is not accounted for.}
  \item{Cor.comp}{estimated correlation matrix of the random effects in the mixed effects model.}
  \item{Var.comp}{estimated variance components in the mixed effects model.}
  \item{Var.C2}{estimated variance components for \eqn{C'}, the total effect, if a mixed effects model is considered.}
  \item{logLik}{the value of maximized log-likelihood of the mixed effects model.}
  \item{HL}{the value of hierarchical-likelihood.}
  \item{convergence}{the logic value indicating if the method converges.}
  \item{time}{the CPU time used, see \code{\link[base]{system.time}}}.
  
  When \code{model.type = "twolevel"}
  \item{delta}{the specified or estimated value of correlation.}
  \item{Coefficients}{the estimated population level effect in the regression models.}
  \item{Lambda}{the estimated covariance matrix of the model errors in the coefficient regression models.}
  \item{Sigma}{the estimated variances of \eqn{E_1} and \eqn{E_2} for each subject.}
  \item{HL}{the value of full-likelihood (hierarchical-likelihood).}
  \item{convergence}{the logic value indicating if the method converges.}
  \item{Var.constraint}{the interval constraints used for the variances in the coefficient regression models.}
  \item{time}{the CPU time used, see \code{\link[base]{system.time}}}.
}
\references{Zhao, Y., & Luo, X. (2014). \emph{Estimating Mediation Effects under Correlated Errors with an Application to fMRI.} arXiv preprint arXiv:1410.7217.
}
\author{ Yi Zhao, Brown University, \email{yi_zhao@brown.edu};
  Xi Luo, Brown University, \email{xi.rossi.luo@gmail.com}.
}
\examples{
# Examples with simulated data
  
  ##############################################################################
  # Single level mediation model
  # Data was generated with 500 independent trials. The correlation between model errors is 0.5.
  data(env.single)
  data.SL<-get("data1",env.single)
  
  ## Example 1: Given delta is 0.5.
  macc(data.SL,model.type="single",delta=0.5)
  # $Coefficients
  #       Estimate        SE          LB          UB
  # A    0.3572722 0.1483680  0.06647618  0.64806816
  # C    0.8261253 0.2799667  0.27740060  1.37485006
  # B   -0.9260217 0.1599753 -1.23956743 -0.61247594
  # C2   0.4952836 0.2441369  0.01678400  0.97378311
  # ABp -0.3308418 0.1488060 -0.62249617 -0.03918738
  # ABd -0.3308418 0.3714623 -1.05889442  0.39721087
  
  ## Example 2: Assume the errors are independent.
  macc(data.SL,model.type="single",delta=0)
  # $Coefficients
  #          Estimate         SE          LB        UB
  # A    0.3572721688 0.14836803  0.06647618 0.6480682
  # C    0.4961424716 0.24413664  0.01764345 0.9746415
  # B   -0.0024040905 0.15997526 -0.31594984 0.3111417
  # C2   0.4952835570 0.24413691  0.01678400 0.9737831
  # ABp -0.0008589146 0.05715582 -0.11288227 0.1111644
  # ABd -0.0008589146 0.34526154 -0.67755910 0.6758413
  
  ## Example 3: Sensitivity analysis (given delta is 0.5).
  macc(data.SL,model.type="single",delta=0.5,sens.plot=TRUE)
  ##############################################################################
  
  ##############################################################################
  # Three-level mediation model
  # Data was generated with 50 subjects and 4 sessions. 
  # The correlation between model errors in the single level is 0.5.
  # We comment out our examples due to the computation time.
  data(env.three)
  data.ML<-get("data3",env.three)
  
  ## Example 1: Correlation is unknown and to be estimated.
  # Assume random effects are independent
  # Add an interval constraint on the variance components.
  
  # "HL" method
  # macc(data.ML,model.type="multilevel",method="HL")
  # $delta
  # [1] 0.5224803

  # $Coefficients
  #            Estimate         LB         UB         SE
  # A        0.51759400  0.3692202  0.6659678 0.07570229
  # C        0.56882035  0.3806689  0.7569718 0.09599742
  # B       -1.13624114 -1.3688690 -0.9036133 0.11868988
  # C2      -0.06079748 -0.4163135  0.2947186 0.18138908
  # AB.prod -0.58811160 -0.7952826 -0.3809406 0.10570145
  # AB.diff -0.62961784 -1.0318524 -0.2273833 0.20522549
  # 
  # $time
  #    user  system elapsed 
  #   44.34    3.53   17.71 
  
  # "ML" method
  # macc(data.ML,model.type="multilevel",method="HL",u.int=TRUE)
  # $delta
  # [1] 0.5430744

  # $Coefficients
  #            Estimate         LB         UB         SE
  # A        0.51764821  0.3335094  0.7017871 0.09395011
  # C        0.59652821  0.3715001  0.8215563 0.11481236
  # B       -1.19426328 -1.4508665 -0.9376601 0.13092240
  # C2      -0.06079748 -0.4163135  0.2947186 0.18138908
  # AB.prod -0.61820825 -0.8751214 -0.3612951 0.13108056
  # AB.diff -0.65732570 -1.0780742 -0.2365772 0.21467155
  # 
  # $time
  #    user  system elapsed 
  #  125.49    9.52   39.10 
  
  # "TS" method
  # macc(data.ML,model.type="multilevel",method="TS")
  # $delta
  # [1] 0.5013719

  # $Coefficients
  #            Estimate         LB         UB         SE
  # A        0.51805823  0.3316603  0.7044561 0.09510271
  # C        0.53638546  0.3066109  0.7661601 0.11723409
  # B       -1.07930526 -1.3386926 -0.8199179 0.13234293
  # C2      -0.06079748 -0.4163135  0.2947186 0.18138908
  # AB.prod -0.55914297 -0.8010745 -0.3172114 0.12343672
  # AB.diff -0.59718295 -1.0204890 -0.1738769 0.21597645
  # 
  # $time
  #    user  system elapsed 
  #   19.53    0.00   19.54
  
  ## Example 2: Given the correlation is 0.5.
  # Assume random effects are independent.
  # Add an interval constraint on the variance components.
  
  # "HL" method
  macc(data.ML,model.type="multilevel",method="HL",delta=0.5)
  # $delta
  # [1] 0.5

  # $Coefficients
  #            Estimate         LB         UB         SE
  # A        0.51760568  0.3692319  0.6659794 0.07570229
  # C        0.53916412  0.3512951  0.7270331 0.09585330
  # B       -1.07675116 -1.3093989 -0.8441035 0.11869999
  # C2      -0.06079748 -0.4163135  0.2947186 0.18138908
  # AB.prod -0.55733252 -0.7573943 -0.3572708 0.10207419
  # AB.diff -0.59996161 -1.0020641 -0.1978591 0.20515811
  # 
  # $time
  #    user  system elapsed 
  #    2.44    0.22    1.03 
  ##############################################################################
  
  ##############################################################################
  # Two-level mediation model
  # Data was generated with 50 subjects. 
  # The correlation between model errors in the single level is 0.5.
  # We comment out our examples due to the computation time.
  data(env.two)
  data.TL<-get("data2",env.two)
  
  ## Example 1: Correlation is unknown and to be estimated.
  # Assume errors in the coefficients regression models are independent.
  # Add an interval constraint on the variance components. 
  
  # "HL" method
  # macc(data.TL,model.type="twolevel",method="HL")
  # $delta
  # [1] 0.5066551

  # $Coefficients
  #            Estimate
  # A        0.51714224
  # C        0.54392056
  # B       -1.05048406
  # C2      -0.02924135
  # AB.prod -0.54324968
  # AB.diff -0.57316190
  # 
  # $time
  #    user  system elapsed 
  #    3.07    0.00    3.07 
  
  # "TS" method
  # macc(data.TL,model.type="twolevel",method="TS")
  # $delta
  # [1] 0.4481611

  # $Coefficients
  #            Estimate
  # A        0.52013697
  # C        0.47945755
  # B       -0.90252718
  # C2      -0.02924135
  # AB.prod -0.46943775
  # AB.diff -0.50869890
  # 
  # $time
  #    user  system elapsed 
  #    1.60    0.00    1.59 
  
  ## Example 2: Given the correlation is 0.5.
  # Assume random effects are independent.
  # Add an interval constraint on the variance components.
  
  # "HL" method
  macc(data.TL,model.type="twolevel",method="HL",delta=0.5)
  # $delta
  # [1] 0.5

  # $Coefficients
  #            Estimate
  # A        0.51718063
  # C        0.53543300
  # B       -1.03336668
  # C2      -0.02924135
  # AB.prod -0.53443723
  # AB.diff -0.56467434
  # 
  # $time
  #    user  system elapsed 
  #    0.21    0.00    0.20 
  ##############################################################################
}
\keyword{ models }
