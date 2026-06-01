\name{jointfit}
\alias{jointfit}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Fitting Multivariate Double Hierarchical Generalized Linear Models using
h-likelihood Approach
}

\description{The jointfit is used to fit a multivariate double hierarchical generalized linear models (MDHGLMs) allowing
different models for multivariate response variables where each response follow DHGLM. A variety of distributions and link
functions for both response and the random effects are allowed. Fixed and random
effects can also be fitted in both the mean and the dispersion components. To call the fitting function
jointfit, models for the mean and dispersion must be specified by DHGLMMODELING object
preferably created by calling the DHGLMMODELING function.}

\usage{
jointfit(RespDist="gaussian",BinomialDen=NULL, DataMain, MeanModel,DispersionModel=NULL,
PhiFix=NULL,LamFix=NULL,structure="correlated",mord=0,dord=1,convergence=1e-05,
Init_Corr=NULL, EstimateCorrelations=TRUE, ZZCorr=NULL, factor=NULL, REML=TRUE,order=1)
}
\arguments{
  \item{RespDist}{The distribution of the response is set by the option \code{RespDist}. The user can set
it to: "gaussian" (default), "binomial", "poisson", or "gamma".}
  \item{BinomialDen}{When \code{RespDist}="binomial", one should use the option \code{BinomialDen} to specify
the denominator for the binomial distribution. This should be "NULL" (default)
or a numeric vector of length equal to the length of \code{DataMain}. When specified
as \code{BinomialDen}=NULL and \code{RespDist}="binomial", the denominator is 1.
}
  \item{DataMain}{The option \code{DataMain} determines the data frame to be used (non-optional).}
  \item{MeanModel}{For the mean model, this option requires \code{DGHLMMODELING} object which
should specified by the option Model="mean".}
  \item{DispersionModel}{For the overdispersion model, this option requires \code{DGHLMMODELING} object
which should be specified by the option Model="dispersion".}
  \item{PhiFix}{The option for overdispersion parameters (phi) to be estimated or maintaned constant.
Specifying defaults such as \code{PhiFix} =NULL implies that phi is to be estimated. If not, phi is fixed at a value
specified by \code{PhiFix}.}
  \item{LamFix}{The option for random-effect variance (lambda) to be estimated or maintaned constant.
Specifying defaults such as \code{LamFix} =NULL implies that lambda is to be estimated. If not, lambda is fixed at a value
specified by \code{LamFix}.
}
  \item{structure}{The option \code{structure} determines structure of random effects. When \code{structure}="correlated" (or "shared"),
  correlated (or "shared") random-effects model is specified. Factor analysis can be fitted by specifying \code{structure}="factor".
  Furthremore, selection model for missing data can be also fitted by specifying \code{structure}="selection".}
  \item{mord}{The option \code{mord} specifies the order of Laplace approximation to the marginal
likelihood for fitting the mean parameters. The choice is either 0 or 1 (default).
}
  \item{dord}{The option \code{dord} specifies the order of adjusted
profile likelihood for fitting the dispersion parameters. The choice is either 1
(default) or 2.
}
  \item{convergence}{Setting this option determines the criterion for convergence, which is computed
as the absolute difference between the values of all the estimated parameters in
the previous and current iterations. The default criterion is 1e-06.
}
  \item{Init_Corr}{Setting initial values of correlation (or shared parameters) between random effects}
  \item{EstimateCorrelations}{Correlation are estimated or fixed when \code{EstimateCorrelations}=TRUE (default) or \code{EstimateCorrelations}=FALSE}
  \item{ZZCorr}{List of model matrices for random effects}
  \item{factor}{factor structure when \code{structure}="factor"}
  \item{REML}{Giving REML estimates when \code{REML}=TRUE (default) or ML estimates when \code{REML}=FALSE}
  \item{order}{first order approximation when \code{order}=1 (default) or second order approximation when \code{order}=2 for factor analysis}  
}
\examples{
data(eg)
eg1<-eg[1:100,] ## using sampled data to have faster results 

jm1<-DHGLMMODELING(Link="identity", LinPred=y1~dose+dose2+(1|litter),RandDist="gaussian")
jm2<-DHGLMMODELING(Link="logit", LinPred=y2~dose+dose2+(1|litter),RandDist="gaussian")

Init_Corr=list(c(0))
SSC=list(as.factor(c(eg1$litter,eg1$litter)),as.factor(c(eg1$litter,eg1$litter)))
EstimateOverDisp=c(TRUE,FALSE)
LaplaceFixed=c(TRUE,TRUE)
ZZ1<-model.matrix(~as.factor(eg1$litter)-1)
ZZCorr=list(ZZ1,ZZ1)

#### independent random-effects model ####
res_ind<-jointfit(RespDist=c("gaussian","binomial"),DataMain=list(eg1,eg1),
    MeanModel=list(jm1,jm2),structure="correlated",
    Init_Corr=Init_Corr,EstimateCorrelations=FALSE,convergence=1,ZZCorr=ZZCorr)

#### correlated random-effects model ####
res_corr<-jointfit(RespDist=c("gaussian","binomial"),DataMain=list(eg1,eg1),
    MeanModel=list(jm1,jm2),structure="correlated",
    Init_Corr=Init_Corr,convergence=1,ZZCorr=ZZCorr)

#### shared random-effects model ####
Init_Corr=c(1,-10)
res_saturated<-jointfit(RespDist=c("gaussian","binomial"),DataMain=list(eg1,eg1),
    MeanModel=list(jm1,jm2),structure="shared",
    Init_Corr=Init_Corr,convergence=1)
}

