\name{oglmx}
\alias{oglmx}
\alias{nobs.oglmx}
\alias{coef.oglmx}
\alias{coef.summary.oglmx}
\alias{oglmx.fit}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Fit Ordered Generalized Linear Model.
}
\description{
\code{oglmx} is used to estimate models for which the outcome variable is discrete and the mean and/or variance of the underlying latent variable can be modelled as a linear combination of explanatory variables. Standard models such as probit, logit, ordered probit and ordered logit are included in the diverse set of models estimated by the function.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
oglmx(formulaMEAN, formulaSD=NULL, data, start=NULL, weights=NULL,
      link="probit", constantMEAN=TRUE, constantSD=TRUE, beta=NULL,
      delta=NULL, threshparam=NULL, analhessian=TRUE, 
      sdmodel=expression(exp(z)), SameModelMEANSD=FALSE, na.action,
      savemodelframe=TRUE, Force=FALSE, robust=FALSE)
      
oglmx.fit(outcomeMatrix, X, Z, w, beta, delta, threshparam, link, start,
          sdmodel, optmeth="maxLik", analhessian, robust)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formulaMEAN}{
  an object of class \code{\link{formula}}: a symbolic description of the model used to explain the mean of the latent variable. The response variable should be a numeric vector or factor variable such that the numerical assignments for the levels of the factor have ordinal meaning.
%%     ~~Describe \code{formulaMEAN} here~~
}
  \item{formulaSD}{
  either \code{NULL} or an object of class \code{\link{formula}}: a symbolic description of the model used to explain the variance of the latent variable.
%%     ~~Describe \code{formulaSD} here~~
}
  \item{data}{
  a data frame containing the variables in the model.
%%     ~~Describe \code{data} here~~
}
  \item{start}{
  either \code{NULL} or a numeric vector specifying start values for each of the estimated parameters, passed to the maximisation routine.
%%     ~~Describe \code{start} here~~
}
  \item{weights}{
  either \code{NULL} or a numeric vector of length equal to the number of rows in the data frame. Used to apply weighted maximum likelihood estimation.
}
  \item{link}{
    specifies a link function for the model to be estimated, accepted values are "\code{probit}", "\code{logit}", "\code{cauchit}", "\code{loglog}" and "\code{cloglog}"
%%     ~~Describe \code{link} here~~
}
  \item{constantMEAN}{
  logical. Should an intercept be included in the model of the mean of the latent variable? Can be overwritten and set to \code{FALSE} using the formulaMEAN argument by writing \code{0 + } as the first element of the equation.
%%     ~~Describe \code{constantMEAN} here~~
}
  \item{constantSD}{
  logical. Should an intercept be included in the model of the variance of the latent variable? Can be overwritten and set to \code{FALSE} using the formulaSD argument by writing \code{0 + } as the first element of the equation.
%%     ~~Describe \code{constantSD} here~~
}
  \item{beta}{
  \code{NULL} or numeric vector. Used to prespecify elements of the parameter vector for the equation of the mean of the latent variable. Vector should be of length one or of length equal to the number of explanatory variables in the mean equation. If of length one the value is presumed to correspond to the constant if a constant is included or the first element of the parameter vector. If of length greater than one then \code{NA} should be entered for elements of the vector to be estimated. 
%%     ~~Describe \code{beta} here~~
}
  \item{delta}{
  \code{NULL} or numeric vector. Used to prespecify elements of the parameter vector for the equation of the variance of the latent variable. Vector should be of length one or of length equal to the number of explanatory variables in the variance equation. If of length one the value is presumed to correspond to the constant if a constant is included or the first element of the parameter vector. If of length greater than one then \code{NA} should be entered for elements of the vector to be estimated.
%%     ~~Describe \code{delta} here~~
}
  \item{threshparam}{
  \code{NULL} or numeric vector. Used to prespecify the threshold parameters of the model. Vector should be of length equal to the number of outcomes minus one. \code{NA} should be entered for threshold parameters to be estimated by the model.
%%     ~~Describe \code{threshparam} here~~
}
  \item{analhessian}{
  logical. Indicates whether the analytic Hessian should be calculated and used, default is TRUE, if set to FALSE a finite-difference approximation of the Hessian is used.
%%     ~~Describe \code{analhessian} here~~
}
  \item{sdmodel}{
  object of mode \dQuote{\code{expression}}. The expression defines function that transforms the linear model for the standard deviation into the standard deviation. The expression should be written as a function of variable \code{z}. The default value is \code{expression(exp(z))}.  
%%     ~~Describe \code{sdmodel} here~~
}
  \item{SameModelMEANSD}{
  logical. Indicates whether the matrix used to model the mean of the latent variable is identical to that used to model the variance. If \code{formulaSD=NULL} and \code{SameModelMEANSD=TRUE} a model with heteroskedasticity is estimated. If \code{SameModelMEANSD=FALSE} and \code{formulaSD==formulaMEAN} value is overridden. Used to reduce memory requirements when models are identical. 
%%     ~~Describe \code{SameModelMEANSD} here~~
}
 \item{na.action}{
  a function which indicates what should happen when the data contain NAs. The default is set by the \code{na.action} setting of \code{\link{options}}, and is \code{\link{na.fail}} if that is unset. The factory-fresh default is \code{\link{na.omit}}. Another possible value is \code{NULL}, no action. Value \code{\link{na.exclude}} can be useful.
%%     ~~Describe \code{na.action} here~~
}
  \item{savemodelframe}{
  logical. Indicates whether the model frame(s) should be saved for future use. Default is \code{FALSE}. Should be set to \code{TRUE} if intending to estimate Average Marginal Effects.
%%     ~~Describe \code{savemodelframe} here~~
}
  \item{Force}{
  logical. If set to \code{FALSE} (the default) the function stops if the response variable has more than twenty categories. Should be changed to \code{TRUE} if a model with more than twenty categories is desired.
%%     ~~Describe \code{Force} here~~
}
  \item{robust}{
  logical. If set to \code{TRUE} the outer product or BHHH estimate of the meat in the sandwich of the variance-covariance matrix is calculated. If calculated standard errors will be calculated using the sandwich estimator by default when calling \code{summary}.
  }
  \item{outcomeMatrix, X, Z}{
  \code{X} is a data matrix for the right hand side of the mean equation, \code{outcomeMatrix} is a matrix that indicates the outcome variable and \code{Z} is a data matrix for the variance equation.
  }
  \item{w}{
  \code{w} specifies a vector of weights for the \code{oglmx.fit} function.  
  }
  \item{optmeth}{
  \code{optmeth} specifies a method for the maximisation of the likelihood, currently "maxLik" is the only available option.
  }
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
An object of class "\code{oglmx}" with the following components:
\item{link}{link function used in the estimated model.}
\item{sdmodel}{Expression for the model for the standard deviation, default is exp(z).}
\item{call}{the call used to generate the results.}
\item{factorvars}{vector listing factor variables included in the model}
\item{Outcomes}{numeric vector listing the values of the different outcomes.}
\item{NoVarModData}{dataframe. Contains data required to estimate the no information model used in calculation of McFadden's R-squared measure.}
\item{NOutcomes}{the number of distinct outcomes in the response variable.}
\item{Hetero}{logical. If \code{TRUE} indicates that the estimated model includes a model for the variance of the error term, i.e. heteroskedasticity.}
\item{formula}{two element list. Each element is an object of type \code{formula} related to the mean and standard deviation equation respectively.}
\item{modelframes}{If \code{savemodelframe} set to \code{FALSE} then returns \code{NULL}, otherwise returns a list with two elements, the model frames for the mean and variance equations.}
\item{BothEq}{Omitted in the case of a homoskedastic model. Dataframe listing variables that are contained in both the mean and variance equations.}
\item{varMeans}{a list containing two numeric vectors. The vectors list the mean values of the variables in the mean and variance equation respectively. Stored for use in a call of \code{margins.oglmx} to obtain marginal effects at means.}
\item{varBinary}{a list containing two numeric vectors. The vectors indicate whether the variables in the mean and variance equations are binary indicators. Stored for use in a call of \code{margins.oglmx} to obtain marginal effects at means.}
\item{loglikelihood }{log-likelihood for the estimated model. Includes as attributes the log-likelihood for the constant only model and the number of observations.}
\item{coefficients}{vector of estimated parameters.}
\item{gradient}{numeric vector, the value of the gradient of the log-likelihood function at the obtained parameter vector. Should be approximately equal to zero.}
\item{no.iterations}{number of iterations of maximisation algorithm.}
\item{returnCode}{code returned by the \code{maxLik} optimisation routine. For details of meaning see \code{\link{maxNR}}.}
\item{hessian}{hessian matrix of the log-likelihood function evaluated at the obtained parameter vector.}
\item{allparams}{a list containing three numeric vectors, the vectors contain the parameters from the mean equation, the variance equation and the threshold parameters respectively. Includes the prespecified and estimated parameters together.}
\item{Est.Parameters}{list containing three logical vectors. Indicates which parameters in the parameter vectors were estimated.}
\item{BHHHhessian}{Omitted if \code{robust = FALSE} and weights were not included. The BHHH variance-covariance estimate.}

%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
  Cameron, A. C. & Trivedi, P. K. (2005) \emph{Microeconometrics : methods and applications } Cambridge University Press
  
  Wooldridge, J. M. (2002) \emph{Econometric analysis of cross section and panel data } The MIT Press
}
  
\author{
Nathan Carroll, \email{nathan.carroll@ur.de}
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{maxLik}}, \code{\link{margins.oglmx}}, \code{\link{polr}}.
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
# create random sample, three variables, two binary.
set.seed(242)
n<-250
x1<-sample(c(0,1),n,replace=TRUE,prob=c(0.75,0.25))
x2<-vector("numeric",n)
x2[x1==0]<-sample(c(0,1),n-sum(x1==1),replace=TRUE,prob=c(2/3,1/3))
z<-rnorm(n,0.5)
# create latent outcome variable
latenty<-0.5+1.5*x1-0.5*x2+0.5*z+rnorm(n,sd=exp(0.5*x1-0.5*x2))
# observed y has four possible values: -1,0,1,2
# threshold values are: -0.5, 0.5, 1.5.
y<-vector("numeric",n)
y[latenty< -0.5]<--1
y[latenty>= -0.5 & latenty<0.5]<- 0
y[latenty>= 0.5 & latenty<1.5]<- 1
y[latenty>= 1.5]<- 2
dataset<-data.frame(y,x1,x2)
# estimate standard ordered probit
results.oprob<-oglmx(y ~ x1 + x2 + z, data=dataset,link="probit",constantMEAN=FALSE,
                     constantSD=FALSE,delta=0,threshparam=NULL)
coef(results.oprob) # extract estimated coefficients
summary(results.oprob)
# calculate marginal effects at means
margins.oglmx(results.oprob)
# estimate ordered probit with heteroskedasticity
results.oprobhet<-oglmx(y ~ x1 + x2 + z, ~ x1 + x2, data=dataset, link="probit",
                     constantMEAN=FALSE, constantSD=FALSE,threshparam=NULL)
summary(results.oprobhet)
library("lmtest")
# likelihood ratio test to compare model with and without heteroskedasticity.
lrtest(results.oprob,results.oprobhet)
# calculate marginal effects at means.
margins.oglmx(results.oprobhet)
# scale of parameter values is meaningless. Suppose instead two of the 
# three threshold values were known, then can include constants in the 
# mean and standard deviation equation and the scale is meaningful.
results.oprobhet1<-oglmx(y ~ x1 + x2 + z, ~ x1 + x2, data=dataset, link="probit",
                        constantMEAN=TRUE, constantSD=TRUE,threshparam=c(-0.5,0.5,NA))
summary(results.oprobhet1)
margins.oglmx(results.oprobhet1)
# marginal effects are identical to results.oprobithet, but using the true thresholds
# means the estimated parameters are on the same scale as underlying data.
# can choose any two of the threshold values and get broadly the same result.
results.oprobhet2<-oglmx(y ~ x1 + x2 + z, ~ x1 + x2, data=dataset, link="probit",
                         constantMEAN=TRUE, constantSD=TRUE,threshparam=c(-0.5,NA,1.5))
summary(results.oprobhet2)
margins.oglmx(results.oprobhet2)
# marginal effects are again identical. Parameter estimates do change.
%##---- Should be DIRECTLY executable !! ----
%##-- ==>  Define data, use random,
%##--	or do  help(data=index)  for the standard data sets.
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
