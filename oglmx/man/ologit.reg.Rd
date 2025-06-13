\name{ologit.reg}
\alias{ologit.reg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Fit an ordered Logit model.
%%  ~~function to do ... ~~
}
\description{
  Wrapper function for \code{\link[oglmx]{oglmx}} to estimate an ordered Logit model. 
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ologit.reg(formula, data, start = NULL, weights=NULL, beta = NULL, threshparam = NULL,
           analhessian = TRUE, na.action, savemodelframe = FALSE, robust = FALSE, 
           Force = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{
  an object of class \code{\link{formula}}: a symbolic description of the model used to explain the mean of the latent variable. The response variable should be a numeric vector or factor variable such that the numerical assignments for the levels of the factor have ordinal meaning.
%%     ~~Describe \code{formula} here~~
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
  \item{beta}{
  \code{NULL} or numeric vector. Used to prespecify elements of the parameter vector for the equation of the mean of the latent variable. Vector should be of length one or of length equal to the number of explanatory variables in the mean equation. \code{NA} should be entered for elements of the vector to be estimated.
%%     ~~Describe \code{beta} here~~
}
  \item{threshparam}{
  numeric vector. Used to prespecify the threshold parameters of the model. Vector should be of length equal to the number of outcomes minus one. \code{NA} should be entered for threshold parameters to be estimated by the model.
%%     ~~Describe \code{threshparam} here~~
}
  \item{analhessian}{
  logical. Indicates whether the analytic Hessian should be calculated and used, default is TRUE, if set to FALSE a finite-difference approximation of the Hessian is used.
%%     ~~Describe \code{analhessian} here~~
}
 \item{na.action}{
  a function which indicates what should happen when the data contain NAs. The default is set by the \code{na.action} setting of \code{\link{options}}, and is \code{\link{na.fail}} if that is unset. The factory-fresh default is \code{\link{na.omit}}. Another possible value is \code{NULL}, no action. Value \code{\link{na.exclude}} can be useful.
%%     ~~Describe \code{na.action} here~~
}
  \item{savemodelframe}{
  logical. Indicates whether the model frame(s) should be saved for future use. Default is \code{FALSE}. Should be switched to \code{TRUE} if intending to estimate Average Marginal Effects.
%%     ~~Describe \code{savemodelframe} here~~
}
  \item{robust}{
  logical. If set to \code{TRUE} the outer product or BHHH estimate of the meat in the sandwich of the variance-covariance matrix is calculated. If calculated standard errors will be calculated using the sandwich estimator by default when calling \code{summary}.
  }
  \item{Force}{
  logical. If set to \code{FALSE} (the default) the function stops if the response variable has more than twenty categories. Should be changed to \code{TRUE} if a model with more than twenty categories is desired.
%%     ~~Describe \code{Force} here~~
}
}
\details{
  object of class "\code{oglmx}", see \code{\link{oglmx}}.
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  object of class "\code{oglmx}", see \code{\link{oglmx}}.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
  Nathan Carroll, \email{nathan.carroll@ur.de}
%%  ~~who you are~~
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link[MASS]{polr}} for alternative method to estimate an ordered logit model. \code{\link[oglmx]{oglmx}}. To obtain marginal effects see \code{\link{margins.oglmx}}.
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
%\examples{
%##---- Should be DIRECTLY executable !! ----
%##-- ==>  Define data, use random,
%##--	or do  help(data=index)  for the standard data sets.
%
%## The function is currently defined as
%function (formula, data, start = NULL, beta = NULL, analhessian = TRUE, 
%    na.action = TRUE, savemodelframe = FALSE) 
%{
%    value <- oglmx(formulaMEAN = formula, data = data, start = start, 
%        beta = beta, analhessian = analhessian, na.action = na.action, 
%        savemodelframe = savemodelframe, link = "logit", constantMEAN = FALSE, 
%        constantSD = FALSE, delta = 0, threshparam = NULL)
%    return(value)
%  }
%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
