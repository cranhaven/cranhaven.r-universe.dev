\name{simplexreg}
\alias{simplexreg}
\alias{print.simplexreg}
\alias{simplexreg.fit}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Simplex Generalized Linear Model Regression Function
%%  ~~function to do ... ~~
}
\description{Regression Analysis of Proportional Data Using Various Types of 
Simplex Models  
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
simplexreg(formula, data, subset, na.action, 
   	link = c("logit", "probit", "cloglog", "neglog"), corr = "Ind", id = NULL, 
   	control = simplexreg.control(...), model = TRUE, y = TRUE, x = TRUE, ...)
	
simplexreg.fit(y, x, z = NULL, t = NULL, link = "logit", corr = "Ind",
   	id = NULL, control = simplexreg.control())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{a symbolic description of the model to be fitted(of type y ~ x or y ~ x | z | t. 
  The Details are given under 'Details'). 
%%     ~~Describe \code{y} here~~
}
  \item{data}{an optional data frame, list or environment containing variables in \code{formula} 
  and \code{id}.
%%     ~~Describe \code{X} here~~
}
  \item{subset, na.action}{arguments controlling formula processing via \code{\link{model.frame}}}
%%  \item{type}{type of specification of dispersion, see 'Details'
%%     ~~Describe \code{type} here~~
%%}
  \item{link}{type of link function to the mean. Currently, \code{"logit"}(logit function), 
  \code{"probit"}(probit function), \code{"cloglog"}(complementary log-log function), 
  \code{"neglog"}(negative log function) are supported. 
%%     ~~Describe \code{link} here~~
}
  \item{corr}{the covariance structure, chosen from \code{"Ind"}(independent structure), 
  \code{"Exc"}(exchangeability) and \code{"AR1"}(AR(1)), see Details}
  \item{id}{a factor identifies the clusters when \code{gee = TRUE}. The length of \code{id}
  should be the same as the number of observations. \code{y}, \code{x}, \code{z}, \code{t} are 
  assumed to be sorted in accordance with clusters specified by \code{id}}
  \item{control}{a list of control argument via \code{\link{simplexreg.control}}}
  \item{model}{a logical value indicating whether \emph{model frame} should be included
  as a component of the return value}
  \item{y, x}{For \code{simplexreg}:logical values indicating whether response vector 
  and covariates modelling the mean parameter should be returned as components of the 
  returned value
  For \code{simplexreg.fit}:\code{x} is the design matrix and \code{y} is the response vector}
  \item{z}{regressor matrix modelling the dispersion parameter}
  \item{t}{time covariate in the correlation structure, see Details}
  \item{...}{argument passed to \code{\link{simplexreg.control}}}
}
\details{Outcomes of continuous proportions arise in many applied areas. Such data could 
be properly modelled using simplex regression. See also \code{\link{simplex}}. The mean and 
dispersion parameters are linked to set of regressors. Regression analysis of the simplex model 
is implemented in \code{simplexreg}. If \code{corr = "Ind"}, simplex generalized regression model
is employed. Estimations is performed by maximum likelihood via Fisher scoring technique. 

Apart from including generalized simplex regression models, this function also provides users with 
generalized estimating equations (GEE) techniques to model longitudinal proportional response. 
Exchangeability and AR(1) structures are available. Parameter estimation and residual analysis 
are involved. 

We employ the specification approach designed in the fitting model function \code{betareg} of
beta regression in the package \pkg{betareg}. As for simplex regression models, assuming the dispersion
is homogeneous, the response is linked to a linear predictor described by \code{y ~ x1 + x2} using a \code{link} 
function. Four types of function are available linking the regressors to the mean. However, for dispersion, 
the \code{link} function is restricted to logarithm function. When modeling dispersion, the regressor 
modelling the dispersion parameter should be specified in a formula form of type \code{y ~ x1 + x2 | z1 + z2} 
where \code{z1} and \code{z2} are linked to the dispersion parameter \eqn{\sigma^2}.

Model specification is a bit complicated when it comes to modelling longitudinal proportional response. 
Song \emph{et. al} (2004) proposed a marginal simplex model consists of three components, the 
population-average effects, the pattern of dispersion and the correlation structure. Let the percentage 
responses for the \eqn{i}th subject be \eqn{y_{ij}}, observed at time \eqn{t_{ij}}. If \code{corr = "AR1"}, 
the working covariance matrix of \eqn{y_{ij}}, 
\eqn{j = 1, 2, ..., n_i}, is 
\deqn{{\exp(\alpha * |t_{ik} - t_{ij}|)}_{kj}}{{exp(\alpha * |t_{ik} - t_{ij}|)}_{kj}}
where \eqn{\alpha < 0} and \eqn{\exp(\alpha)}{exp(\alpha)} is the lag-1 autocorrelation. If 
\code{corr = "Exc"}, the covariance matrix will be \eqn{(1 - exp(\alpha)) I + exp(\alpha) 1}
where I is the identity matrix while 1 the matrix with all elements being equal to one.  

For homogeneous dispersion, the formula is supposed to be of the form \code{y ~ x1 + x2 | 1 | t} where \eqn{t} is the
time covariate. Otherwise, the formula will be of the form \code{y ~ x1 + x2 | z1 + z2 | t}.
        	
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
  \item{fixef}{estimates of coefficients modelling the mean as well 
  as the standard deviation}
  \item{dispar}{estimates of coefficients modelling dispersion as 
  well as the standard deviation}
  \item{Dispersion}{estimate of the dispersion parameter}
  \item{appstdPerr}{approximated standard deviations of the regression coefficients}
  \item{stdPerr}{exact standard deviations of the regression coefficients}
  \item{meanmu}{estimate of mean parameter}
  \item{adjvar}{adjusted dependent variable \eqn{s_i}. Details could be found in 
  McCullagh and Nelder (1989)} 
  \item{stdscor}{standardised score residuals. Details can be found in Song \emph{et al.} (2004)}
  \item{predict}{predicted values of \eqn{g(\mu_i)} where g is the link function and 
  \eqn{\mu_i} the mean parameter}
  \item{loglike}{value of maximum log-likelihood function}
  \item{deviance}{value of deviance}
  \item{call}{the original function call}
  \item{formula}{the original formula}
  \item{terms}{a list with elements \code{"mean"} and \code{"dispersion"} containing term object for the model}
  \item{levels}{a list with elements \code{"mean"} and \code{"dispersion"} containing levels of categorical regressors}
  \item{link}{type of function linking to the mean}
  \item{type}{type = \code{"homo"} for homogeneous dispersion while type = \code{"hetero"} for 
  heterogeneous dispersion}
  \item{model}{the full model frame (if \code{model = TRUE})}
  \item{y}{response vector (if \code{y = TRUE})}
  \item{x}{a list with elements \code{mean}, \code{dispersion}, \code{time} and \code{id} containing 
  corresponding variables}
  \item{n}{number of proportional observations}
  \item{iter}{number of Fisher iterations}
  \item{...}{argument passed to {\code{\link{simplexreg.control}}}}
}
\references{Barndorff-Nielsen,  O.E. and  Jorgensen, B. (1991)
     Some parametric models on the simplex.
     \emph{ Journal of  Multivariate Analysis,} \bold{ 39:} 106--116
     
     Jorgensen, B. (1997)
   \emph{ The Theory of Dispersion Models}.  London: Chapman and Hall
   
   McCullagh, P and Nelder J. (1989)
   \emph{ Generalized Linear Models}. London: Chapman and Hall
   
   Song, P. and Qiu, Z. and Tan, M. (2004) Modelling Heterogeneous Dispersion in 
   Marginal Models for Longitudinal Proportional Data. \emph{ Biometrical Journal,} 
   \bold{ 46:} 540--553
   
   Zhang, P. and Qiu, Z. and Shi, C. (2016) {simplexreg}: An {R} Package for Regression
	Analysis of Proportional Data Using the Simplex Distribution. \emph{ Journal of Statistical Software,}
	\bold{ 71:} 1--21
%% ~put references to the literature/web site here ~
}
\author{Zhenguo Qiu, Peng Zhang and Chengchun Shi
%%  ~~who you are~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{simplex}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
# GLM models
data("sdac", package = "simplexreg")
sim.glm1 <- simplexreg(rcd~ageadj+chemo, link = "logit", 
  data = sdac)
sim.glm2 <- simplexreg(rcd~ageadj+chemo|age, link = "logit", 
  data = sdac)

# GEE models
data("retinal", package = "simplexreg")
sim.gee1 <- simplexreg(Gas~LogT+LogT2+Level|1|Time, link = "logit", 
  corr = "Exc", id = ID, data = retinal)
sim.gee2 <- simplexreg(Gas~LogT+LogT2+Level|LogT+Level|Time, 
  link = "logit", corr = "AR1", id = ID, data = retinal)	
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ simplex regression }
