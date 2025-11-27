\name{ordSmooth}
\alias{ordSmooth}
\title{Smoothing dummy coefficients of ordinal predictors}
\description{Fits dummy coefficients of ordinally scaled independent variables
with the sum of squared differences of adjacent dummy coefficients being penalized.}

\usage{
ordSmooth(x, y, u = NULL, z = NULL, offset = rep(0,length(y)), lambda, 
  model = c("linear", "logit", "poisson"), restriction = c("refcat", "effect"),
  penscale = identity, scalex = TRUE, nonpenx = NULL, eps = 1e-3, delta = 1e-6, 
  maxit = 25, ...)
}
\arguments{
  \item{x}{the matrix (or \code{data.frame}) of ordinal predictors, with each 
    column corresponding to one predictor and containing numeric values from 
    \{1,2,...\}; for each covariate, category 1 is taken as reference category 
    with zero dummy coefficient.}
  \item{y}{the response vector.}
  \item{u}{a matrix (or \code{data.frame}) of additional categorical (nominal) 
    predictors, with each column corresponding to one (additional) predictor 
    and containing numeric values \{1,2,...\}; corresponding dummy coefficients
    will not be penalized, and for each covariate category 1 is taken as reference category.}
  \item{z}{a matrix (or \code{data.frame}) of additional metric predictors, with 
    each column corresponding to one (additional) predictor; corresponding
    coefficients will not be penalized.}
  \item{offset}{vector of offset values.}
  \item{lambda}{vector of penalty parameters (in decreasing order).
    Optimization starts with the first component. See details below.}
  \item{model}{the model which is to be fitted. Possible choices are "linear"
    (default), "logit" or "poisson". See details below.}
   \item{restriction}{identifiability restriction for dummy coding. "reference" takes category 1 is as reference category (default), while with "effect" dummy coefficients sum up to 0 (known as effect coding).}
  \item{penscale}{rescaling function to adjust the value of the penalty
    parameter to the degrees of freedom of the parameter group.}
  \item{scalex}{logical. Should (split-coded) design matrix corresponding to \code{x} be scaled to have unit variance over columns before fitting? See details below.}
  \item{nonpenx}{vector of indices indicating columns of
    \code{x} whose regression coefficients are not penalized.}
  \item{eps}{a (small) constant to be added to the columnwise standard
    deviations when scaling the design matrix, to control the effect of very small
    stds. See details below.}
  \item{delta}{a small positive convergence tolerance which is used as stopping
    criterion for the penalized Fisher scoring when a logit or poisson model
    is fitted. See details below.}
  \item{maxit}{integer given the maximal number of (penalized) Fisher scoring
    iterations.}
  \item{...}{additional arguments.}
}
\details{
  The method assumes that categorical covariates (contained in \code{x} and 
  \code{u}) take values 1,2,...,max, where max denotes the (columnwise) highest 
  level observed in the data. If any level between 1 and max is not observed for an ordinal predictor, 
  a corresponding (dummy) coefficient is fitted anyway. If any level > max is 
  not observed but possible, and a corresponding coefficient is to 
  be fitted, the easiest way is to add a corresponding row to \code{x} (and 
  \code{u},\code{z}) with corresponding \code{y} value being \code{NA}.
  

  If a linear regression model is fitted, response vector \code{y} may contain 
  any numeric values; if a logit model is fitted, \code{y} has to be 0/1 coded;
  if a poisson model is fitted, \code{y} has to contain count data. 
  
  
  If \code{scalex} is \code{TRUE}, (split-coded) design matrix constructed from \code{x} is scaled to have 
  unit variance over columns. If a certain \code{x}-category, 
  however, is observed only a few times, variances may become very small and
  scaling has enormous effects on the result and may cause numerical problems.
  Hence a small constant \code{eps} can be added to each standard deviation 
  when used for scaling. 
  
  
  A logit or poisson model is fitted by penalized Fisher scoring. For stopping 
  the iterations the criterion \code{sqrt(sum((b.new-b.old)^2)/sum(b.old^2)) < delta}
  is used. 
  
  
  Please note, \code{ordSmooth} is intended for use with high-dimensional ordinal predictors; more precisely, if the number of ordinal predictors is large. Package \code{ordPens}, however, also includes auxiliary functions such that \code{\link{gam}} from \code{\link{mgcv}} can be used for fitting generalized linear and additive models with first- and second-order ordinal smoothing penalty as well as built-in smoothing parameter selection. In addition, \code{\link{mgcv}} tools for further statistical inference can be used. Note, however, significance of smooth (ordinal) terms is only reliable in case of the second-order penalty. Also note, if using \code{\link{gam}}, dummy coefficients/fitted functions are centered over the data observed. For details, please see Gertheiss et al. (2021) and examples below.
}

\value{An \code{ordPen} object, which is a list containing:
  \item{fitted}{the matrix of fitted response values of the training data. 
    Columns correspond to different \code{lambda} values.}
  \item{coefficients}{the matrix of fitted coefficients with respect to 
    dummy-coded (ordinal or nominal) categorical input variables (including the
    reference category) as well as metric predictors. Columns correspond to 
    different lambda values.}
  \item{model}{the type of the fitted model: "linear", "logit", or "poisson".}
  \item{restriction}{the type of restriction used for identifiability.}
  \item{lambda}{the used lambda values.}
  \item{fraction}{the used fraction values (\code{NULL} in case of \code{ordSmooth}).}
  \item{xlevels}{a vector giving the number of levels of the ordinal predictors.}
  \item{ulevels}{a vector giving the number of levels of the nominal predictors (if any).}
  \item{zcovars}{the number of metric covariates (if any).}
}
\references{
Gertheiss, J., F. Scheipl, T. Lauer, and H. Ehrhardt (2022). \emph{Statistical 
inference for ordinal predictors in generalized linear and additive models with
application to bronchopulmonary dysplasia}. BMC research notes, 15, 112.

Gertheiss, J. and G. Tutz (2009). \emph{Penalized regression with ordinal 
predictors}. International Statistical Review, 77, 345-365.

Tutz, G. and J. Gertheiss (2014). \emph{Rating scales as predictors -- the old
  question of scale level and some answers}. Psychometrica, 79, 357-376.
  
Tutz, G. and J. Gertheiss (2016). \emph{Regularized regression for categorical data}. Statistical Modelling, 16, 161-200. 
}

\author{Jan Gertheiss, Aisouda Hoshiyar}

\seealso{\code{\link{plot.ordPen}}, \code{\link{predict.ordPen}}}


\examples{
# smooth modeling of a simulated dataset
set.seed(123)

# generate (ordinal) predictors
x1 <- sample(1:8,100,replace=TRUE)
x2 <- sample(1:6,100,replace=TRUE)
x3 <- sample(1:7,100,replace=TRUE)

# the response
y <- -1 + log(x1) + sin(3*(x2-1)/pi) + rnorm(100)

# x matrix
x <- cbind(x1,x2,x3)

# lambda values
lambda <- c(1000,500,200,100,50,30,20,10,1)

# smooth modeling
osm1 <- ordSmooth(x = x, y = y, lambda = lambda)

# results
round(osm1$coef,digits=3)
plot(osm1)

# If for a certain plot the x-axis should be annotated in a different way,
# this can (for example) be done as follows:
plot(osm1, whx = 1, xlim = c(0,9), xaxt = "n")
axis(side = 1, at = c(1,8), labels = c("no agreement","total agreement"))

# add a nominal covariate to control for
u1 <- sample(1:8,100,replace=TRUE)
u <- cbind(u1)
osm2 <- ordSmooth(x = x, y = y, u = u, lambda = lambda)
round(osm2$coef,digits=3)


## Use gam() from mgcv for model fitting:
# ordinal predictors need to be ordered factors
x1 <- as.ordered(x1)
x2 <- as.ordered(x2)
x3 <- as.ordered(x3)

# model fitting with first-order penalty and smoothing parameter selection by REML
gom1 <- gam(y ~ s(x1, bs = "ordinal", m = 1) + s(x2, bs = "ordinal", m = 1) + 
s(x3, bs = "ordinal", m = 1) + factor(u1), method = "REML")

# plot with confidence intervals
plot(gom1)

# use second-order penalty instead
gom2 <- gam(y ~ s(x1, bs = "ordinal", m = 2) + s(x2, bs = "ordinal", m = 2) + 
s(x3, bs = "ordinal", m = 2) + factor(u1), method = "REML")

# summary including significance of smooth terms
# please note, the latter is only reliable for m = 2
summary(gom2)

# plotting
plot(gom2)
}

\keyword{models}
\keyword{regression}

