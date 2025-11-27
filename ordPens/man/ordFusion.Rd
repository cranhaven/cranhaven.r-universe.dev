\name{ordFusion}
\alias{ordFusion}
\title{Fusion and selection of dummy coefficients of ordinal predictors}
\description{Fits dummy coefficients of ordinally scaled independent variables
with a fused lasso penalty on differences of adjacent dummy coefficients. Using the \code{ordinalNet} algorithm if cumulative logit model is fitted, otherwise \code{glmpath} algorithm is used.}

\usage{
ordFusion(x, y, u = NULL, z = NULL, offset = rep(0,length(y)), lambda,  
  model = c("linear", "logit", "poisson", "cumulative"), 
  restriction = c("refcat", "effect"), scalex = TRUE, nonpenx = NULL, 
  frac.arclength = NULL, ...)
}
\arguments{
  \item{x}{the matrix of ordinal predictors, with each column corresponding to
    one predictor and containing numeric values from \{1,2,...\}; for each 
    covariate, category 1 is taken as reference category with zero dummy 
    coefficient.}
  \item{y}{the response vector.}
  \item{u}{a matrix (or \code{data.frame}) of additional categorical (nominal) 
    predictors, with each column corresponding to one (additional) predictor and
    containing numeric values from \{1,2,...\}; corresponding dummy coefficients
    will not be penalized, and for each covariate category 1 is taken as reference category. Curretnly not supported if \code{model == "cumulative"}.}
  \item{z}{a matrix (or \code{data.frame}) of additional metric predictors, with 
    each column corresponding to one (additional) predictor; corresponding
    coefficients will not be penalized. Curretnly not supported if \code{model == "cumulative"}.}
  \item{offset}{vector of offset values.}
  \item{lambda}{vector of penalty parameters, i.e., lambda values.}
  \item{model}{the model which is to be fitted. Possible choices are "linear"
    (default), "logit", "poisson" or "cumulative". See details below.}
  \item{restriction}{identifiability restriction for dummy coding. "reference" takes category 1 is as reference category (default), while with "effect" dummy coefficients sum up to 0 (known as effect coding).}
  \item{scalex}{logical. Should (split-coded) design matrix corresponding to
    \code{x} be scaled to have unit variance over columns before fitting? See details below.}
  \item{nonpenx}{vectors of indices indicating columns of
    \code{x} whose regression coefficients are not penalized. Curretnly not supported if \code{model == "cumulative"}.}
  \item{frac.arclength}{just in case the corresponding \code{glmpath} argument is to be modified; default is \code{1} for \code{model == "linear"}, and \code{0.1} otherwise.}  
  \item{...}{additional arguments to \code{ordinalNet} (if \code{model == "cumulative"}) or \code{glmpath}.}
}
\details{
  The method assumes that categorical covariates (contained in \code{x} and 
  \code{u}) take values 1,2,...,max, where max denotes the (columnwise) highest 
  level observed in the data. If any level between 1 and max is not observed for an ordinal predictor, 
  a corresponding (dummy) coefficient is fitted anyway (by linear interpolation, due to some additional but small quadratic penalty, see \code{glmpath} for details). If any level > max is 
  not observed but possible in principle, and a corresponding coefficient is to 
  be fitted, the easiest way is to add a corresponding row to \code{x} (and 
  \code{u},\code{z}) with corresponding \code{y} value being \code{NA}.

  
  If a linear regression model is fitted, response vector \code{y} may contain 
  any numeric values; if a logit model is fitted, \code{y} has to be 0/1 coded;
  if a poisson model is fitted, \code{y} has to contain count data. If a cumulative   logit model is fitted, \code{y} takes values 1,2,...,max.
  
  
  If \code{scalex} is \code{TRUE}, (split-coded) design matrix constructed from \code{x} is scaled to have 
  unit variance over columns (see \code{standardize} argument of \code{glmpath} or/and \code{ordinalNet}).
}

\value{An \code{ordPen} object, which is a list containing:
  \item{fitted}{the matrix of fitted response values of the training data.
    Columns correspond to different \code{lambda} values.}
  \item{coefficients}{the matrix of fitted coefficients with respect to 
    dummy-coded (ordinal or nominal) categorical input variables (including the 
    reference category) as well as metric predictors. Columns correspond to 
    different lambda values.}
  \item{model}{the type of the fitted model: "linear", "logit", "poisson", or "cumulative".}
  \item{restriction}{the type of restriction used for identifiability.}
  \item{lambda}{the used lambda values.}
  \item{xlevels}{a vector giving the number of levels of the ordinal predictors.}
  \item{ulevels}{a vector giving the number of levels of the nominal predictors (if any).}
  \item{zcovars}{the number of metric covariates (if any).}
}
\references{
  Gertheiss, J. and G. Tutz (2010). \emph{Sparse modeling of categorial explanatory
  variables}. The Annals of Applied Statistics, 4, 2150-2180.

  Hoshiyar, A., Gertheiss, L.H., and Gertheiss, J. (2023). \emph{Regularization and    Model Selection for Item-on-Items Regression with Applications to Food Products' Survey Data.}  Preprint, available from https://arxiv.org/abs/2309.16373.
  
  Park, M.Y. and T. Hastie (2007). \emph{L1 regularization path algorithm for 
  generalized linear models}. Journal of the Royal Statistical Society B, 69, 659-677.
  
  Tutz, G. and J. Gertheiss (2014). \emph{Rating scales as predictors -- the old
  question of scale level and some answers}. Psychometrica, 79, 357-376.
  
  Tutz, G. and J. Gertheiss (2016). \emph{Regularized regression for categorical
  data}. Statistical Modelling, 16, 161-200.

 }

\author{Jan Gertheiss, Aisouda Hoshiyar}

\seealso{\code{\link{plot.ordPen}}, \code{\link{predict.ordPen}}, 
\code{\link{ICFCoreSetCWP}}}


\examples{
# fusion and selection of ordinal covariates on a simulated dataset
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
lambda <- c(80,70,60,50,40,30,20,10,5,1) 

# fusion and selection
ofu <- ordFusion(x = x, y = y, lambda = lambda)

# results
round(ofu$coef,digits=3)
plot(ofu)

# If for a certain plot the x-axis should be annotated in a different way,
# this can (for example) be done as follows:
plot(ofu, whx = 1, xlim = c(0,9), xaxt = "n")
axis(side = 1, at = c(1,8), labels = c("no agreement","total agreement"))
} 
 
\keyword{models}
\keyword{regression}

