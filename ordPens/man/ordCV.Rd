\name{ordCV}
\alias{ordCV}
\title{Cross-validation for penalized regression with ordinal predictors.}
\description{Performs k-fold cross-validation in order to evaluate the performance and/or select an optimal smoothing parameter of a penalized regression model with ordinal predictors.}

\usage{
ordCV(x, y, u = NULL, z = NULL, k=5, lambda, offset = rep(0,length(y)), 
  model = c("linear", "logit", "poisson", "cumulative"), 
  type=c("selection", "fusion"), ...)
}
\arguments{
  \item{x}{matrix of integers 1,2,... giving the observed levels
  of the ordinal factor(s).}
  \item{y}{the vector of response values.}
  \item{u}{a matrix (or \code{data.frame}) of additional categorical (nominal) 
    predictors, with each column corresponding to one (additional) predictor and
    containing numeric values from \{1,2,...\}; corresponding dummy coefficients
    will not be penalized, and for each covariate category 1 is taken as reference category. Currently not supported if \code{model="cumulative"}.}
  \item{z}{a matrix (or \code{data.frame}) of additional metric predictors, with 
    each column corresponding to one (additional) predictor; corresponding
    coefficients will not be penalized. Currently not supported if \code{model="cumulative"}.}
  \item{k}{number of folds.}
  \item{lambda}{vector of penalty parameters (in decreasing order).}
  \item{offset}{vector of offset values.} 
  \item{model}{the model which is to be fitted. Possible choices are "linear"
    (default), "logit", "poisson" or "cumulative". See details below.}
  \item{type}{penalty to be applied. If "selection", group lasso penalty for smoothing and selection is used. If "fusion", a fused lasso penalty for fusion and selection is used.}
  \item{...}{additional arguments to \code{\link{ordFusion}} and
  \code{\link{ordSelect}}, respectively.}
}
\details{
   The method assumes that categorical covariates (contained in \code{x} and 
  \code{u}) take values 1,2,...,max, where max denotes the (columnwise) highest 
  level observed in the data. If any level between 1 and max is not observed for an ordinal predictor, 
  a corresponding (dummy) coefficient is fitted anyway. If any level > max is 
  not observed but possible in principle, and a corresponding coefficient is to 
  be fitted, the easiest way is to add a corresponding row to \code{x} (and 
  \code{u},\code{z}) with corresponding \code{y} value being \code{NA}.

  
  If a linear regression model is fitted, response vector \code{y} may contain 
  any numeric values; if a logit model is fitted, \code{y} has to be 0/1 coded;
  if a poisson model is fitted, \code{y} has to contain count data. If a cumulative   logit model is fitted, \code{y} takes values 1,2,...,max. 
  
  
  
 For the cumulative model, the measure of performance used by the function is the brier score, being the sum of squared differences between (indicator) outcome and predicted probabilities  \eqn{P(Y_i=r)=P(y_{ir})=\pi_{ir}}, with observations \eqn{i=1,...,n} and classes \eqn{r=1,...,c}. Otherwise, the deviance is used.
}
  
  

\value{
Returns a list containing the following components:
  \item{Train}{matrix of size (\code{k} \eqn{x} \code{length(lambda)}) containing brier/deviance scores on the training data. }
  \item{Test}{Brier/deviance score matrix when looking at the test data set.}
}

\references{
  Hoshiyar, A., Gertheiss, L.H., and Gertheiss, J. (2023). \emph{Regularization and     Model Selection for Item-on-Items Regression with Applications to Food Products' Survey Data.} Preprint, available from https://arxiv.org/abs/2309.16373.
} 

\author{Aisouda Hoshiyar}

\seealso{\code{\link{ordSelect}}, \code{\link{ordFusion}}}

 
\keyword{cross validation}
