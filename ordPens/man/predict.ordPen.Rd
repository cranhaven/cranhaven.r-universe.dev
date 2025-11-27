\name{predict.ordPen}
\alias{predict.ordPen}

\title{Predict method for ordPen objects}
\description{
  Obtains predictions from an \code{ordPen} object.
}
\usage{
\method{predict}{ordPen}(object, newx, newu = NULL, newz = NULL,
  offset = rep(0,nrow(as.matrix(newx))), 
  type = c("link", "response", "class"), ...)
}

\arguments{
  \item{object}{an \code{ordPen} object.}
  \item{newx}{the matrix (or \code{data.frame}) of new observations of the
    considered ordinal predictors, with each column corresponding to
    one predictor and containing numeric values from \{1,2,...\}.}
  \item{newu}{a matrix (or \code{data.frame}) of new observations of the
    additional categorical (nominal) predictors, with each column corresponding
    to one (additional) predictor and containing numeric values \{1,2,...\}.}
  \item{newz}{a matrix (or \code{data.frame}) of new observations of the
    additional metric predictors, with each column corresponding to one
    (additional) predictor.}
  \item{offset}{potential offset values.}
  \item{type}{the type of prediction; \code{type = "link"} is on the
    scale of linear predictors, whereas \code{type = "response"} is on
    the scale of the response variable, i.e., \code{type = "response"}
    applies the inverse link function to the linear predictors. \code{type = "class"} is only available for cumulative logit models and returns the class number with the highest fitted probability.}
  \item{...}{additional arguments (not supported at this time).}
}
\value{
  A matrix of predictions whose columns correspond to the different values of
  the penalty parameter \code{lambda} of the \code{ordPen} object.
}

\author{Jan Gertheiss, Aisouda Hoshiyar}

\seealso{\code{\link{ordSelect}}, \code{\link{ordSmooth}}, \code{\link{ordFusion}}}

\examples{
# the training data
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

# selecting and/or smoothing/fusing
o1 <- ordSmooth(x = x, y = y, lambda = lambda)
o2 <- ordSelect(x = x, y = y, lambda = lambda)
o3 <- ordFusion(x = x, y = y, lambda = lambda)

# new data
x1 <- sample(1:8,10,replace=TRUE)
x2 <- sample(1:6,10,replace=TRUE)
x3 <- sample(1:7,10,replace=TRUE)
newx <- cbind(x1,x2,x3)

# prediction
round(predict(o1, newx), digits=3)
round(predict(o2, newx), digits=3)
round(predict(o3, newx), digits=3)
}

\keyword{methods}

