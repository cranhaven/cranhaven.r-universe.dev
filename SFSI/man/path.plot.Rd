\name{Regression coefficients path}
\alias{path.plot}
\title{Coefficients path plot}
\usage{
path.plot(object, K = NULL, i = NULL,
          prune = FALSE, cor.max = 0.97,
          lambda.min = .Machine$double.eps^0.5,
          nbreaks.x = 6, npaths.max = 5000, ...)
}
\arguments{
\item{object}{An object of the 'LASSO' or 'SGP' class}

\item{K}{(numeric matrix) Kinship relationships. Only needed for an object of the class 'SGP'}

\item{i}{(integer vector) Index a response variable (columns of matrix \code{Gamma}) for an object of the class 'LASSO'. Index testing elements (stored in \code{object$tst}) for an object of the class 'SGP'. Default \code{i = NULL} will consider either all columns in matrix \code{Gamma} or all elements in \code{object$tst}, respectively}

\item{prune}{\code{TRUE} or \code{FALSE} to whether prune within groups of correlated coefficients, keeping only one per group. A group of coefficients that are highly correlated are likely to overlap in the plot}

\item{cor.max}{(numeric) Correlation threshold to prune within groups of correlated coefficients}

\item{lambda.min}{(numeric) Minimum value of lambda to show in the plot as \code{-log(lambda)}}

\item{nbreaks.x}{(integer) Number of breaks in the x-axis}

\item{npaths.max}{(integer) Maximum number of paths defined by the number of predictors times the number of columns of matrix \code{Gamma} for an object of the class 'LASSO'. This correspond to the number of training elements (stored in \code{object$trn}) times the number of testing elements (stored in \code{object$tst}) for an object of the class 'SGP'}

\item{...}{Other arguments for method \code{plot}: 'xlab', 'ylab', 'main', 'lwd'
}
}
\value{
Returns the plot of the coefficients' evolution path along the regularization parameter
}
\description{
Coefficients evolution path plot from an object of the class 'LASSO' or 'SGP'
}
\examples{
  require(SFSI)
  data(wheatHTP)
  
  index = which(Y$trial \%in\% 1:6)       # Use only a subset of data
  Y = Y[index,]
  X = scale(X_E1[index,])               # Reflectance data
  M = scale(M[index,])/sqrt(ncol(M))    # Subset and scale markers
  G = tcrossprod(M)                     # Genomic relationship matrix
  y = as.vector(scale(Y[,'E1']))        # Subset response variable
  
  # Sparse phenotypic regression
  fm = LARS(var(X),cov(X,y))
  \donttest{
  path.plot(fm)
  }
  # Sparse Genomic Prediction
  fm = SGP(y, K=G, trn=12:length(y), tst=1:11)
  \donttest{
  path.plot(fm, prune=TRUE)
  path.plot(fm, K=G, prune=TRUE, cor.max=0.9)
  }
  # Path plot for the first individual in testing set for the SGP
  path.plot(fm, K=G, i=1)
}

