\name{mspeFHdb}
\alias{mspeFHdb}
\title{
Compute MSPE through double bootstrap method for Fay Herriot model
}
\description{
This function returns MSPE estimate with double bootstrap appoximation method for Fay Herriot model.
}
\usage{
mspeFHdb(formula, data, D, K = 50, C = 50, method = 1, na_rm, na_omit)
}

\arguments{
  \item{formula}{
(formula). Stands for the model formula that specifies the auxiliary variables to be used in the regression model. 
    This should follow the R model formula syntax.
}
  \item{data}{
(data frame). It represents the data containing the response values and auxiliary variables for the Nested Error Regression Model.
}
  \item{D}{
(vector). It represents the knowing sampling variance for Fay Herriot model.
}
  \item{K}{
(integer). It represents the first bootstrap sample number. Default value is 50.
}
  \item{C}{
(integer). It represents the second bootstrap sample number. Default value is 50.
}
  \item{method}{
It represents the variance component estimation method. See "Details".
}
  \item{na_rm}{
  A logical value indicating whether to remove missing values (NaN) from the input matrices and vectors. 
  If \code{TRUE}, missing values in the input data (\code{X}, \code{Y}, and \code{D}) are automatically cleaned using internal functions.
  If \code{FALSE}, missing values are not removed. Defaults to \code{FALSE}.
}
  \item{na_omit}{
  A logical value indicating whether to stop the execution if missing values (NaN) are present in the input data. 
  If \code{TRUE}, the function will check for missing values in \code{X}, \code{Y}, and \code{D}. 
  If any missing values are found, an error message will be raised, prompting the user to handle the missing data before proceeding. 
  Defaults to \code{FALSE}.
}
}
\details{
This method was proposed by P. Hall and T. Maiti. Double bootstrap method uses boostrap tool twice for Fay Herriot model to avoid the unattractivitive bias correction: one is to estimate the estimator bias, the other is to correct for bias.

Default value for \code{method} is 1, \code{method = 1} represents the MOM method, \code{method = 2} and \code{method = 3} represents ML and REML method, respectively.


}
\value{
A list with components:
\item{MSPE}{(vector) MSPE estimate based on double bootstrap method.}
\item{bhat}{(vector) estimate of the unknown regression coefficients.}
\item{Ahat}{(numeric) estimate of the variance component.}
}

\references{
P. Hall and T. Maiti. On parametric bootstrap methods for small area prediction. \emph{Journal of the Royal Statistical Society: Series B (Statistical Methodology)}, 2006.
}
\author{
Peiwen Xiao, Xiaohui Liu, Yu Zhang, Yuzi Liu, Jiming Jiang
}


\examples{
X <- matrix(runif(10 * 3), 10, 3)
X[,1] <- rep(1, 10) 
D <- (1:10) / 10 + 0.5
Y <- X \%*\% c(0.5, 1, 1.5) + rnorm(10, 0, sqrt(2)) + rnorm(10, 0, sqrt(D))
data <- data.frame(Y = Y, X1 = X[,2], X2 = X[,3])
formula <- Y ~ X1 + X2
result <- mspeFHdb(formula, data, D, K = 10, C = 10, method = 1)
}

