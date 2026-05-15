\name{mspeNERpb}
\alias{mspeNERpb}
\title{
Compute MSPE through parameter bootstrap method for Nested error regression model
}
\description{
This function returns MSPE estimator with parameter bootstrap appoximation method for Nested error regression model
}
\usage{
mspeNERpb(ni, formula, data, Xmean, K = 50, method = 4, na_rm, na_omit)
}

\arguments{
  \item{ni}{
(vector). It represents the sample number for every small area.
}
  \item{formula}{
(formula). Stands for the model formula that specifies the auxiliary variables to be used in the regression model. 
    This should follow the R model formula syntax.
}
  \item{data}{
(data frame). It represents the data containing the response values and auxiliary variables for the Nested Error Regression Model.
}
  \item{Xmean}{
(matrix). Stands for the population mean of auxiliary values.
}
  \item{K}{
(integer). It represents the bootstrap sample number. Default value is 50.
}
  \item{method}{
The variance component estimation method to be used. See "Details".
}
  \item{na_rm}{
  A logical value indicating whether to remove missing values (NaN) from the input matrices and vectors. 
  If \code{TRUE}, missing values in the input data (\code{X}, \code{Y}, \code{ni}, and \code{Xmean}) are automatically cleaned using internal functions.
  If \code{FALSE}, missing values are not removed. Defaults to \code{FALSE}.
}
  \item{na_omit}{
  A logical value indicating whether to stop the execution if missing values (NaN) are present in the input data. 
  If \code{TRUE}, the function will check for missing values in \code{X}, \code{Y}, \code{ni}, and \code{Xmean}. 
  If any missing values are found, an error message will be raised, prompting the user to handle the missing data before proceeding. 
  Defaults to \code{FALSE}.
}

}
\details{
This method was proposed by Peter Hall and T. Maiti. Parametric bootstrap (pb) method uses bootstrap-based method to measure the accuracy of EB estimator. In this case, only EB estimator is available (\code{method = 4}).

}
\value{
This function returns a list with components:
\item{MSPE}{(vector) MSPE estimates for NER model.}
\item{bhat}{(vector) Estimates of the unknown regression coefficients.}
\item{sigvhat2}{(numeric) Estimates of the area-specific variance component.}
\item{sigehat2}{(numeric) Estimates of the random error variance component.}
}
\references{
F. B. Butar and P. Lahiri. On measures of uncertainty of empirical bayes small area estimators. \emph{Journal of Statistical Planning and Inference}, 112(1-2):63-76, 2003.

N. G. N. Prasad and J. N. K. Rao. The estimation of the mean squared error of small-area estimators. \emph{Journal of the American Statistical Association}, 85(409):163-171, 1990.

Peter Hall and T. Maiti. On parametric bootstrap methods for small area prediction. \emph{Journal of the Royal Statistical Society: Series B (Statistical Methodology)}, 2006a.

H. T. Maiti and T. Maiti. Nonparametric estimation of mean squared prediction error in nested error regression models. \emph{Annals of Statistics]}, 34(4):1733-1750, 2006b.
}
\author{
Peiwen Xiao, Xiaohui Liu, Yu Zhang, Yuzi Liu, Jiming Jiang
}


\examples{
Ni <- 1000
sigmaX <- 1.5
K <- 50
C <- 50
m <- 10
beta <- c(0.5, 1)
sigma_v2 <- 0.8
sigma_e2 <- 1
ni <- sample(seq(1, 10), m, replace = TRUE)
n <- sum(ni)
p <- length(beta)
### population function
pop.model <- function(Ni, sigmaX, beta, sigma_v2, sigma_e2, m) {
  x <- rnorm(m * Ni, 1, sqrt(sigmaX))
  v <- rnorm(m, 0, sqrt(sigma_v2))
  y <- numeric(m * Ni)
  theta <- numeric(m)
  kk <- 1
  for (i in 1:m) {
    sumx <- 0
    for (j in 1:Ni) {
      sumx <- sumx + x[kk]
      y[kk] <- beta[1] + beta[2] * x[kk] + v[i] + rnorm(1, 0, sqrt(sigma_e2))
      kk <- kk + 1
    }
    meanx <- sumx / Ni
    theta[i] <- beta[1] + beta[2] * meanx + v[i]
  }
  group <- rep(seq(m), each = Ni)
  data <- data.frame(y = y, group = group, x1 = x)
  return(list(data = data, theta = theta))
} 

### sample function
sampleXY <- function(Ni, ni, m, Population) {
  Indx <- c()
  for (i in 1:m) {
    Indx <- c(Indx, sample(c(((i - 1) * Ni + 1):(i * Ni)), ni[i]))
  }
  Sample <- Population[Indx, ]
  return(Sample)
} 

### data generation process
Population <- pop.model(Ni, sigmaX, beta, sigma_v2, sigma_e2, m)$data
XY <- sampleXY(Ni, ni, m, Population)

formula <- y ~ x1
data <- XY

Xmean <- matrix(NA, m, p)
for (tt in 1:m) {
  Xmean[tt, ] <- colMeans(Population[which(Population$group == tt), "x1", drop = FALSE])
}

result <- mspeNERpb(ni, formula, data, Xmean, K = 50, method = 4)
}

