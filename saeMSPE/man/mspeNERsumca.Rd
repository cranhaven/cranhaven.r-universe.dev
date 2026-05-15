\name{mspeNERsumca}
\alias{mspeNERsumca}
\title{
Compute MSPE through Sumca method for Nested error regression model
}
\description{
This function returns MSPE estimator with the combination of linearization and resampling appoximation method for Nested error regression model.
}
\usage{
mspeNERsumca(ni, formula, data, Xmean, K = 50, method = 1, na_rm, na_omit)
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
(integer). It represents the Monte-Carlo sample size for "Sumca". Default value is 50.
}
  \item{method}{
The MSPE estimation method to be used. See "Details".
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
This method was proposed by J. Jiang, P. Lahiri, and T. Nguyen, sumca method combines the advantages of linearization and resampling methods and obtains unified, positive, low-computation burden and second-order unbiased MSPE estimators.

Default value for \code{method} is 1, \code{method = 1} represents the MOM method, \code{method = 2} and \code{method = 3} represents ML and REML method, respectively.

}

\value{
This function returns a list with components:
\item{MSPE}{(vector) MSPE estimates for NER model.}
\item{bhat}{(vector) Estimates of the unknown regression coefficients.}
\item{sigvhat2}{(numeric) Estimates of the area-specific variance component.}
\item{sigehat2}{(numeric) Estimates of the random error variance component.}
}

\references{
J. Jiang and M. Torabi. Sumca: simple; unified; monte carlo assisted approach to second order unbiased mean squared prediction error estimation. \emph{Journal of the Royal Statistical Society: Series B (Statistical Methodology)}, 82(2):467-485, 2020.
}

\author{
Peiwen Xiao, Xiaohui Liu, Yu Zhang, Yuzi Liu, Jiming Jiang
}


\examples{
Ni <- 1000
sigmaX <- 1.5
m <- 10
beta <- c(0.5, 1)
sigma_v2 <- 0.8
sigma_e2 <- 1
ni <- sample(seq(1, 10), m, replace = TRUE)
n <- sum(ni)
p <- length(beta)

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

sampleXY <- function(Ni, ni, m, Population) {
  Indx <- c()
  for (i in 1:m) {
    Indx <- c(Indx, sample(c(((i - 1) * Ni + 1):(i * Ni)), ni[i]))
  }
  Sample <- Population[Indx, ]
  return(Sample)
} 

Population <- pop.model(Ni, sigmaX, beta, sigma_v2, sigma_e2, m)$data
XY <- sampleXY(Ni, ni, m, Population)

formula <- y ~ x1
data <- XY

Xmean <- matrix(NA, m, p)
for (tt in 1:m) {
  Xmean[tt, ] <- colMeans(Population[which(Population$group == tt), "x1", drop = FALSE])
}

result <- mspeNERsumca(ni, formula, data, Xmean, K = 50, method = 1)
}

