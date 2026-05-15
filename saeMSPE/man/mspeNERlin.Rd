\name{mspeNERlin}
\alias{mspeNERlin}
\alias{mspeNERPR}
\alias{mspeNERDL}
\title{
Compute MSPE through linearization method for Nested error regression model
}
\description{
This function returns MSPE estimator with linearization method for Nested error regression model. These include the seminal Prasad-Rao method and its generalizations by Datta-Lahiri. All these methods are developed for general linear mixed effects models
}
\usage{
mspeNERlin(ni, formula, data, X.mean,
			 method = "PR", var.method = "default", na_rm, na_omit)

mspeNERPR(ni, formula, data, X.mean,
			 var.method = "default", na_rm, na_omit)

mspeNERDL(ni, formula, data, X.mean,
			 var.method = "default", na_rm, na_omit)

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
  \item{X.mean}{
(matrix). Stands for the population mean of auxiliary values.
}
  \item{method}{
The MSPE estimation method to be used. See "Details".
}
  \item{var.method}{
The variance component estimation method to be used. See "Details".
}
  \item{na_rm}{
  A logical value indicating whether to remove missing values (NaN) from the input matrices and vectors. 
  If \code{TRUE}, missing values in the input data (\code{X}, \code{Y}, \code{ni}, and \code{X.mean}) are automatically cleaned using internal functions.
  If \code{FALSE}, missing values are not removed. Defaults to \code{FALSE}.
}
  \item{na_omit}{
  A logical value indicating whether to stop the execution if missing values (NaN) are present in the input data. 
  If \code{TRUE}, the function will check for missing values in \code{X}, \code{Y}, \code{ni}, and \code{X.mean}. 
  If any missing values are found, an error message will be raised, prompting the user to handle the missing data before proceeding. 
  Defaults to \code{FALSE}.
}
}
\details{
Default \code{method} for \code{mspeNERlin} is "PR", proposed by N. G. N. Prasad and J. N. K. Rao, Prasad-Rao (PR) method uses Taylor series expansion to obtain a second-order approximation to the MSPE. Function \code{mspeNERlin} also provide the following method:

Method "DL" advanced PR method to cover the cases when the variance components are estimated by ML and REML estimator. Set \code{method = "DL"}.

For \code{method = "PR"}, \code{var.method = "MOM"} is the only available variance component estimation method,

For \code{method = "DL"}, \code{var.method = "ML"} or \code{var.method = "REML"} are available.

}

\value{
This function returns a list with components:
\item{MSPE}{(vector) MSPE estimates for NER model.}
\item{bhat}{(vector) Estimates of the unknown regression coefficients.}
\item{sigvhat2}{(numeric) Estimates of the area-specific variance component.}
\item{sigehat2}{(numeric) Estimates of the random error variance component.}
}

\references{
N. G. N. Prasad and J. N. K. Rao. The estimation of the mean squared error of small-area estimators. \emph{Journal of the American Statistical Association}, 85(409):163-171, 1990.

G. S. Datta and P. Lahiri. A unified measure of uncertainty of estimated best linear unbiased predictors in small area estimation problems. \emph{Statistica Sinica}, 10(2):613-627, 2000.
}
\author{
Peiwen Xiao, Xiaohui Liu, Yu Zhang, Yuzi Liu, Jiming Jiang
}


\examples{
### parameter setting 
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

result <- mspeNERlin(ni, formula, data, Xmean, method = "PR", var.method = "default")
}

