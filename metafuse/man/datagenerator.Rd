\name{datagenerator}
\alias{datagenerator}
\title{simulate data}
\usage{
datagenerator(n, beta0, family, seed = NA)
}
\arguments{
\item{n}{a vector of length \code{K} (the total number of datasets being integrated), specifying the sample sizes of individual datasets; can also be an scalar, in which case the function simulates \code{K} datasets of equal sample size}

\item{beta0}{a coefficient matrix of dimension \code{K * p}, where \code{K} is the number of datasets being integrated and \code{p} is the number of covariates, including the intercept}

\item{family}{the type of the response vector, \code{c("gaussian", "binomial", "poisson", "cox")}; \code{"gaussian"} for continuous response, \code{"binomial"} for binary response, \code{"poisson"} for count response, \code{"cox"} for observed time-to-event response, with censoring indicator}

\item{seed}{the random seed for data generation, default is \code{NA}}
}
\value{
Returns data frame with \code{n*K} rows (if \code{n} is a scalar), or \code{sum(n)} rows (if \code{n} is a \code{K}-element vector). The data frame contains columns "y", "x1", \dots, "x_{p-1}" and "group" if \code{family="gaussian"}, \code{"binomial"} or \code{"poisson"}; or contains columns "time", "status", "x1", \dots, "x_{p-1}" and "group" if \code{family="cox"}.
}
\description{
Simulate a dataset with data from \code{K} different sources, for demonstration of \code{metafuse}.
}
\details{
These datasets are artifical, and are used to demonstrate the features of \code{metafuse}. In the case when \code{family="cox"}, the response will contain two vectors, a time-to-event variable \code{time} and a censoring indicator \code{status}.
}

\keyword{data}
\keyword{generator}

\examples{
########### generate data ###########
n <- 200    # sample size in each dataset (can also be a K-element vector)
K <- 10     # number of datasets for data integration
p <- 3      # number of covariates in X (including the intercept)

# the coefficient matrix of dimension K * p, used to specify the heterogeneous pattern
beta0 <- matrix(c(0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,   # beta_0 of intercept
                  0.0,0.0,0.0,0.0,0.0,1.0,1.0,1.0,1.0,1.0,   # beta_1 of X_1
                  0.0,0.0,0.0,0.0,0.5,0.5,0.5,1.0,1.0,1.0),  # beta_2 of X_2
                K, p)

# generate a data set, family=c("gaussian", "binomial", "poisson", "cox")
data <- datagenerator(n=n, beta0=beta0, family="gaussian", seed=123)
names(data)

# if family="cox", returned dataset contains columns "time"" and "status" instead of "y"
data <- datagenerator(n=n, beta0=beta0, family="cox", seed=123)
names(data)
}

