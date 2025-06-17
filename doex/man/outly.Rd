\name{outly}
\alias{outly}
\title{Outlier generation function}
\usage{
outly(ndata,noutlier,meand,vard,dif,alpha,normality.status,skewn.status)
}
\description{
This function generates the outlier(s) by Interquantile range approach.
}
\examples{
library(doex)
outly(8,2,2,0.05,FALSE)
}
\arguments{
  \item{ndata}{sample size of the data without outlier(s).}
  \item{noutlier}{number of outlier(s) in data.}
  \item{meand}{mean of the data.}
  \item{vard}{variance of the data.}
  \item{dif}{distance level of outlier(s) from the whiskers.}
  \item{alpha}{significance level for the normality test.}
  \item{normality.status}{a logical operator controls the normality of data with outlier. "TRUE" for normal and "FALSE" for non-normal}
  \item{skewn.status}{a logical operator controls the skewness of the data with outlier. "0" for symmetric, "1" for right-skewed and "-1" for left-skewed.}
}

\value{
  \item{data}{the vector contains the generated data with outlier(s)}
  \item{outlier}{the vector contains the generated outlier(s)}
  \item{normality.test}{the result of the Shapiro-Wilk normality test for the generated data}
}
\author{Mustafa CAVUS}
\references{
Alexander, R.A., Govern, D.M. (1994) A new and simplier approximation for ANOVA under variance heterogeneity, Journal of Educational Statistics, 19(2), 91-101.
}
