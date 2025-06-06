\name{india}
\Rdversion{1.1}
\alias{india}
\docType{data}
\title{
Malnutrition of Childen in India
}
\description{
Data sample from a 'Demographic and Health Survey' about malnutrition of children in india.
Data set only contains 1/10 of the observations and some basic variables to enable first analyses.
}
\usage{data(india)}
\format{
  A data frame with 4000 observations on the following 6 variables.
  \describe{
    \item{\code{stunting}}{A numeric malnutrition score with range (-600;600).}
    \item{\code{cbmi}}{BMI of the child.}
    \item{\code{cage}}{Age of the child in months.}
    \item{\code{mbmi}}{BMI of the mother.}
    \item{\code{mage}}{Age of the mother in years.}
    \item{\code{distH}}{The distict in India, where the child lives. Encoded in the region naming
                        of the map \code{\link{india.bnd}}.}
  }
}

\source{
\url{https://dhsprogram.com}
}
\references{
Fenske N and Kneib T and Hothorn T (2009)
\emph{ Identifying Risk Factors for Severe Childhood Malnutrition
by Boosting Additive Quantile Regression}
Technical Report 052, University of Munich
}
\examples{
data(india)

expreg <- expectreg.ls(stunting ~ rb(cbmi),smooth="fixed",data=india,
lambda=30,estimate="restricted",expectiles=c(0.01,0.05,0.2,0.8,0.95,0.99))
plot(expreg)
}
\keyword{datasets}
