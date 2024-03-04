\name{fit_dist_test}
\alias{fit_dist_test}

\title{
Summary of Godness-of-fit tests
}
\description{
Returns a data frame with the summary of Fiting distribution tests for the following distributions: "normal","log-normal","geometric","exponential","Poisson", "cauchy" , "logistic" and "weibull".

The considered Godness-of-fit tests are: Bayesian Information Criterium (BIC), Akaike Information Criterium (AIC), Kolmogorov-Smirnov test and Anderson-Darling test.

}
\usage{
fit_dist_test(x)
}

\arguments{
  \item{x}{
 A numeric vector of length at least one containing only finite values
}
}

\value{
\item{Distribution }{Name of the tested distribution}
\item{BayesianIC }{Bayesian Information Criterium (BIC)}
\item{AkaikeIC }{Akaike Information Criterium (AIC)}
\item{Kol-SmirD }{The value of the Kolmogorov-Smirnov test statistic}
\item{Kol-SmirPvalue }{The value of the Kolmogorov-Smirnov test p-value}
\item{Signigicance KS }{A column to check the significance of the Kolmogorov-Smirnov test}
\item{And-Darl }{The value of the nderson-Darling test statistic}
\item{And-DarlPvalue }{The value of the Anderson-Darling test p-value}
\item{Signigicance AD }{A column to check the significance of the Anderson-Darling test}

}

\author{
F. Barrio-Parra
}



\seealso{
ad.test library(kSamples), 
AIC library(stats), 
BIC library(stats), 
ks.test library(stats), 
}
\examples{
set.seed(123)

a <- rnorm(n=100, mean =10, sd = 1) 

fit_dist_test(a)

b<- rexp(n = 100,rate = 1)

fit_dist_test(b)
}
\keyword{ methods }

