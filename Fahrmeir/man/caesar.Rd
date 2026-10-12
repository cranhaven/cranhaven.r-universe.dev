\name{caesar}
\alias{caesar}
\docType{data}
\title{ Caesarian Birth Study   }
\description{
  Data on infection from births by Caesarian section
}
\usage{data(caesar)}
\format{
  A data frame with 24 observations on the following 7 variables.
  \describe{
    \item{y}{a factor with levels \code{1} \code{2} \code{3}, the response}
    \item{w}{number of patients in group}
    \item{noplan}{a factor with levels \code{not} \code{planned}, 
                    was the caesarian planned?}
    \item{factor}{a factor with levels \code{risk factors} \code{without}, 
                was there risk factors?}
    \item{antib}{a factor with levels \code{antibiotics} \code{without}}
    \item{yl}{logistic response, 0=no infection}
    \item{patco}{covariate pattern number}
  }
}
\details{
  Infection from birth by Caesarian section. The response variable, 
   \code{y}, has levels 1=type I infection, 2=type II infection, 
   3=none infection. Where risk-factors (diabetes, overweight, others)
   present? Where antibiotics used as prophylaxis? Aim is to 
   analyse effects on response by covariates.
}
\source{
   Ludwig Fahrmeir, Gerhard Tutz (1994):  Multivariate Statistical Modelling
   Based on Generalized Linear Models. Springer Series in Statistics. 
   Springer Verlag. New-York  Berlin Heidelberg
}
\author{Kjetil Halvorsen}
\examples{
summary(caesar)
caesar.glm1 <- glm(yl ~ noplan+factor+antib, data=caesar, weight=w, 
                       family=binomial(link="logit"))
caesar.glm2 <- glm(yl ~ noplan+factor+antib, data=caesar, weight=w, 
                   family=binomial(link="probit"))
summary(caesar.glm1)
summary(caesar.glm2)
}
\keyword{datasets}
