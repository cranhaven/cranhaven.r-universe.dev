\name{cells}
\alias{cells}
\docType{data}
\concept{cells}
\concept{poisson}
\concept{negative binomial}
\title{ Cellular Differentiation   }
\description{
  The effect of two agents of immuno-activating ability that may induce cell
   differentiation was investigated. 
}
\usage{data(cells)}
\format{
  A data frame with 16 observations on the following 3 variables.
  \describe{
    \item{y}{number of cells differentiating}
    \item{TNF}{dose of TNF, U/ml}
    \item{IFN}{dose of IFN, U/ml}
  }
}
\details{
  The effect of two agents of immuno-activating ability that may induce cell
   differentiation was investigated.  As response variable the number of
   cells that exhibited markers after exposure was recorded. It is of interest
   if the agents TNF (tumor necrosis factor) and IFN (interferon)
   stimulate cell differentiation independently, or if there is a synergetic
   effect.  200 cells were examined at each dose combination.
}
\source{
     Ludwig Fahrmeir, Gerhard Tutz (1994):  Multivariate Statistical Modelling
   Based on Generalized Linear Models. Springer Series in Statistics. 
   Springer Verlag. New-York  Berlin Heidelberg
}
\author{Kjetil Halvorsen}
\examples{
str(cells)
cells.poisson <- glm(y~TNF+IFN+TNF:IFN, data=cells, 
                    family=poisson)
summary(cells.poisson)
confint(cells.poisson)
# Now we follow the book, example 2.6, page 51:
# there seems to be overdispersion?
cells.quasi <- glm(y~TNF+IFN+TNF:IFN, data=cells, 
                    family=quasipoisson)
summary(cells.quasi)
anova(cells.quasi)
confint(cells.quasi)
# We follow the book, example 2.7, page 56:
with(cells, tapply(y, factor(TNF), function(x) c(mean(x), var(x))))
# which might indicate the use of a negative binomial model
}
\keyword{datasets}
