\name{wine}
\alias{wine}
\docType{data}
%# Example 7.2 page 228.
\title{ Bitterness of White Wines  }
\description{
   In a study on the bitterness of white wine it is of interest wheater treatments
   that can be controlleds during pressing the grapes influence the bitterness
   of wines.  The two factors considered are the temperature and the admission of
   contact with skin when pressing the grapes. 
}
\usage{data(wine)}
\format{
  A data frame with 72 observations on the following 5 variables.
  \describe{
    \item{temp}{a factor, temperature,  with levels \code{high} \code{low}}
    \item{contact}{a factor with levels \code{no} \code{yes}}
    \item{bottle}{a factor with levels \code{1} \code{2} \code{3} \code{4} 
                    \code{5} \code{6} \code{7} \code{8}}
    \item{judge}{a factor with levels \code{1} \code{2} \code{3} \code{4} 
                  \code{5} \code{6} \code{7} \code{8} \code{9}}
    \item{score}{numeric, ordinal score, from '1'=nonbitter to
                          '5'=very bitter }
  }
}
\source{
     Ludwig Fahrmeir, Gerhard Tutz (1994):  Multivariate Statistical Modelling
   Based on Generalized Linear Models. Springer Series in Statistics. 
   Springer Verlag. New-York  Berlin Heidelberg
}
\examples{
str(wine) 
summary(wine)
}
\keyword{datasets}
