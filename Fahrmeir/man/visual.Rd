\name{visual}
\alias{visual}
\docType{data}
\concept{bivariate logistic}
%# Example 3.13 page 116.
\title{ Visual Impairment Data   }
\description{
   For 5199 individuals bivariate binary responses were observed, indicating 
   wheater or not an eye was visually impaired, with covariates. The main 
   objective is to analyze the influence of age and race on visual impairment, 
   controlling for education, a surrogate for socioeconomic status. Data are only 
   given individually for right and left eye, the bivariate response is lost. 
}
\usage{data(visual)}
\format{
  The format is:
List of 2
 $ left :`data.frame':	16 obs. of  4 variables:
  ..$ left: Factor w/ 2 levels "no","yes": 2 1 2 1 2 1 2 1 2 1 ...
  ..$ race: Factor w/ 2 levels "black","white": 2 2 2 2 2 2 2 2 1 1 ...
  ..$ age : Factor w/ 4 levels "40-50","51-60",..: 1 1 2 2 3 3 4 4 1 1 ...
  ..$ n   : int [1:16] 15 617 24 557 42 789 139 673 29 750 ...
 $ right:`data.frame':	16 obs. of  4 variables:
  ..$ right: Factor w/ 2 levels "no","yes": 2 1 2 1 2 1 2 1 2 1 ...
  ..$ race : Factor w/ 2 levels "black","white": 2 2 2 2 2 2 2 2 1 1 ...
  ..$ age  : Factor w/ 4 levels "40-50","51-60",..: 1 1 2 2 3 3 4 4 1 1 ...
  ..$ n    : int [1:16] 19 613 25 556 48 783 146 666 31 748 ...
}
\source{
     Ludwig Fahrmeir, Gerhard Tutz (1994):  Multivariate Statistical Modelling
   Based on Generalized Linear Models. Springer Series in Statistics. 
   Springer Verlag. New-York  Berlin Heidelberg
}
\examples{
str(visual) 
summary(visual)
}
\keyword{datasets}
