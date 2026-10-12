\name{breath}
\alias{breath}
\docType{data}
\concept{ordered categorical response}
\concept{continuation ratio}
\title{ Breathing Test  }
\description{
   Effects of age and smoking status on
   breathing test results for  workers in industrial plants in 
   Texas.  
}
\usage{data(breath)}
\format{
  A data frame with 18 observations on the following 4 variables.
  \describe{
    \item{Age}{a factor with levels \code{<40} \code{40-59}}
    \item{n}{number of workers in group}
    \item{Smoking.status}{a factor with levels \code{Current.smoker} 
              \code{Former.smoker} \code{Never.smoked}}
    \item{Breathing.test}{a factor with levels \code{Abnormal} 
              \code{Borderline} \code{Normal}}
  }
}
\details{
  We consider the effects of age and smoking status upon breathing
  test results for workers in industrial plants in Texas. 
  The test results are given on an ordered scale with categories
  "Abnormal", "Borderline" and "Normal". It is of interest how age
  and smoking status are connected to breathing test results. 
}
\source{
    Ludwig Fahrmeir, Gerhard Tutz (1994):  Multivariate Statistical Modelling
   Based on Generalized Linear Models. Springer Series in Statistics. 
   Springer Verlag. New-York  Berlin Heidelberg
}
\examples{
str(breath)
breath$Breathing.test <- ordered(breath$Breathing.test)
library(MASS)
breath.polr1 <- polr(Breathing.test ~ Age*Smoking.status, weight=n, 
                     data=breath)
breath.polr2 <- polr(Breathing.test ~ Age*Smoking.status, weight=n, 
                     data=breath, method="cloglog")
summary(breath.polr1)
summary(breath.polr2)
# continuation ratio models (as of page 89) might be fitted with
# Design or VGAM package.
}
\keyword{datasets}
