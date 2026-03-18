\name{Fat}
\alias{Fat}
\docType{data}
\title{ Human body fat and several covariates for calculating it. }
\description{
  Data from Johnson (1996) on human body fat: determined by under-water
  weight and several covariates to estimate it statistically.
}
\usage{data(Fat)}
\format{
  A data frame with 252 observations on the following 19 variables:
  \describe{
    \item{\code{Case}}{case number}
    \item{\code{PBF.B}}{percentage body fat estimated using Brozek's equation}
    \item{\code{PBF.S}}{percentage body fat estimated using Siri's
      equation}
    \item{\code{Dens}}{Density (gm/cm^3)}
    \item{\code{Age}}{Age (yrs)}
    \item{\code{Weight}}{Weight (lbs)}
    \item{\code{Height}}{Height (inches)}
    \item{\code{AI}}{Adiposity index = Weight/Height^2 (kg/m^2)}
    \item{\code{FFWt}}{Fat Free Weight using Brozek's formula (lbs)}
    \item{\code{Neck}}{Neck circumference (cm)}
    \item{\code{Chest}}{Chest circumference (cm)}
    \item{\code{Abd}}{Abdomen circumference (cm)}
    \item{\code{Hip}}{Hip circumference (cm)}
    \item{\code{Thigh}}{Thigh circumference (cm)}
    \item{\code{Knee}}{Knee circumference (cm)}
    \item{\code{Ankle}}{Ankle circumference (cm)}
    \item{\code{Biceps}}{Extended biceps circumference (cm)}
    \item{\code{FArm}}{Forearm circumference (cm)}
    \item{\code{Wrist}}{Wrist circumference (cm)}
  }
}
\source{
  Johnson, R. 1996. Fitting percentage of body fat to simple body
  measurements. Journal of Statistics Education 2(1), 1--6.
  
  van Hulst, R. 2018. Evaluating Scientific Evidence. ms.
}
\examples{
data(Fat)
qqnorm(Fat$Height)
qqline(Fat$Height)
}
\keyword{datasets}
