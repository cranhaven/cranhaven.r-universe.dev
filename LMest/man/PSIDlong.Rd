\name{PSIDlong}
\alias{PSIDlong}

\docType{data}

\title{Dataset about income dynamics}

\description{Longitudinal dataset deriving from the Panel Study of Income Dynamics (PSID) from 1987 to 1993.}

\usage{data(PSIDlong)}

\format{
  A data frame with 1446 observations on the following  variables.
  \describe{
  \item{\code{id}}{subject id.}
    \item{time}{occasion of observation.}
    \item{\code{Y1Fertility}}{indicating whether a woman had given birth to a child in a certain year 1 for "yes", 0 for "no".}
       \item{\code{Y2Employment}}{indicating whether she was employed 1 for "yes", 0 for "no".}
    \item{\code{X1Race}}{dummy variable equal to 1 for a "black" woman, 0 for "other".}
        \item{\code{X2Age}}{age in 1986, rescaled by its maximum value.}
        
         \item{\code{X3Age2}}{squared age.}
    
    \item{\code{X4Education}}{number of years of schooling.}
    \item{\code{X5Child1_2}}{number of children in the family aged between 1 and 2 years, referred to the previous year.}
          \item{\code{X6Child3_5}}{number of children in the family aged between 3 and 5 years, referred to the previous year.}
       \item{\code{X7Child6_13}}{number of children in the family aged between 6 and 13 years, referred to the previous year.}
         \item{\code{X8Child14}}{number of children in the family aged over 14 years, referred to the previous year.}
         \item{\code{X9Income}}{income of the husband (in dollars, referred to the previous year, divided by 1,000.}
  
  }
}

\source{\url{https://psidonline.isr.umich.edu}}


\references{This dataset is downloadable through the package `psidR`.}

\examples{
data(PSIDlong)
}

\keyword{datasets}
