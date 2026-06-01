\name{bladder}
\alias{bladder}
\docType{data}
\title{Bladder Cancer Data}
\description{
Therneau and Lumley (2015) reported data on recurrences of bladder
cancer, which were used to demonstrate methodology for recurrent event
modelling (Wei et al., 1989). This is the data
set from 85 subjects who were assigned to either thiotepa or placebo,
and reports the first four recurrences for any patient. The stop variable
is the tumor recurrence or censoring time in month. The event variable
is 1 for recurrence and 0 for everything else (including death for any
reason).
}
\usage{data("bladder")}
\format{
  A data frame with 340 observations on the following 7 variables.
  \describe{
    \item{\code{id}}{patients identifier for 85 patients}
    \item{\code{rx}}{treatment; 1 (=placebo) and 2 (=thiotepa)}
    \item{\code{number}}{initial number of tumours (8=8 or more)}
    \item{\code{size}}{size (cm) of largest initial tumour}
    \item{\code{stop}}{recurrence or censoring time}
    \item{\code{event}}{event variable with 1 for recurrence and 0 for everythis else (including death for any reason)}
    \item{\code{enum}}{which recurrence (up to 4)}
  }
}
\references{
Therneau, T. and Lumley, T. (2015). survival: Survival Analysis. R
package version 2.38-3.

Wei, L.J., Lin, D.Y., and Weissfeld, L. (1989). Regression analysis
of multivariate incomplete failure time data by modeling marginal
distributions. Journal of the Americal Statistical Association, 84,
1065--1073.
}

