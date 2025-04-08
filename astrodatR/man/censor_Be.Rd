\name{censor_Be}
\docType{data}
\alias{censor_Be}

\title{Stellar abundances and planets}
\description{
This data set allows investigation of whether the presence or absence of a giant planet is correlated with the level of lithium and beryllium in the photosphere of the host star.  The dataset consists of 3 measured variables for 68 solar-type stars, roughly half with and half without detected planets (Santos et al. 2002).  Some missing data are present.   This dataset is useful for survival analysis involving correlation of singly- and doubly-censored data.  Note that astronomical data are typically left-censored.   Further description and references for the dataset are given in  Appendix C.4 of Feigelson & Babu (2012).   
}
\usage{censor_Be}
\format{A table containing 68 rows and 8 columns with header row. Column 2 is a sample indicator (1 = star with planet; 2 = star without planet).  Columns 4 and 7 are censoring indicators (1 = detected; 0 = undetected, left-censored).}
\source{Feigelson and Babu (2012), Santos et al. (2002)}
\references{
Feigelson, E. D. and Babu, G. J. (2012) \emph{Modern Statistical Methods for Astronomy with R Applications}, Cambridge UK:Cambridge University Press

Santos, N. C., Garcia Lopez, R. J., Israelian, G., Mayor, M., Rebolo, R., Garcia-Gil, A., Perrez de Taoro, M. R. & Randich, S. (2002) Beryllium abundances in stars hosting giant planets, \emph{Astropnomy & Astrophysics}, 386, 1028-1-38
}
\keyword{datasets}
