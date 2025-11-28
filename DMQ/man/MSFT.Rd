\name{MSFT}
\docType{data}
\alias{MSFT}
\alias{vY}
\title{data: Microsoft Corporation logarithmic percentage returns from December 8, 2010 to November 15, 2018 for a total of T = 2000 observation downloaded from Yahoo finance.}
\description{
This dataset is the one used in Catania and Luati (2023).\cr
%

Dow Jones 30 Constituents closing value log returns from 1987-03-16 to
2009-02-03 from Yahoo Finance. Note that AIG was replaced by KFT (Kraft Foods)
on September 22, 2008. This is not reflected in this data set as that would
bring the starting date of the data to 2001.
}
\usage{data("MSFT")}
\format{An \code{xts} object of 2000 logarithmic percentage returns.}
\source{Yahoo Finance}
\references{
Catania, L, and Luati, A. (2023). 
"Semiparametric modeling of multiple quantiles."
Journal of Econometrics
\doi{10.1016/j.jeconom.2022.11.002}.
}
\keyword{datasets}
