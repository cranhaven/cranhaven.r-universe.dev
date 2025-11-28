\name{longley2}
\alias{longley2}
\docType{data}
\title{Art Hoerl's update of the infamous Longley(1967) benchmark dataset}
\description{Data from the "Employment and Training Report of the President, 1976"
 compiled by Art Hoerl, University of Delaware.  This data.frame contains both [1] some
 different ("corrected") numerical values from those used by Longley(1967) and [2] the added
 years of 1963-1975. Longley(1967) used only data from the 16 years of 1947 through 1962.}
\usage{data(longley2)}
\format{
  A data.frame of 7 variables covering 29 consecutive years; no NAs.
  \describe{
    \item{GNP.deflator}{GNP price deflation index.}
    \item{Unemployed}{Unemployment percentage.}
    \item{Armed.Forces}{Size of the Armed Forces.}
    \item{Population}{Total U.S. Population.}
    \item{Year}{1947 to 1975.}
    \item{Employed}{employment.}
    \item{GNP}{Gross National Product.}
  }
}
\references{
 Longley JW. An appraisal of least-squares programs from the point of view of the user.
 J. Amer. Statist. Assoc. 1967; 62: 819-841.
 
 Hoerl AE, Kennard RW. Ridge Regression - Present and Future. 12th Symposium on the
 Interface. 1979. Waterloo, Ontario, Canada.
}
\keyword{datasets}