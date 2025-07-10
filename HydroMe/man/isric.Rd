\name{isric}
\alias{isric}
\docType{data}
\title{Water Retention World Dataset from ISRIC}
\description{
This is part of world dataset of measured water retention characteristics. It contains soil moisture contents (y) at eight (8) levels of suction pressure heads (x).
}
\usage{data(isric)}
\format{
  A data frame with 320 observations on the following 6 variables.
  \describe{
    \item{\code{Sample}}{which is a factor with levels such as: \code{Benin1} \code{Benin2} \code{Benin3} \code{Italy4} \code{Italy5} \code{Italy6} \code{Italy7} \code{Nicaragua21} \code{Nicaragua22} \code{Nicaragua23} \code{Nicaragua24} \code{Nicaragua25} \code{Nicaragua26} \code{Nicaragua27} \code{Nicaragua28} \code{Nicaragua29} \code{Nicaragua30} \code{Nicaragua31} \code{Nicaragua32} \code{Nicaragua33} \code{Nicaragua34} \code{Nicaragua35} \code{Nicaragua36} \code{Nicaragua37} \code{Nicaragua38} \code{Nicaragua39} \code{Nicaragua40} \code{Poland10} \code{Poland11} \code{Poland8} \code{Poland9} \code{Rwanda16} \code{Rwanda17} \code{Rwanda18} \code{Rwanda19} \code{Rwanda20} \code{Togo12} \code{Togo13} \code{Togo14} \code{Togo15}}
    \item{\code{Country}}{which is a factor with levels such as: \code{Benin} \code{Italy} \code{Nicaragua} \code{Poland} \code{Rwanda} \code{Togo}}
    \item{\code{BD}}{which is a numeric vector for bulk density}
    \item{\code{x}}{which is a numeric vector for suction potential}
    \item{\code{lnx}}{which is a numeric vector for log(e)}
    \item{\code{y}}{which is a numeric vector of moisture contents}
  }
}
\details{
The data is grouped according to samples from each country from where the data were collected. In addition, it also contains surface bulk density (BD) for each sample. The whole dataset can be obtained from www.isric.org
}
\source{
  \url{https://www.isric.org/}
}
\examples{
data(isric)
str(isric)
}
\keyword{datasets}
