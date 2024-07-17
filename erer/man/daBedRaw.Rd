\name{daBedRaw}
\alias{daBedRaw}
\docType{data}
\title{Wooden Beds Import Data}
\description{This data set contains a mulitiple time series related to wooden beds imports by the United States. The time covered is January 1996 to December 2008 with 156 observations. There are 34 variables in total: 17 import values (dollars) and 17 import quantities (dollars / piece). In total, 16 countries are covered and the world total is also reported.

\tabular{ll}{
\code{vBR	} \tab cost-insurance-freight import values in dollar from Brazil \cr
\code{vCA	} \tab cost-insurance-freight import values in dollar from Canada \cr
\code{vCN	} \tab cost-insurance-freight import values in dollar from China \cr
\code{vDK	} \tab cost-insurance-freight import values in dollar from Denmark \cr
\code{vFR	} \tab cost-insurance-freight import values in dollar from France \cr
\code{vHK	} \tab cost-insurance-freight import values in dollar from Hong Kong \cr
\code{vIA	} \tab cost-insurance-freight import values in dollar from India \cr
\code{vID	} \tab cost-insurance-freight import values in dollar from Indonesia \cr
\code{vIT	} \tab cost-insurance-freight import values in dollar from Italy \cr
\code{vMY	} \tab cost-insurance-freight import values in dollar from Malaysia \cr
\code{vMX	} \tab cost-insurance-freight import values in dollar from Mexico \cr
\code{vPH	} \tab cost-insurance-freight import values in dollar from Philippines \cr
\code{vTW	} \tab cost-insurance-freight import values in dollar from Taiwan \cr
\code{vTH	} \tab cost-insurance-freight import values in dollar from Thailand \cr
\code{vUK	} \tab cost-insurance-freight import values in dollar from United Kingdom \cr
\code{vVN	} \tab cost-insurance-freight import values in dollar from Vietnam \cr
\code{vWD	} \tab cost-insurance-freight import values in dollar from World in total \cr
\code{qBR	} \tab quantity in piece from Brazil \cr
\code{qCA	} \tab quantity in piece from Canada \cr
\code{qCN	} \tab quantity in piece from China \cr
\code{qDK	} \tab quantity in piece from Denmark \cr
\code{qFR	} \tab quantity in piece from France \cr
\code{qHK	} \tab quantity in piece from Hong Kong \cr
\code{qIA	} \tab quantity in piece from India \cr
\code{qID	} \tab quantity in piece from Indonesia \cr
\code{qIT	} \tab quantity in piece from Italy \cr
\code{qMY	} \tab quantity in piece from Malaysia \cr
\code{qMX	} \tab quantity in piece from Mexico \cr
\code{qPH	} \tab quantity in piece from Philippines \cr
\code{qTW	} \tab quantity in piece from Taiwan  \cr
\code{qTH	} \tab quantity in piece from Thailand  \cr
\code{qUK	} \tab quantity in piece from United Kingdom \cr
\code{qVN	} \tab quantity in piece from Vietnam \cr
\code{qWD	} \tab quantity in piece from World in total \cr
}
}

\usage{data(daBedRaw)}

\format{Monthly time series from Janurary 1996 to December 2008 with 156 observations for each of the 34 variables.}

\details{Under the Harmonized Tariff Schedule (HTS) system, the commodity of wooden beds is classified as HTS 9403.50.9040. The monthly cost-insurance-freight values in dollar and quantities in piece are reported by country from U.S. ITC (2010).}

\source{U.S. ITC, 2010. Interactive tariff and trade data web. US International Trade Commission (Assecced on March 1, 2010).}

\references{Wan, Y., C. Sun, and D.L. Grebner. 2010. Analysis of import demand for wooden beds in the United States. Journal of Agricultural and Applied Economics 42(4):643-658.}

\seealso{\code{\link{aiStaFit}}; \code{\link{daBed}}.}

\examples{
data(daBedRaw)
class(daBedRaw); dim(daBedRaw); colnames(daBedRaw)
}
\keyword{datasets}