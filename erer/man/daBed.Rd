\name{daBed}
\alias{daBed}
\docType{data}
\title{Transformed Wooden Beds Import Data for Static AIDS Models}
\description{This data set contains transformed values related to wooden beds imports by the United States from January 2001 to December 2008. There are 96 observations and 20 variables.

\tabular{ll}{
\code{sCN} \tab monthly import share of wooden beds from China \cr
\code{sVN} \tab monthly import share of wooden beds from Vietnam \cr
\code{sID} \tab monthly import share of wooden beds from Indonesia \cr
\code{sMY} \tab monthly import share of wooden beds from Malaysia \cr
\code{sCA} \tab monthly import share of wooden beds from Canada \cr
\code{sBR} \tab monthly import share of wooden beds from Brazil \cr
\code{sIT} \tab monthly import share of wooden beds from Italy \cr
\code{sRW} \tab monthly import share of wooden beds from the rest of world \cr
\code{rte} \tab real total expenditure in logarithm \cr
\code{lnpCN} \tab monthly import price of wooden beds from China in logarithm \cr
\code{inpVN} \tab monthly import price of wooden beds from Vietnam in logarithm \cr
\code{lnpID} \tab monthly import price of wooden beds from Indonesia in logarithm \cr
\code{lnpMY} \tab monthly import price of wooden beds from Malaysia in logarithm \cr
\code{lnpCA} \tab monthly import price of wooden beds from Canada in logarithm \cr
\code{inpBR} \tab monthly import price of wooden beds from Brazil in logarithm \cr
\code{lnpIT} \tab monthly import price of wooden beds from Italy in logarithm \cr
\code{lnpRW} \tab monthly import price of wooden beds from the rest of world in logarithm \cr
\code{dum1} \tab  a pulse dummy variable (1 for October 2003, 0 otherwise) \cr
\code{dum2} \tab  a pulse dummy variable (1 for July 2004, 0 otherwise) \cr
\code{dum3} \tab  a pulse dummy variable (1 for January 2005, 0 otherwise) \cr
}
}
\usage{data(daBed)}

\format{Monthly time series from Janurary 2001 to December 2008 with 96 observations for each of the 20 variables.}

\details{This is the transformated data set for static AIDS model. The transformation detail is described in Wan et al. (2010).}

\source{U.S. ITC, 2010. Interactive tariff and trade data web. US International Trade Commission (Assecced on March 1, 2010).}

\references{Wan, Y., C. Sun, and D.L. Grebner. 2010. Analysis of import demand for wooden beds in the United States. Journal of Agricultural and Applied Economics 42(4):643-658.}

\seealso{\code{\link{aiStaFit}}; \code{\link{daBedRaw}}.}

\examples{
data(daBed)
class(daBed); dim(daBed); colnames(daBed)
daBed
}
\keyword{datasets}