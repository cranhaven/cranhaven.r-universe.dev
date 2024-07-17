\name{daExp}
\alias{daExp}
\docType{data}
\title{Expenditure Data for a Hausman Test in AIDS Model}
\description{This data set contains seven monthly times series for expenditure from 2001 to 2008.
\tabular{ll}{
\code{pinc } \tab Billions of dollars, personal income \cr
\code{dpi	 } \tab Billions of dollars, disposable personal income  \cr
\code{pce	 } \tab Billions of dollars, personal consumption expenditures  \cr
\code{dg	 } \tab Billions of dollars, Personal consumption expenditures for durable goods  \cr
\code{rdpi } \tab Billions of dollars, real disposable personal income  \cr
\code{rpce } \tab Billions of dollars, real personal consumption expenditures  \cr
\code{rdg	 } \tab Billions of dollars, real personal consumption expenditures for durable goods  \cr
}
}

\usage{data(daExp)}

\format{Monthly time series from Janurary 2001 to December 2008 with 96 observations for each of the seven variables.}

\details{This is the data set for conducting a Hausman test in a static AIDS model, as detailed in Wan et al. (2010). The test focuses on whether the expenditure variable in a AIDS model is exogenous or not. Each of the seven expenditure data can be used as an instrumental variable in an auxiliary regression.}

\source{Federal Reserve Bank of St. Louis. Economic Data - Fred. Internet site: http://stlouisfed.org (Accessed February 25, 2010).}

\references{Wan, Y., C. Sun, and D.L. Grebner. 2010. Analysis of import demand for wooden beds in the United States. Journal of Agricultural and Applied Economics 42(4):643-658.}

\examples{
data(daExp)
class(daExp); dim(daExp); colnames(daExp)
daExp
}
\keyword{datasets}