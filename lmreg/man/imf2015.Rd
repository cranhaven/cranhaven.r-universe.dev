\name{imf2015}
\alias{imf2015}
\docType{data}
\title{
IMF unemployment data
}
\description{
The estimated or reported figures of a number of economic variables for a few countries in the year 2015, extracted from IMF World Economic Outlook (2017)
}
\usage{data(imf2015)}
\format{
  A data frame with 33 observations on the following 8 variables.
  \describe{
    \item{\code{Country}}{Country name, a character vector}
    \item{\code{CAB}}{Current account balance as \% of GDP, a numeric vector}
    \item{\code{DEBT}}{Governmentt gross debt as \% of GDP, a numeric vector}
    \item{\code{EXP}}{Government total expenditure as \% of GDP, a numeric vector}
    \item{\code{GDP}}{GDP per capita, current prices in '000 US$, a numeric vector}
    \item{\code{INFL}}{Inflation, average consumer prices in \%, a numeric vector}
    \item{\code{INV}}{Total investment as \% of GDP, a numeric vector}
    \item{\code{UNMP}}{Unemployment as \% of labor force, a numeric vector}
  }
}

\source{
http://www.imf.org/external/pubs/ft/weo/2017/01/weodata/weoselgr.aspx.
}

\examples{
data(imf2015)
head(imf2015)
}
\keyword{datasets}
