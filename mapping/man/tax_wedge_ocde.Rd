\name{tax_wedge_ocde}
\alias{tax_wedge_ocde}
\docType{data}
\title{OCDE tax wedge}

\description{Tax wedge for OCDE countries}

\usage{data("tax_wedge_ocde")}
\format{
  A data frame with 74 observations on the following 7 variables.
  \describe{
    \item{\code{country_code}}{a factor with country code}
    \item{\code{year}}{a character vector with year}
    \item{\code{family_type}}{a factor with family levels}
    \item{\code{average_rate_employees_SSC}}{a numeric vector with Social Securities Contribution by employees}
    \item{\code{average_rate_employer_SSC}}{a numeric vector with Social Securities Contribution by employers}
    \item{\code{net_personal_average_tax_rate}}{a numeric vector with personal average tax rate}
    \item{\code{average_tax_wedge}}{a numeric vector with average tax wedge}
  }
}


\source{OECD (2020), Tax wedge (indicator). doi: 10.1787/cea9eba3-en (Accessed on 30 November 2020). \url{https://data.oecd.org/tax/tax-wedge.htm}}

\examples{
data(tax_wedge_ocde)
str(tax_wedge_ocde)
}
\keyword{datasets}
