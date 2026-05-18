\name{c401k}
\alias{c401k}
\docType{data}
\title{c401k}
\description{
Cross-sectional data with 9,275 observations including 11 variables on eligibility for and participation in 401(k) along with income and demographic information.
}


\usage{data(c401k)}
\format{
  \describe{
    \item{\code{pira}}{participation in IRA, participation = 1}
    \item{\code{nettfa}}{net family financial assets in $1000}
    \item{\code{p401k}}{participation in 401(k), participation = 1}
    \item{\code{e401k}}{eligibility for 401(k), eligible = 1}
    \item{\code{inc}}{income}
    \item{\code{incsq}}{income square}
    \item{\code{marr}}{marital status, married = 1}
    \item{\code{male}}{sex, male = 1}
    \item{\code{age}}{age}
    \item{\code{agesq}}{age square}
    \item{\code{fsize}}{family size}
  }
}
\details{An exemplary data to illustrate the usage of \code{\link{larf}}. The data includes both a binary outcome (pira) and a continuous outcome (nettfa). The treatment is participation in 401k, p401k. Eligibility for 401(k), e401k, is used as an instrument for p401k.}
\source{The Wooldridge Data Sets (Wooldridge 2010), originally entitled "401ksubs.dta" in Stata format, available at \url{http://www.stata.com/texts/eacsap/}.
}
\references{
Wooldridge, Jeffrey M. 2010. \emph{Econometric Analysis of Cross Section and Panel Data}. 2nd Edition. MIT Press.
}
\seealso{
\code{\link{larf}}, \code{\link{larf.fit}}
}
\examples{
data(c401k)
}
