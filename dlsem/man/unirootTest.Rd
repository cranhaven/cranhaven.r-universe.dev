\name{unirootTest}
\alias{unirootTest}
\title{Unit root test}
\description{Unit root test is performed on a set of quantitative variables. A single group factor may be taken into account.}
\usage{unirootTest(x = NULL, group = NULL, time = NULL, data, test = NULL, log = FALSE)}
\arguments{
  \item{x}{A vector including the name of the quantitative variables to be tested.
  If \code{NULL} (the default), all the quantitative variables contained in \code{data} will be tested.}
  \item{group}{The name of the group factor (optional). If \code{NULL}, no groups are considered.}
  \item{time}{The name of the time factor (optional).  This variable must be either a numeric identificative or a date in format '\%Y/\%m/\%d','\%d/\%m/\%Y', or '\%Y-\%m-\%d'. If \code{time} is \code{NULL} and \code{group} is not \code{NULL}, data are assumed to be temporally ordered within each group.
  If both \code{time} and \code{group} are \code{NULL}, data are assumed to be temporally ordered.}
  \item{data}{An object of class \code{data.frame} containing the variables to be tested,
  the group factor if \code{group} is not \code{NULL}, and the time factor if \code{time} is not \code{NULL}.}
  \item{test}{The test to apply, that may be either \code{"kpss"} (Kwiatkowski, 1992) or \code{"adf"} (Dickey \& Fuller, 1981).
  If \code{NULL} (the default), the choice is for the KPSS test if the number of periods is less than 100, otherwise the ADF test is used.}
  \item{log}{Logical. If \code{TRUE}, logarithmic transformation is applied to all strictly positive quantitative variables. Default is \code{FALSE}.}
  }
\value{An object of class \code{unirootTest}, consisting of a list with one component for each variable tested. Each list contains the following components:
  \item{statistic}{The value of the test statistic.}
  \item{lag.order}{The lag order at which the test statistic is computed. It is automatically selected according to the precedure by Ng \& Perron (2001).}
  \item{n}{The total number of observations if \code{group} is \code{NULL}, otherwise the number of observations per group.}
  \item{z.value}{The z-value of the test.}
  \item{p.value}{The p-value of the test.}
}
\note{The null hypothesis of the ADF test is the presence of a unit root.
The lag order to calculate the statistic of the ADF test is automatically selected according to the precedure by Ng \& Perron (2001).

The null hypothesis of the KPSS test is stationarity.
The statistic of the KPSS test is calculated at the lag order 4*(n/100)^0.25.

If the group factor is specified, p-values of each group are combined
using the method proposed by Demetrescu (2006).
}
\references{
M. Demetrescu, U. Hassler, and A. Tarcolea (2006). Combining Significance of Correlated Statistics with Application to Panel Data. \emph{Oxford Bulletin of Economics and Statistics}, 68(5), 647-663. DOI: 10.1111/j.1468-0084.2006.00181.x
  
D. A. Dickey, and W. A. Fuller (1981). Likelihood Ratio Statistics for Autoregressive Time Series with a Unit Root. \emph{Econometrica}, 49: 1057-1072. DOI: 10.2307/1912517

D. Kwiatkowski, P. C. B. Phillips, P. Schmidt and Y. Shin (1992). Testing the null hypothesis of stationarity against the alternative of a unit root. \emph{Journal of Econometrics}, 54(1-3): 159-178.

S. Ng, and W. P. Perron (2001). Lag Length  Selection and the Construction of Unit Root Tests with Good Size and Power. \emph{Econometrica}, 60: 1519-1554. DOI: 10.1111/1468-0262.00256.
}
%\seealso{}
\examples{
data(industry)
indus.urt <- unirootTest(c("Job","Consum","Population","GDP"),
  group="Region",time="Year",data=industry,log=TRUE)
indus.urt      ## p-values
indus.urt$Job  ## details for variable 'Job'
}