\name{evRisk}
\alias{evRisk}
\alias{print.evRisk}
\title{Risk Evaluation for Event Analysis}
\description{Conduct a risk analysis by firm and evaluate the change of risk before and after an event. The model used is the Captial Asset Pricing Model.}

\usage{evRisk(x, m = 50, r.free = "tbill", \dots)}
\arguments{
  \item{x }{ a object from \code{evReturn}.}
  \item{m }{ the number of days before and after the event date for estimating CAPM.}
  \item{r.free }{ the column name of risk free asset in \code{y}.}
  \item{\dots }{ additional arguments to be passed.}
}
\details{This fits CAPM for each firm and reports the statistics for alpha, beta, and gamma. The statistics of gamma reveal the change of risk before and after the event.}

\value{
Return a list object of class "evReturn" with the following components:
  \item{x }{ a object from \code{evReturn}.}
  \item{daEst }{  data used to estimate CAPM for the last firm as specified in code{firm}.}
  \item{rb }{ fitted CAPM for the last firm.}
  \item{reg }{ regression coefficients by firm.}
}

\section{Methods}{
  One method is defined as follows:
  \describe{
    \item{\code{print}:}{print selected outputs.}
  }
}

\references{Mei, B., and C. Sun. 2008. Event analysis of the impact of mergers and acquisitions on the financial performance of the U.S. forest products industry. Forest Policy and Economics 10(5):286-294.}
\author{Changyou Sun (\email{cs258@msstate.edu})}
\seealso{\code{\link{evReturn}}}

\examples{

data(daEsa)

hh <- evReturn(y = daEsa, firm = "wpp", 
   y.date = "date", index = "sp500", est.win = 250, digits = 3,
   event.date = 19990505, event.win = 5)
hh2 <- update(hh, firm = c("tin", "wy", "pcl", "pch"))

kk <- evRisk(x = hh2, m = 100, r.free="tb3m")
kk

}
\keyword{regression}