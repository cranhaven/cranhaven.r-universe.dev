\name{evReturn}
\alias{evReturn}
\alias{print.evReturn}
\alias{plot.evReturn}
\title{Estimating Abnormal Return from Event Analysis}
\description{Conduct an event analysis and estimate abnormal returns over time and across firms.}

\usage{evReturn(y, firm, event.date, y.date = "date",
  index = "sp500", event.win = 3, est.win = 250, digits = 4, \dots)}
\arguments{
  \item{y }{ a data frame object with one column for date, return series by firms, a return series for a stock market index, and a return series for a risk free asset.}
  \item{firm }{ a character vector of firm names; this is the name of the return series in \code{y}.}
  \item{event.date }{ event dates for each firm as specified in \code{firm}; this should be a numerical vector and can match the values in \code{y$y.date}; if event dates are the same for all the firms, this can be specificed as a single number.}
  \item{y.date }{ a character value for the column name of date in \code{y}.}
  \item{index }{ a character value for the column name of index in \code{y}.}
  \item{event.win }{ the one-side width of event window in days; the default value of 3 corresponds to a 7-day window (i.e., 3 + 1 + 3).}
  \item{est.win }{ the width of estimation window in days.}
  \item{digits }{ number of digits used to format outputs.}
  \item{\dots }{ additional arguments to be passed.}
}
\details{This is the core function for event analysis. It estimates a market model by firm and then calculate abnormal returns by firm and over time. The time series of stock returns have irregular time frequency because of varying trading days. Thus, the time dimension is explicitly specified as a \code{y.date} column in the data of \code{y}.}

\value{
Return a list object of class "evReturn" with the following components:
  \item{y }{ a data frame of raw return data. }
  \item{y.date }{ a character value for the column name of date in \code{y}..}
  \item{firm }{  a character vector of firm names.}
  \item{N}{  the number of firms.}
  \item{index }{ a character value for the column name of index in \code{y}.}
  \item{event.date }{ event dates for each firm as specified in \code{firm}.}
  \item{event.win }{ the one-side width of event window in days.}
  \item{event.width }{  total number of days in an event window.}
  \item{est.win }{ the width of estimation window in days..}
  \item{daEst }{  data used to estimate the market model for the last firm as specified in code{firm}.}
  \item{daEve }{  data over the event window for the last firm.}
  \item{ra }{ fitted market model for the last firm.}
  \item{digits }{ number of digits used to format outputs.}
  \item{reg }{ regression coefficients by firm.}
  \item{abr }{ abnormal returns by day over the event window and by firm.}
  \item{abc }{ average abnormal returns across firms.}
  \item{call }{ a record of the system call; this allows \code{update.default} to be used. }
}

\section{Methods}{
  Two methods are defined as follows:
  \describe{
    \item{\code{print}:}{print three selected outputs.}
    \item{\code{plot}:}{plot average cumulative abnormal returns from event analysis versus days in event window.}    
  }
}

\references{
Mei, B., and C. Sun. 2008. Event analysis of the impact of mergers and acquisitions on the financial performance of the U.S. forest products industry. Forest Policy and Economics 10(5):286-294.

Sun, C., and X. Liao. 2011. Effects of litigation under the Endangered Species Act on forest firm values. Journal of Forest Economics 17(4):388-398.
}

\author{Changyou Sun (\email{cs258@msstate.edu})}
\seealso{\code{\link{evRisk}}}

\examples{
data(daEsa)

# event analysis for one firm and one event window
hh <- evReturn(y = daEsa, firm = "wpp", 
   y.date = "date", index = "sp500", est.win = 250, digits = 3,
   event.date = 19990505, event.win = 5)
hh; plot(hh)

# event analysis for many firms and one event window
hh2 <- update(hh, firm = c("tin", "wy", "pcl", "pch")); hh2

# event analysis for many firms and many event windows: need a for loop

}
\keyword{regression}