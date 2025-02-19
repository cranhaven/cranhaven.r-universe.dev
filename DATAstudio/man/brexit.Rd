\encoding{UTF-8}
\name{brexit}
\alias{brexit}
\title{Brexit Poll Tracker}
\description{
  The data consist of 267 polls conducted before the June 23 2016 EU
  referendum, which took place in the UK.
}
\usage{brexit}
\format{
  A dataframe with 272 observations on six variables.
  \itemize{
    \item{\code{leave}, \code{stay}, \code{undecided}}{: 
      percentage in favor of each option.
    }
    \item{\code{date}}{: 
      date on which the poll was conducted.
    }
    \item{\code{pollster}}{: 
      institution conducting the poll.
    }
    \item{\code{size}}{: 
      number of polled subjects.
    }    
  }
}
\references{
  de Carvalho, M. and Martos, G. (2020). Brexit: Tracking and
  disentangling the sentiment towards leaving the EU. \emph{International Journal
  of Forecasting}, \bold{36}, 1128-1137.
}
\source{
  Financial Times (FT) Brexit poll tracker.	
}	
\examples{
## Leave-stay plot (de Carvalho and Martos, 2018; Fig. 1)
data(brexit)
attach(brexit)
oldpar <- par(pty = "s")
plot(leave[(leave > stay)], stay[(leave > stay)],
     xlim = c(22, 66), ylim = c(22, 66), pch = 16, col = "red",
     xlab = "Leave", ylab = "Stay")
points(leave[(stay > leave)], stay[(stay > leave)],
       pch = 16, col = "blue")
points(leave[(stay == leave)], stay[(stay == leave)],
       pch = 24)
abline(a = 0, b = 1, lwd = 3)
par(oldpar)
}
\keyword{Political}
