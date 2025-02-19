\encoding{UTF-8}
\name{metsynd}
\alias{metsynd}
\title{Metabolic Syndrome Data}
\description{
  The \code{metsynd} data includes Gamma-Glutamyl Transferase (GGT)
  levels and curves of arterial oxygen saturation, for samples of women
  suffering from metabolic syndrome and women without metabolic
  syndrome; the data were gathered from a population-based survey
  conducted in Galicia (NW Spain), and it includes 35 women suffering
  from metabolic syndrome and 80 women without metabolic syndrome.
}
\usage{metsynd}
\format{
  The data consist of a list with the following elements:
  \itemize{
    \item{\code{y0}}{: 
      GGT levels for women without metabolic syndrome.
    }
    \item{\code{y1}}{: 
      GGT levels for women suffering from metabolic syndrome.
    }
    \item{\code{X0}}{: 
      Curves of arterial oxygen saturation (\%) for women without
      metabolic syndrome (\code{X0$data}, \code{X0$time}).
    }
    \item{\code{X1}}{: 
      Curves of arterial oxygen saturation (\%) for women suffering from
      metabolic syndrome (\code{X1$data}, \code{X1$time}).      
    }
  }
}
\details{
  The curves of arterial oxygen saturation are included in the matrices
  \code{X0$data} and \code{X1$data}, with each row representing a
  patient, and with columns representing ordered measurements over time.
  Here \code{X0$time} and \code{X1$time} represents the time (in hours) at
  which measurements were made, i.e., every 20 seconds during three
  hours of sleep. Further details on these data can be found in the
  references below. 
}
\references{  
  Inácio de Carvalho, V., de Carvalho, M., Alonzo, T. A.,
  González-Manteiga, W. (2016) Functional covariate-adjusted partial
  area under the specificity-ROC curve regression with an application
  to metabolic syndrome case study. \emph{Annals of Applied Statistics},
  \bold{10}, 1472-1495
}
\examples{
data(metsynd)
library(scales)
attach(metsynd)

## Inacio de Carvalho et al (2016; Fig 1)
oldpar <- par(mfrow = c(1,2))
n0 <- length(y0)
n1 <- length(y1)
t <- X1$time
plot(t, X1$data[1, ], type = "l", lwd = 3, ylim = c(70, 100), 
     xlab = "Time (in hours)", ylab = "Arterial oxygen saturation (\%)", 
     main = "Metabolic syndrome")
for (i in 2:n1)
  lines(t, X1$data[i, ], type = "l", lwd = 3, col = alpha("black", i / n1))
plot(t, X0$data[1, ], type = "l", lwd = 3, col = "gray", ylim = c(70, 100), 
     xlab = "Time (in hours)", ylab = "Arterial oxygen saturation (\%)", 
     main = "No metabolic syndrome")
for (i in 1:n0)
  lines(t, X0$data[i, ], type = "l", lwd = 3, col = alpha("gray", i / n0))
par(oldpar)
}
\keyword{Medical}
