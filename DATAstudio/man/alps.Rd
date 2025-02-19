\encoding{UTF-8}
\name{alps}
\alias{alps}
\title{Swiss Alps Temperature Data}
\description{
  The \code{alps} data data consist of daily winter temperature
  minima and maxima measured at 2m above ground surface 
  at two sites in the Swiss Alps: Montana and Zermatt.
}
\usage{alps}
\format{
  The \code{alps} data frame contains the following columns:
  \itemize{
    \item{\code{date}}{:
	  date of measurements.
    }
    \item{\code{min_montana}, \code{min_zermatt}}{: 
      daily minimum temperature in ºC on Montana and Zermatt. 
    }
    \item{\code{min_montana}, \code{min_zermatt}}{: 
      daily maximum temperature in ºC on Montana and Zermatt. 
    }    
  }
}
\source{MeteoSwiss}
\references{
  Mhalla, L., de Carvalho, M., and Chavez-Demoulin, V. (2019)
  Regression type models for extremal dependence.
  \emph{Scandinavian Journal of Statistics}, \bold{46}, 1141-1167.
}
\examples{
## visualizing the data
data(alps)
oldpar <- par(pty = 's', mfrow = c(1, 2))
plot(alps$min_montana, alps$min_zermatt, pch = 20, 
     xlab = "Montana", ylab = "Zermatt", main = "Daily Minimum")
plot(alps$max_montana, alps$max_zermatt, pch = 20, 
     xlab = "Montana", ylab = "Zermatt", main = "Daily Maximum")
par(oldpar)

oldpar <- par(pty = 's', mfrow = c(1, 2))
plot(alps$min_montana, alps$max_montana, pch = 20, 
     xlab = "Minimum", ylab = "Maximum", main = "Montana")
abline(a = 0, b = 1, col = "red", lty = 2)
plot(alps$min_zermatt, alps$max_zermatt, pch = 20, 
     xlab = "Minimum", ylab = "Maximum", main = "Zermatt")
abline(a = 0, b = 1, col = "red", lty = 2)
par(oldpar)

## to download the NAO daily index in Mhalla et al (2019) use
## the R package data.table to access NOAA via ftp 
link <- "ftp://ftp.cdc.noaa.gov/Public/gbates/teleconn/nao.reanalysis.t10trunc.1948-present.txt"
NAO.daily <- data.table::fread(link)
NAO.daily <- data.frame(NAO.daily)
colnames(NAO.daily) <- c("year", "month", "day", "NAO")
}
\keyword{Climatological}
