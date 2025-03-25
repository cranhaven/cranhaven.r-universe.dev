\name{CInp2TS}
\alias{CInp2TS}
\title{
Function to convert Constant Input to Time Series}
\description{
Given daily, weekly and monthly factors, this function converts from a constant numeric input
to a time series.
}
\usage{CInp2TS(cinp, prec, cinp.daily.file, cinp.weekly, cinp.seasonal)}

\arguments{
\item{cinp}{a numeric object that defines the mean constant input to be converted in time series.}

\item{prec}{A data.frame object with observations on the following 2 variables:
  \describe{
    \item{\code{time}}{a POSIXct vector}
    \item{\samp{P [mm]}}{a numeric vector}
  }}

\item{cinp.daily.file}{the path and file name of the comma separated value (csv) file that contains the
daily factors. The first column of this file should be the time in format "H:M:S" and should span for 24 hours. 
The second column should contain the factors as numeric class for the specified time. These factors must 
average to 1.}

\item{cinp.weekly}{a "list" that contains the factors per day of the week with 7 elements called "mon" for Monday,
"tue" for Tuesday, "wed" for Wednesday, "thu" for Thursday, "fri" for Friday, "sat" for Saturday, and "sun" for
Sunday. These factors must average to 1. See example about the definition of this argument.}

\item{cinp.seasonal}{a "list" that contains the factors per month of the year with 12 elements called with the three first lower case letters of the month from "jan" for January to "dec" for December. These factors must average to 1. See example about the definition of this argument.}
}

\value{Object of class \code{"list"}. This object contains 3 elements: 

    \item{\code{time}}{a POSIXct vector of length n, where n is the number of rows of the \code{prec} data.frame.}
    \item{\code{data}}{a numeric matrix of size [1:n, 1:4], where columns are: "factor", the daily factor time
    series based on \code{cinp.daily.file}; 
    "cinp", the daily time series of the variable defined by \code{cinp} according to the factors defined by 
    "factor"; 
    "cinp.week", the weekly time series of the variable defined by \code{cinp} according to the factors defined by
    \code{factor} and \code{cinp.weekly}; and   
    "cinp.season", the monthly time series of the variable defined by \code{cinp} according to the factors 
    defined by \code{factor}, \code{cinp.weekly}, and \code{cinp.seasonal}.}
  \item{\code{xts}}{An "xts" object containing the 4 same columns that \code{data}.}

 }
    
\details{
The generated time series has number of rows equal to the number of rows of the \code{prec} data.frame. 
The time index of the time series generated is the same that \code{prec}.
}

%\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
%}

%\references{
%%  ~~ possibly secondary sources and usages ~~
%}

\author{
J.A. Torres-Matallana; U. Leopold
}

\examples{
library(EmiStatR)
library(zoo)

data("Esch_Sure2010")
data("qs_factor")

cinp            <- 150 # water consumption [m3/h]
prec            <- Esch_Sure2010[1:1000,] # selecting just the first 1,000 rows

cinp.daily.file <- qs_factor
cinp.weekly     <- list(mon=1, tue=.83, wed=.83, thu=.83, fri=1, sat=1.25, sun=1.25)  
# factors average to 1
cinp.seasonal   <- list(jan=.79, feb=.79, mar=1.15, apr=1.15, may=1.15, jun=1.15,
                        jul=1.15, aug=1.15, sep=1.15, oct=1.15, nov=.79, dec=.79) 
# factors average to 1
ts1 <- CInp2TS(cinp, prec, cinp.daily.file, cinp.weekly, cinp.seasonal)
str(ts1)

head(ts1[["xts"]])
summary(ts1[["xts"]])

dev.new()
par(mfrow = c(4,1))
plot(index(ts1[["xts"]][,1]), ts1[["xts"]][,1], type = "l", 
     xlab = "", ylab = "Daily factor [-]", 
     main="Daily factor time series")
plot(index(ts1[["xts"]][,1]), ts1[["xts"]][,2], type = "l", 
     xlab = "", ylab = "Water consumption [l/(PE d)]", 
     main="Daily water consumption time series")
plot(index(ts1[["xts"]][,1]), ts1[["xts"]][,3], type = "l", 
     xlab = "", ylab = "Water consumption [l/(PE d)]", 
     main="Weekly water consumption time series")
plot(index(ts1[["xts"]]), ts1[["xts"]][,4], type = "l", 
     xlab = "Time", ylab = "Water consumption [l/(PE d)]", 
     main="Yearly water comsumption time series")
}
\keyword{CInp2TS}
