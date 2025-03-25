\name{IsReg}
\alias{IsReg}
\title{
Wrapping Function for Function "is.regular" from "zoo" package for "data.frame" objects
}
\description{
"IsReg" is a wrapping Function for Function "is.regular" from "zoo" package. Given a  "data.frame" 
object it is converted into a "xts" object, while the regularity of the object is checked. The first 
column of the "data.frame" should contain a character string vector to be converted via as.POSIXct 
accordingly with the date format (format) and time zone (tz). 

}
\usage{IsReg(data, format, tz)}

\arguments{
\item{data}{an object of class \code{data.frame} containing in its first column a character string vector
to be converted via as.POSIXct into a date vector accordingly with the date format (format) and time zone (tz)
defined}

\item{format}{character string giving a date-time format as used by \code{strptime}.}

\item{tz}{a time zone specification to be used for the conversion, if one is required. 
System-specific, but "" is the current time zone, and "GMT" is UTC (Universal Time, Coordinated). 
Invalid values are most commonly treated as UTC, on some platforms with a warning.}
}

\value{Object of class \code{"list"}. This object contains 2 elements, 
    the first one contains a character string "_TSregular" if the xts object created is strict regular, 
    or "_TSirregular" if it is strict irregular. More details can be found in the "is.regular" function of the
    "zoo" package.}
    
\details{
"IsReg" calls the as.POSIXct function from \code{base} package to convert an object to one of the two 
classes used to represent date/times (calendar dates plus time to the nearest second). 
More details can be found in the "is.regular" function of the "zoo" package.
}
%\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
%}
%\references{
%%  ~~ possibly secondary sources and usages ~~
%}

\author{
J.A. Torres-Matallana
}

\examples{
library(EmiStatR)
data("P1")

class(P1)
head(P1)

ts <- IsReg(data = P1, format = "\%Y-\%m-\%d \%H:\%M:\%S", tz = "UTC")
str(ts)

ts[[1]]

head(ts[[2]]); tail(ts[[2]])

plot(ts[[2]], ylab = "Precipitation [mm]")
}

\keyword{IsReg}
\keyword{Is Regular}