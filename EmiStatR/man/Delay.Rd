\name{Delay}
\alias{Delay}
\title{Delay function for time series}
\description{
This function allows to cretae a n-time steps delayed time series, where n is the number of time steps defined by argument \code{x}. 
Henceforth, it is possible to calibrate this argument (parameter) \code{x}. 
}
\usage{
  Delay(P1, x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{P1}{
    A \code{data.frame} that contains the time series of the 
    environmental variable to be delayed, e.g. precipitation. This data.frame should have 
    at least two columns: the first one, Time [y-m-d h:m:s]; the second one, a numeric value equal to the 
    magnitude of the environmental variable. If the environmental variable is different than precipitation,
    then the column name of the values should be named as \code{value}.}
  \item{x}{
    A \code{numeric} value that specifies the delayed time in time steps.}
}
%\details{
  %%  ~~ If necessary, more details than the description above ~~
    %}
\value{
  A \code{data.frame} with two columns:
    \item{time}{the date-time time series of the delayed variable}
    \item{value}{time series with the magnitude (equal to the original, \code{P1}, time series) of the delayed variable.}
}
%\references{
  %% ~put references to the literature/web site here ~
    %}
%\details{
%%  ~~ If necessary, more details than the __description__ above ~~
%}
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
data(P1)

P1_delayed <- Delay(P1 = P1, x = 500)

head(P1_delayed)

dev.new()
par(mfrow = c(2, 1))
plot(P1[,1], P1[,2], typ = "l", col = "blue")
plot(P1_delayed[,1], P1_delayed[,2], typ= "l", col = "red")
}
\keyword{Delay}
