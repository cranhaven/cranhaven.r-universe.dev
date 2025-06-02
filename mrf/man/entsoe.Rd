\docType{data}
\name{entsoe}
\alias{entsoe}
\title{Entsoe DataFrame containing Time Series}
\description{
Data from a European Network of Transmission System Operators for Electricity
Accessed: 2020-08-20, 2019.
Time series contains 3652 data points without missing values.
Data describes electrict load for time range between 2006 and 2015
}
\format{
A DataFrame with 3652 rows and 2 columns
}
%\describe{
%  \item{time}{Timestamps}
%  \item{value}{Electricity demand values in MWH}
%}
\source{
\href{https://www.entsoe.eu/data/power-stats/}{Archive}
}
\usage{
data(entsoe)
}
\examples{
data(entsoe)
data = entsoe$value
}
\keyword{datasets}
