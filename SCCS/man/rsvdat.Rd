\name{rsvdat}
\docType{data}
\alias{rsvdat}
\title{Data on RSV and ambient temperature}
\description{
The data are a weekly time series of counts of respiratory syncytial virus (RSV) isolates from England and Wales and average temperatures (in degrees Celsius) in Central England. 
}
\usage{rsvdat}
\format{A data frame containing 364 rows and 5 columns. The column names are 'year' (year of report, 1996 to 2003), 'week' (week of the year, numbered 1 to 52), 'win' (4-week window, numbered 1 to 91), 'temp' (average daily temperature for the week prior to the current week), and 'rsv' (count of RSV isolates for the current week). The data are in time series format, for SCCS analysis using generalised linear models.}
%\source{}
\references{Whitaker, H. J., Hocine, N. and Farrington C. P. (2007). On case-crossover methods for environmental time series data. Environmetrics 18 (2), 157-171.
}
\keyword{datasets}
