\name{C_utils-time}
\Rdversion{1.1}
\encoding{UTF-8}
\alias{hms}
\alias{doy}
\alias{dom}
\alias{dst}
\alias{truncDay}

\title{Utilities for time indexes.}

\description{Several small functions to extract information from \code{POSIXct}
  indexes.
}

\usage{
hms(x)
doy(x)
dom(x)
dst(x)
truncDay(x)
}
\arguments{
  \item{x}{A \code{POSIXct} vector.}
}
\value{\code{doy} and \code{dom} provide the (numeric) day of year and day of month,
  respectively.

  \code{hms} gives the numeric value

  \code{hour(x)+minute(x)/60+second(x)/3600}

  \code{dst} is +1 if the Daylight Savings Time flag is in force,
  zero if not, -1 if unknown (\code{\link{DateTimeClasses}}).

  \code{truncDay} truncates the \code{POSIXct} object towards the day.
}


\author{
  Oscar Perpiñán Lamigueiro,
  Francisco Delgado López.
}
\seealso{\code{as.POSIXct}
}

\keyword{utilities}

