\name{difftimeFmt}
\alias{difftimeFmt}
\title{
  Formats a time difference in multiple units
}

\description{
  This function converts a time difference (in seconds) into a string with various time units (days, hours, minutes, seconds and milliseconds).
}

\usage{
  difftimeFmt(x)
}

\arguments{
  \item{x}{
    Time difference to format (single value). Can be a floating number of seconds, or a \code{difftime} object.
}
}

\value{
  Returns a character of length 1, according to the following format : "[d]d [h]:[m]:[s].[ms]". Days and milliseconds are omitted if they can be.
}

\author{
  Sylvain Mareschal
}

\examples{
  # Full format
  difftimeFmt(94521.125846)
  
  # With dates
  T1 <- strptime("2011-03-21 14:32:03", format="\%Y-\%m-\%d \%H:\%M:\%S")
  T2 <- strptime("2011-08-06 18:14:21", format="\%Y-\%m-\%d \%H:\%M:\%S")
  dT <- as.double(difftime(T2, T1, units="secs"))
  difftimeFmt(dT)
}