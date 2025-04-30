\name{mono.range}
\alias{mono.range}

\title{
Proportion in Range
}
\description{
This function reports the proportion of entries that fall inside of the prespecified range.
}
\usage{
mono.range(data, data.r, tol, xr.col, x.col, y.col)
}

\arguments{
  \item{data}{
a data.frame or matrix of measurement data
}
  \item{data.r}{
range for y values; must have three columns: 1 - must match values in x.col, 2 - lower range values, 3 - upper range values
}
  \item{tol}{
tolerance; how much outside of the range (data.r) is acceptable; same units as data in y.col
}
  \item{xr.col}{
column where x values, or time variable is stored in data.r
}
  \item{x.col}{
column where x values, or time variable is stored in data
}
  \item{y.col}{
column where y values, or measurements are stored in data
}
}

\value{
Returns the proportion of y values that fall inside the prespecified range 
}

\author{Michele Josey  \href{mailto:mjosey@nccu.edu}{mjosey@nccu.edu} 
Melyssa Minto  \href{mailto:mminto@nccu.edu}{mminto@nccu.edu}
}

\examples{
data(simulated_data)
data(data.r)

mono.range(simulated_data, data.r, tol=4, xr.col=1 ,x.col=2, y.col=3)
}
\keyword{range}

