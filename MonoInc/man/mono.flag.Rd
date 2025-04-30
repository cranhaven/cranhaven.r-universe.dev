\name{mono.flag}
\alias{mono.flag}

\title{
Flag
}
\description{
This function flags data that is outside the prespecified range and that is not monotonic. 
}
\usage{
mono.flag(data, id.col, x.col, y.col, min, max, data.r = NULL, tol = 0, direction)
}

\arguments{
  \item{data}{
a data.frame or matrix of measurement data
}
  \item{id.col}{
column where the id's are stored
}
  \item{x.col}{
column where x values, or time variable is stored
}
  \item{y.col}{
column where y values, or measurements are stored
}
  \item{min}{
lowest acceptable value for measurement; does not have to be a number in ycol
}
  \item{max}{
highest acceptable value for measurement; does not have to be a number in ycol
}
  \item{data.r}{
prespecified range for y values; must have three columns: 1 - must match values in xcol, 2 - lower range values, 3 - upper range values
}
  \item{tol}{
tolerance; how much outside of the range (data.r) is acceptable; same units as data in ycol 
}
  \item{direction}{
the direction of the function a choice between increasing 'inc', and decreasing 'dec'
}
}
\details{
The data range (data.r) does not need to have the same number of rows as data; it only needs to include the exact time increments as xcol. 
}
\value{
Returns the data matrix with two additional columns. "Decreasing" is a logical vector that is TRUE if the observation decreases, or causes the ID to be non-monotonic.  "Outside.Range" is a logical vector that returns TRUE if the observation is outside of the data.r +/- tol range. Any duplicate rows are removed.
}

\author{Michele Josey  \href{mailto:mjosey@nccu.edu}{mjosey@nccu.edu} 
Melyssa Minto  \href{mailto:mminto@nccu.edu}{mminto@nccu.edu}
}

\examples{
data(simulated_data)
simulated_data <- simulated_data[1:1000,]
data(data.r)
## run mono.flag function 
test <- mono.flag(simulated_data, 1, 2, 3, 30, 175, data.r=data.r, direction='inc')
head(test)

}

\keyword{flag}

