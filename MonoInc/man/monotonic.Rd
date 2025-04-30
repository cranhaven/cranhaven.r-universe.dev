\name{monotonic}
\alias{monotonic}

\title{
Monotonic Check 
}
\description{
This function can check the monoticity of a single vector, matrix, or data.frame that has multiple IDs within the matrix or data.frame. 
}
\usage{
monotonic(data, id.col=NULL, y.col=NULL, direction)
}

\arguments{
  \item{data}{
a data.frame or matrix or vector of measurement data
}
  \item{id.col}{
column where the id's are stored; default is NULL
}
  \item{y.col}{
column where y values, or measurements are stored; default is NULL
}
  \item{direction}{
the direction of the function a choice between increasing 'inc', and decreasing 'dec'
}
}

\value{
If the user enters a vector, the function returns TRUE or FALSE as to where that particular vector is monotonic increasing or not, it returns NA if the vector has missing values. If the user enters a matrix or data frame, the function returns a matrix with 2 columns. The first column as the id.  The second column as a 0 for FALSE and 1 for TRUE as to where the data in that particular id is monotonic increasing or not, or NA if the y column has missing values in that particular id.
}

\author{Michele Josey  \href{mailto:mjosey@nccu.edu}{mjosey@nccu.edu} 
Melyssa Minto  \href{mailto:mminto@nccu.edu}{mminto@nccu.edu}
}

\examples{
data(simulated_data)
## Run monotonic
test <- monotonic(simulated_data, 1,3, direction='inc')

## look at the number of ids that are non-monotonic
table(as.logical(test[,2]))

##to ignore NA values
x<-c(1,2,3,5,NA,7,8)
monotonic(na.omit(x), direction='inc')

}

\keyword{mono}

