\name{lss}
\alias{lss}
\title{List Objects with Their Sizes}
\description{Show the name and memory size of objects in an R session.}
\usage{lss(n = 5, pos = 1, decreasing = TRUE, order.by=c("Size", "Type")) }
\arguments{
  \item{n }{ number of objects to show.}
  \item{pos }{ specifying the environment; see \code{ls}.}
  \item{decreasing }{ sorting order.}
  \item{order.by }{ sorting variable.}
      
}
\details{This function shows the names and sizes of objects in an R session. This is useful for managing avaiable memory in an R Session.}

\value{
Return a dataframe object with the following columns: name, type, size, prettysize, rows, and columns.
}

\author{Changyou Sun (\email{cs258@msstate.edu})}

\seealso{\code{\link{ls}}.}

\examples{
  lss()
  ls()
}
\keyword{manip}