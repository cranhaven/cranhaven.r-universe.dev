\name{orderFeatures}
\alias{orderFeatures}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Order features, based on their ability to discriminate}

\description{ For each row of \code{data}, an F or (potentially) other
statistic is calculated, using the function \code{FUN}, that measures
the extent to which this variable separates the data into groups. This
statistic is then used to order the rows.
}

\usage{ orderFeatures(x, cl, subset = NULL, FUN = aovFbyrow, values =
FALSE) }
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{Matrix; rows are features, and columns are observations 
('samples')}
  \item{cl}{Factor that classifies columns into groups}
  \item{subset}{allows specification of a subset of the columns of \code{data}}
  \item{FUN}{specifies the function used to measure separation between groups}
  \item{values}{if \code{TRUE}, F-values as well as the ordering are returned}
}
\value{
Either (\code{values=FALSE}) a vector that orders the rows,
or (\code{values=TRUE})
  \item{ord}{a vector that orders the rows}
  \item{stat}{ordered values of the statistic}
}
\author{John Maindonald}
\examples{
mat <- matrix(rnorm(1000), ncol=20)
cl <- factor(rep(1:3, c(7,9,4)))
ord <- orderFeatures(mat, cl)

## The function is currently defined as
function(x, cl, subset=NULL, FUN=aovFbyrow, values=FALSE){
    if(dim(x)[2]!=length(cl))stop(paste("Dimension 2 of x is",
                  dim(x)[2], "differs from the length of cl (=",
                  length(cl)))
    ## Ensure that cl is a factor & has no redundant levels
    if(is.null(subset))
      cl <- factor(cl)
    else
      cl <- factor(cl[subset])
    if(is.null(subset))
      stat <- FUN(x, cl)
    else
      stat <- FUN(x[, subset], cl)
    ord <- order(-abs(stat))
    if(!values)ord else(list(ord=ord, stat=stat[ord]))
  }
}
\keyword{htest}% at least one, from doc/KEYWORDS

