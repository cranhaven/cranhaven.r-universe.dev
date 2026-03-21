\name{ALLNAS}
\alias{ALLNAS}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
NA vector identification
}
\description{
Determine whether vectors consist of NA values only. 
}
\usage{
ALLNAS(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
x can be simple vector or data frame. Latter is evaluated row-wise.
}
}
\value{
T/F for single vector, vector with logicals for data frames.
}
\author{
Sander Lestrade
}
\note{
Used as input requirement for \code{VMATCH}.
}
\seealso{
\code{VMATCH}
}
\examples{
x=rep(NA, 8)
ALLNAS(x)
y=data.frame(c(1, rep(NA,2)), rep(NA,3))
ALLNAS(y)
}
\keyword{misc}
