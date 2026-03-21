\name{RUN}
\alias{RUN}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Run simulation
}
\description{
Run simulation for specified number of hours. Language change beyond phonological change to happen generally requires multiple hours of simulation.
}
\usage{
RUN(nHours = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nHours}{
Number of hours to run simulation. 
}
}
\value{
No output. Objects in work space (\code{population}, \code{graveyard}, \code{situation}, \code{proposition}, \code{utterance}, \code{interpretation}) are adapted.
}
\author{
Sander Lestrade
}
\seealso{
\code{MULTIRUN}
}
\examples{
FOUND()
\dontrun{RUN(.000001)}
}
\keyword{misc}
