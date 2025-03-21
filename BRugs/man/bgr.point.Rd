\name{bgrPoint}
\alias{bgrGrid}
\alias{bgrPoint}
\title{Internal functions (to support plotting the Gelman-Rubin convergence statistic)}
\description{These functions are for internal use only. 
    They support \code{\link{samplesBgr}} and \code{\link{plotBgr}}.
}
\usage{
bgrGrid(node, bins = 50)
bgrPoint(sample)
}
\arguments{
  \item{node}{Character vector of length 1, name of a variable in the model.}
  \item{bins}{Blocksize}
  \item{sample}{Monitored sample from multiple chains to calculate the
    convergence statistic for.}
}
\note{Intended for internal use only.}
\seealso{\code{\link{samplesBgr}}, \code{\link{BRugs}}, \code{\link{help.WinBUGS}}}
\keyword{internal}

