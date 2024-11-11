\name{detail}
\alias{detail}
\title{
  The detail extraction function for simulated power objects
}
\description{
  Objects created by the \code{pwrFDR} function with option
  \code{method}=="simulation" are returned with an attribute named
  \code{detail}. This is its extractor function
}
\usage{
detail(obj)
}
\arguments{
  \item{obj}{
    An object created by the \code{pwrFDR} function with option
    \code{method}=="simulation".
  }
}
\value{
  A list with components
  \item{reps}{A data frame of \code{n.sim} rows containing the results of 
    the simulations as columns: \code{M1}, the \# of non-null distributed
    statistics, \code{R}, the \# of statistical tests rejected under the
    BH-FDR procedure, and \code{T}, the \# of true positives.
    Depending on the value of \code{FDP.control.method}, additional
    values are returned:\cr
    FDP.control.method:\cr
    "BHFDR": nothing further\cr
    "BHCLT": In addition to \code{M1}, \code{R} and \code{T}, we compute
    the \# tests rejected under the BH-CLT procedure, \code{R.st},
    (see detail), \code{T.st}, \# non-null distributed tests rejected
    under the BH-CLT procedure and if \code{sim.level}=2,
    \code{R.st.ht}, and \code{T.st.ht}.
  }
  \item{X}{A single simulation replicate of the \code{m} raw test
    statistics, included as a sanity check}
}
\author{
  Grant Izmirlian izmirlig at mail dot nih dot gov
}
