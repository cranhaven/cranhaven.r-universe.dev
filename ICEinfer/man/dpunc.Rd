\name{dpunc}
\alias{dpunc}
\docType{data}
\title{
 Output list object of class ICEuncrt for the High Uncertainty numerical example in
 the ICEinfer package, data(dulxparx).
}
\description{
 dpunc is the output list object of class ICEuncrt resulting from the following time consuming
 computation: dpunc <- ICEuncrt(dulxparx, dulx, idb, ru, lambda=0.26)
}
\usage{data(dpunc)}
\format{
  Output list object of class ICEuncrt.
  \describe{
    \item{df}{Saved value of the name of the data.frame input to ICEuncrt.}
    \item{lambda}{Saved positive value of lambda input to ICEuncrt.}
    \item{ceunit}{Saved value of ceunit ("cost" or "effe") input to ICEuncrt.}
    \item{R}{Saved integer value for number of bootstrap replications input to ICEuncrt.}
    \item{trtm}{Saved name of the treatment indicator within the df data.frame.}
    \item{xeffe}{Saved name of the treatment effectiveness variable within the df data.frame.}
    \item{ycost}{Saved name of the treatment cost variable within the df data.frame.}
    \item{effcst}{Saved value of the sorted 3-variable (trtm,effe,cost) data.frame.}
    \item{t1}{Observed value of (DeltaEffe, DeltaCost) when each patient is included exactly once.}
    \item{t}{R x 2 matrix of values of (DeltaEffe, DeltaCost) computed from bootstrap resamples.}
    \item{seed}{Saved value of the seed used to start pseudo random number generation.}
  }
}
\examples{
    # Intermediate ICEinfer Output List for the dulxparx dataset...
    data(dpunc)
    plot(dpunc)
}
\keyword{datasets}