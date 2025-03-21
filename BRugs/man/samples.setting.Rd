\name{samplesSetting}
\alias{samplesSetBeg}
\alias{samplesSetEnd}
\alias{samplesSetThin}
\alias{samplesSetLastChain}
\alias{samplesSetFirstChain}
\title{Change settings used for calculations}
\description{These low level functions can be used to set begin, end, and thinning of chains 
as well as the first/last chain of the stored sample.}
\usage{
samplesSetBeg(begIt)
samplesSetEnd(endIt)
samplesSetThin(thin)
samplesSetFirstChain(first)
samplesSetLastChain(last)
}
\arguments{
  \item{begIt}{First iteration of the stored sample used for calculating statistics.}
  \item{endIt}{Last iteration of the stored sample used for calculating statistics.}
  \item{thin}{Every \code{thin}-th iteration of each chain is used to contribute to the statistics being calculated.}
  \item{first, last}{First/last chain of the stored sample used for calculating statistics.}
}
\details{
\code{samplesSetBeg} sets the first iteration of the stored sample used for calculating statistics to \code{begIt}.

\code{samplesSetEnd} sets the last iteration of the stored sample used for calculating statistics to \code{endIt}.

\code{samplesSetThin} sets the numerical field used to select every \code{thin}-th iteration of 
    each chain to contribute to the statistics being calculated. 
    
\code{samplesSetFirstChain} is used to set the first chain of the stored sample used for 
    calculating statistics to be \code{first}.

\code{samplesSetLastChain} is used to set the last chain of the stored sample used for calculating 
    statistics to be \code{last}.
}
\note{
    Note the difference between this and the thinning facility of the update function: 
    when thinning via the update function we are permanently discarding samples as the MCMC simulation runs, 
    whereas here we have already generated (and stored) a suitable number of (posterior) samples and 
    may wish to discard some of them only temporarily. 
    Thus, setting \code{thin > 1} here will not have any impact on the storage (memory) requirements; 
    if you wish to reduce the number of samples actually stored (to free-up memory) you should thin via the update function.
}
\seealso{\code{\link{BRugs}}, \code{\link{help.WinBUGS}}}
\keyword{interface}

