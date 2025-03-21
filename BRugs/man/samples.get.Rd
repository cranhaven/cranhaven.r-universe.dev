\name{samplesGet}
\alias{samplesGetBeg}
\alias{samplesGetEnd}
\alias{samplesGetThin}
\alias{samplesGetLastChain}
\alias{samplesGetFirstChain}
\title{Get settings used for calculations}
\description{These low level functions can be used to get information on settings of begin, end, and thinning of chains,
as well as the number of the first/last chain of the stored sample.}
\usage{
samplesGetBeg()
samplesGetEnd()
samplesGetThin()
samplesGetFirstChain()
samplesGetLastChain()
}
\value{
\code{samplesGetBeg} returns the first iteration of the stored sample used for calculating statistics.

\code{samplesGetEnd} returns the last iteration of the stored sample used for calculating statistics to end.

\code{samplesGetThin} returns the thin parameter, see \code{\link{samplesSetThin}}.

\code{samplesGetFirstChain} returns the number of the first chain of the stored sample used for calculating statistics.

\code{samplesGetLastChain} returns the number of the last chain of the stored sample used for calculating statistics.

}
\seealso{\code{\link{samplesSetBeg}}, \code{\link{BRugs}}, \code{\link{help.WinBUGS}}}
\keyword{interface}
