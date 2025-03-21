\name{modelUpdate}
\alias{modelUpdate}
\title{Updating the model}
\description{This function updates the model.}
\usage{
modelUpdate(numUpdates, thin = 1, overRelax = FALSE)
}
\arguments{
  \item{numUpdates}{This function updates the model by carrying out \code{thin * numUpdates} MCMC iterations for each chain.}
  \item{thin}{The samples from every k\emph{th} iteration will be used for inference, where k is the value of \code{thin}. 
        Setting \code{thin > 1} can help to reduce the autocorrelation in the sample, 
        but there is no real advantage in thinning except to reduce storage requirements. }
  \item{overRelax}{If \code{overRelax} is \code{TRUE} an over-relaxed form of MCMC (Neal, 1998) 
        which will be executed where possible. 
        This generates multiple samples at each iteration and then selects one that is 
        negatively correlated with the current value. 
        The time per iteration will be increased, but the within-chain correlations should be 
        reduced and hence fewer iterations may be necessary. 
        However, this method is not always effective and should be used with caution. 
        The auto-correlation function may be used to check whether the mixing of the chain is improved.}
}
\note{This function can be executed once the model has been compiled and initialized.

If an attempt is made to execute this function in an inappropriate context the generic error message 
\sQuote{command is not allowed (greyed out)} is displayed. 
}
\references{
Neal, R. (1998): Suppressing random walks in Markov chain Monte Carlo using ordered over-relaxation. 
In M.I. Jordan (Ed.): \emph{Learning in Graphical Models}, Kluwer
Academic Publishers, Dordrecht, 205-230.
\url{https://glizen.com/radfordneal/publications.html}
}
\seealso{\code{\link{BRugs}}, \code{\link{help.WinBUGS}}}
\keyword{interface}
\concept{update}
\concept{iteration}
