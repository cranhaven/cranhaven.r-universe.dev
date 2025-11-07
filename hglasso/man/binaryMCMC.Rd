\name{binaryMCMC}
\alias{binaryMCMC}
\title{
Generate samples using Gibbs sampling for binary network specified by the parameter Theta 
}
\description{
Sampling from the binary Ising model using Gibbs sampling.  This function is not efficient and is only intended to be used in the examples.
}
\usage{
binaryMCMC(n, Theta, burnin, skip,trace=FALSE)
}

\arguments{
  \item{n}{
The number of samples.
}
  \item{Theta}{
A symmetric parameter matrix for the model from which the data is being generated.}
  \item{burnin}{
The number of samples to discard as burn in.
}
  \item{skip}{
The number of samples to discard in-between returned samples.
}
\item{trace}{
Default value of trace=FALSE. If trace=TRUE, the progress of Gibbs sampling is printed when each observation is sampled.
}
}

\value{
\item{X}{An n x p matrix of samples generated from the binary network specified by Theta.}
}
\references{
Tan et al. (2014). Learning graphical models with hubs. To appear in Journal of Machine Learning Research. arXiv.org/pdf/1402.7349.pdf.
}
\author{
Kean Ming Tan}

\seealso{
  \code{\link{HubNetwork}}
  }
\examples{
# generate Theta that specified the structure of a binary Ising model with p=10 variables and 2 hubs
#p<-10
#n<-50
#Theta <- HubNetwork(p,0.95,2,0.3,type="binary")$Theta

# generate samples using Gibbs sampling
#X <- binaryMCMC(n,Theta,burnin=1000,skip=500)

}


