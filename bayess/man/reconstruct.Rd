\name{reconstruct}
\alias{reconstruct}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Image reconstruction for the Potts model with six classes%%  ~~function to do ... ~~
}
\description{
This function adresses the reconstruction of an image distributed from
a Potts model based on a noisy version of this image. The purpose of image segmentation 
(Chapter 8) is to cluster pixels into homogeneous classes without supervision or
preliminary definition of those classes, based only on the spatial
coherence of the structure. The underlying algorithm is an hybrid Gibbs sampler.
}
\usage{
reconstruct(niter, y)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{niter}{
number of Gibbs iterations%%     ~~Describe \code{niter} here~~
}
  \item{y}{
blurred image defined as a matrix%%     ~~Describe \code{y} here~~
}
%  \item{m}{
%useless parameter?%%     ~~Describe \code{m} here~~
%}
}
\details{
Using a Potts model on the true image, and uniform priors on
the genuine parameters of the model, the hybrid Gibbs sampler generates
the image pixels and the other parameters one at a time,
the \emph{hybrid} stage being due to the Potts model parameter, since
it implies using a numerical integration via \code{integrate}. 
The code includes (or rather excludes!) the numerical integration via the vector \code{dali},
which contains the values of the integration over a 21 point grid, since
this numerical integration is extremely time-consuming.
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
  \item{beta}{MCMC chain for the parameter \eqn{\beta}{beta} of the Potts model}
  \item{mu}{MCMC chain for the mean parameter of the blurring model}
  \item{sigma}{MCMC chain for the variance parameter of the blurring model}
  \item{xcum}{frequencies of simulated colours at every pixel of the image}
}
\seealso{
\code{\link{Menteith}}
}
\examples{
\dontrun{data(Menteith)
lm3=as.matrix(Menteith)
#warning, this step is a bit lengthy
titus=reconstruct(20,lm3)
#allocation function
affect=function(u) order(u)[6]
#
aff=apply(titus$xcum,1,affect)
aff=t(matrix(aff,100,100))
par(mfrow=c(2,1))
image(1:100,1:100,lm3,col=gray(256:1/256),xlab="",ylab="")
image(1:100,1:100,aff,col=gray(6:1/6),xlab="",ylab="")
}}
\keyword{Menteith}
\keyword{Potts model}
\keyword{image reconstruction}
\keyword{numerical integration}
