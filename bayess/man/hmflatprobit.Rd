\name{hmflatprobit}
\alias{hmflatprobit}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Metropolis-Hastings for the probit model under a flat prior%%  ~~function to do ... ~~
}
\description{
This random walk Metropolis-Hastings algorithm takes advantage of the
availability of the maximum likelihood estimator (available via the \code{glm}
function) to center and scale the random walk in an efficient manner.
}
\usage{
hmflatprobit(niter, y, X, scale)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
 \item{niter}{
number of iterations%%     ~~Describe \code{niter} here~~
}
  \item{y}{
binary response variable%%     ~~Describe \code{y} here~~
}
  \item{X}{
covariates%%     ~~Describe \code{X} here~~
}
  \item{scale}{
scale of the random walk%%     ~~Describe \code{scale} here~~
}
}
\value{
%%  ~Describe the value returned
The function produces a sample of \eqn{\beta}{beta}'s of size \code{niter}.
}
\seealso{
\code{\link{hmflatlogit}}%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
data(bank)
bank=as.matrix(bank)
y=bank[,5]
X=bank[,1:4]
flatprobit=hmflatprobit(1000,y,X,1)
mean(flatprobit[101:1000,1])
}
\keyword{flat prior}
\keyword{probit model}
\keyword{Metropolis-Hastings algorithm}
\keyword{random walk proposal}
