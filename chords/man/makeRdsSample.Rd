\name{makeRdsSample}
\alias{makeRdsSample}

\title{
Generate a synthetic (simulated) RDS sample.
}

\description{
Generates a sample from the sampling process assumed in the reference.
Well, actually, only the sufficient statistics required by \code{\link{Estimate.b.k}} are returned.
}

\usage{
makeRdsSample(N.k, b.k, sample.length)
}

\arguments{
  \item{N.k}{An integer vector with the population frequency of each degree. }
  \item{b.k}{A numeric vector of the sampling rates of each degree. }
  \item{sample.length}{The length of the sample. Specified as the number of recruitees before termination.}
}

%\details{
%%  ~~ If necessary, more details than the description above ~~
%}

\value{
  An object of class \code{rds-object} suitable for applying \code{\link{Estimate.b.k}}.
}

\references{
  [1] Berchenko, Y., Rosenblatt J.D., and S.D.W. Frost. 
  "Modeling and Analyzing Respondent Driven Sampling as a Counting Process." 
  arXiv:1304.3505
  }


\note{
 The simulator does not prodice a whole RDS sample, but rather the sufficient statistics required for applying \code{\link{Estimate.b.k}}.
}


\seealso{
    \code{\link{Estimate.b.k}}
}
\examples{
# Generate data:
true.Nks <- rep(0,100); true.Nks[c(2,100)] <- 1000
theta <- 1e-1
true.log.bks <- rep(-Inf, 100);true.log.bks[c(2,100)] <- theta*log(c(2,100))
sample.length <- 1000L
rds.simulated.object <- makeRdsSample(
  N.k =true.Nks , 
  b.k = exp(true.log.bks),
  sample.length = sample.length)

# Estimate:
Estimate.b.k(rds.object = rds.simulated.object )
chords:::compareNkEstimate(rds.simulated.object$estimates$Nk.estimates, true.Nks)
}
