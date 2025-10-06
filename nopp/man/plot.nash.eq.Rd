\name{plot.nash.eq}
\alias{plot.nash.eq}
\title{
Plot function for Nash equilibrium object
}
\description{
Plot function for Nash equilibrium object
}
\usage{
\method{plot}{nash.eq}(x,\dots)
}
\arguments{
  \item{x}{a \code{nash.eq} object}
  \item{\dots}{additional arguments passed to the inner plot function}
}
\details{
See vignette.
}
\author{
Luigi Curini, Stefano M. Iacus
}
\seealso{
See Also as \code{\link{equilibrium}}
}
\references{
Curini, L., and Iacus, S.M. (2017) Nash Optimal Party Positions: The nopp R Package, Journal of Statistical Software, 81(11), 1--25
}
\examples{
\dontrun{
data(italy2006)
election <- set.data(italy2006 , shape="wide", choice="vote", varying=c(5:14), sep="_")
m <- mlogit(vote~prox+partyID | gov_perf+sex+age+education, election, reflevel = "UL")

true.pos <- list(FI=7.59, UL=3.50, RC=1.95, AN=8.08, UDC=5.66)
true.votes <- list(FI=.24, UL=.40, RC=.10, AN=.18, UDC=.08)

# comparison against true votes and party positions
nash.eq <- equilibrium(model=m, data=election, pos=true.pos, votes=true.votes)
nash.eq
par(mfrow=c(3,1))
plot(nash.eq)



# bootstrap confidence intervals
nash.eq <- equilibrium(model=m, data=election, boot=10)
nash.eq
plot(nash.eq)

par(mfrow=c(1,1))
}
}
