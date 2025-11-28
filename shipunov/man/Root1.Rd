\name{Root1}
\alias{Root1}

\title{Roots phylogenetic trees even if outgroup is not monophyletic}

\description{
  \code{Root1} non-interactively reroots a phylogenetic tree with respect
  to the specified outgroup even if it is not monophyletic.
}

\usage{
Root1(phy, outgroup, select=1, ...)
}

\arguments{
  \item{phy}{An object of class \code{"phylo"}.}
  \item{outgroup}{A vector of mode numeric or character specifying the
    new outgroup.}
  \item{select}{Which element of outgroup to select if it is not monophyletic.}
  \item{\dots}{Arguments passed to ape::root().}
}

\details{
  This is a wrapper of ape::root() to use in non-interactive mode.
  If specified outgroup is not monophyletic, instead of error, it issues
  error _message_, and chooses the 'select' element as a new outgroup.
}

\value{
  An object of class \code{"phylo"}
}

\author{Alexey Shipunov}

\seealso{
  ape::\code{\link[ape]{root}}
}

\examples{
data(bird.orders, package="ape")
ape::root(bird.orders, 1:2)
## ape::root(bird.orders, 1:3) # gives error
Root1(bird.orders, 1:3) # only outputs error _message_
Root1(bird.orders, 1, resolve.root=TRUE)
}

\keyword{manip}
