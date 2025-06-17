\name{effect.sizes}
\alias{effect.sizes}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to extract species pairwise effect sizes from an object of class \code{cooccur}.
}
\description{
Calculate standardized and raw effect sizes from an object of class \code{cooccur}.
}
\usage{
effect.sizes(mod, standardized = TRUE, matrix = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mod}{
Object of class \code{cooccur}.}
  \item{standardized}{
Logical. Should the effect sizes be standardized?
}
  \item{matrix}{
Logical. Should the effect sizes be returned in a distance matrix? The alternative is a list of pairs.}
}
\value{
Returns either a data.frame of species pairs and effects sizes or a distance matrix with the rows and columns ordered from the order of species in the original community data matrix.
}
\author{
Daniel M Griffith}

\examples{
#data(finches)
#cooccur.finches <- cooccur(mat=finches,
#			   type="spp_site",
#			   thresh=FALSE,
#			   spp_names=TRUE)
#effect.sizes(cooccur.finches,matrix=TRUE)
}
\keyword{co-occurrence}
