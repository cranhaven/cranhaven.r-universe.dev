\name{pair}
\alias{pair}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to examine co-occurrence patterns for an individual species.
}
\description{
Extracts results for a single species from a \code{cooccur} object.
}
\usage{
pair(mod, spp, all = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mod}{
Object of class \code{cooccur}.}
  \item{spp}{
Name of the species, or the number assigned to it.}
  \item{all}{
Logical. If TRUE then all, not just significant, results are returned.
}
}

\value{
Same as \code{prob.table()} but for only one species.
}

\author{
Daniel M Griffith}

\seealso{
\code{\link{prob.table}}
}
\examples{
#data(finches)
#cooccur.finches <- cooccur(mat=finches,
#			   type="spp_site",
#			   thresh=TRUE,
#			   spp_names=TRUE)
#pair(cooccur.finches,"Geospiza fortis",all=TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{co-occurrence}
