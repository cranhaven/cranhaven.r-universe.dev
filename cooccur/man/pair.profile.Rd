\name{pair.profile}
\alias{pair.profile}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to produce a visualization of species contributions to co-occurrence patterns.
}
\description{
Plots a bar plot for visualizing the associations of each individual species from a \code{cooccur} object.}
\usage{
pair.profile(mod)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mod}{
Object of class \code{cooccur}.}
}

\author{
Daniel M Griffith}

\seealso{
\code{\link{obs.v.exp}},\code{\link{plot.cooccur}}
}
\examples{
#data(finches)
#cooccur.finches <- cooccur(mat=finches,
#			   type="spp_site",
#			   thresh=TRUE,
#			   spp_names=TRUE)
#pair.profile(cooccur.finches)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Graphics}
