\name{obs.v.exp}
\alias{obs.v.exp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to plot a visualization of the observed versus expected oc-occurrences from an analysis stored in a \code{coccurr} object.}
\description{
Plot the observed number of co-occurrences versus the number expected from the probability analysis in a cooccur object.
}
\usage{
obs.v.exp(mod)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mod}{
Object of class \code{cooccur}.}
}
\author{
Daniel M Griffith
}
\seealso{
\code{\link{pair.profile}},\code{\link{plot.cooccur}}
}
\examples{
#data(finches)
#cooccur.finches <- cooccur(mat=finches,
#			   type="spp_site",
#			   thresh=TRUE,
#			   spp_names=TRUE)
#obs.v.exp(cooccur.finches)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Graphics}
