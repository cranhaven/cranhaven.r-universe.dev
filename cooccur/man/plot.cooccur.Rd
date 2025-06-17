\name{plot.cooccur}
\alias{plot.cooccur}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function for producing a heatmap co-occurrence visualization.
}
\description{
Heatmap visualization of the pairwise species associations revealed by a cooccur analysis.}
\usage{
\method{plot}{cooccur}(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Object of class \code{cooccur}.}
\item{...}{Additional args}
}

\author{
Daniel M Griffith}

\seealso{
\code{\link{obs.v.exp}},\code{\link{pair.profile}}
}
\examples{
#data(finches)
#cooccur.finches <- cooccur(mat=finches,
#			   type="spp_site",
#			   thresh=TRUE,
#			   spp_names=TRUE)
#plot(cooccur.finches)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Graphics}
