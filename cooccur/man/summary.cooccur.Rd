\name{summary.cooccur}
\alias{summary.cooccur}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Method to summarize co-occurrence patterns.}
\description{
Presents a count of positive, negative, random, and unclassified pairwise comparisons from a \code{cooccurr} object.
}
\usage{
\method{summary}{cooccur}(object, ...)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
Object of class \code{cooccur}.}
\item{...}{Additional args}
}


\author{
Daniel M Griffith}

\examples{
#data(finches)
#cooccur.finches <- cooccur(mat=finches,
#			   type="spp_site",
#			   thresh=TRUE,
#			   spp_names=TRUE)
#summary(cooccur.finches)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{co-occurrence}
