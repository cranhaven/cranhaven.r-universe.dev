\name{print.cooccur}
\alias{print.cooccur}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Print significant pairwise species results.
}
\description{
Returns a table of analysis results for all significant pairwise interactions found in a \code{cooccur} object.}
\usage{
\method{print}{cooccur}(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
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
#print(cooccur.finches)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{co-occurrence}
