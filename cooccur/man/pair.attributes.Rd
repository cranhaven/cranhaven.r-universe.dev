\name{pair.attributes}
\alias{pair.attributes}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to create a species-wise summary of co-occurrence patterns.}
\description{
Summarizes the positive, negative, and random interactions for each species in an cooccur analysis.
}
\usage{
pair.attributes(mod)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mod}{
Object of class \code{cooccur}.}
}

\value{
Returns a data.frame with the percentage of "pos", "neg", "rand" associations that each species ("spp") participates in. Columns with the prefix "num_" are counts.
}
\author{
Daniel M. Griffith}

\examples{
#data(finches)
#cooccur.finches <- cooccur(mat=finches,
#			   type="spp_site",
#			   thresh=TRUE,
#			   spp_names=TRUE)
#pair.attributes(cooccur.finches)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{co-occurrence}
