\name{prob.table}
\alias{prob.table}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to extract the probability table from an analysis of species co-occurrence.
}
\description{
Returns a results table for all analyzed species pairs in a \code{cooccur} object.}
\usage{
prob.table(mod)
}
\arguments{
  \item{mod}{
Object of class \code{cooccur}.}
}
\value{
Returns a data.frame with the following columns.
\item{sp1}{Numeric label giving the identity of species 1, assigned based on the order in the input matrix}
\item{sp2}{Numeric label for species 2}
\item{sp1_inc}{Number of sites (or samples) that have species 1}
\item{sp2_inc}{Number of sites that have species 2}
\item{obs}{cooccur Observed number of sites having both species}
\item{prob}{cooccur Probability that both species occur at a site}
\item{exp}{cooccur Expected number of sites having both species}
\item{p_lt}{Probability that the two species would co-occur at a frequency less than the observed number of co-occurrence sites if the two species were distributed randomly (independently) of one another}
\item{p_gt}{Probability of co-occurrence at a frequency greater than the observed frequency}
\item{sp1_name}{If species names were specified in the community data matrix this field will contain the supplied name of sp1}
\item{sp2_name}{The supplied name of sp2}
}

\author{
Daniel M Griffith}


\seealso{
\code{\link{pair}}, \code{\link{pair.attributes}}
}
\examples{
#data(finches)
#cooccur.finches <- cooccur(mat=finches,
#			   type="spp_site",
#			   thresh=TRUE,
#			   spp_names=TRUE)
#prob.table(cooccur.finches)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{co-occurrence}
