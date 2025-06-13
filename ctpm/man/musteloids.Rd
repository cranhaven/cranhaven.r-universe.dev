\name{musteloids}
\alias{musteloids}
\encoding{UTF-8}
\docType{data}

\title{Musteloidea phylogeny.}

\description{A phylogenetic tree of Musteloidea.}

\usage{data("musteloids")}

\format{ An object of class "phylo", from package "ape".}

%\details{
%%  ~~ If necessary, more details than the __description__ above ~~
%}
%\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
%}
%\references{
%%  ~~ possibly secondary sources and usages ~~
%}

\references{
Law, Chris J. and Slater, Graham J. and Mehta, Rita S. (2017). Lineage Diversity and Size Disparity in Musteloidea: Testing Patterns of Adaptive Radiation Using Molecular and Fossil-Based Methods. Systematic Biology, 67(1), 127-144. \doi{10.1093/sysbio/syx047}.
}

\seealso{ \code{\link{moid_traits}}. }

\examples{
# Load package and tree
library(ctpm)
data("musteloids")

# Plot the phylogeny
plot(musteloids)
}

\keyword{datasets}
