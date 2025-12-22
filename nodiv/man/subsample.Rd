\name{subsample}
\alias{subsample}
\alias{subsample.distrib_data}
\alias{subsample.nodiv_data}
\alias{subsample.nodiv_result}

\title{
Subsampling data objects to certain species or sites 
}
\description{
Takes an object of type \code{distrib_data} or \code{nodiv_data}, and subsamples it to contain certain species or sites, while keeping the integrity of the data.
}
\usage{
subsample(x, ...)
\method{subsample}{distrib_data}(x, sites = NULL, species = NULL, ...)
\method{subsample}{nodiv_data}(x, sites = NULL, species = NULL, node = NULL, ...)
\method{subsample}{nodiv_result}(x, node = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
An object of type \code{distrib_data} or \code{nodiv_data} 
}
  \item{sites}{
A numeric or character vector identifying the sites to keep in the new object. If specified, the function will subsample the data object to only include the defined sites, and the species that exist there. If sites == \"all\", all sites will be kept when subsampling for species, even if no species now exist in the sites. 
}
  \item{species}{
A numeric or character vector identifying the species to keep in the new object. If specified, the function will subsample the data object to only include the defined species, and the sites where they exist. 
}
  \item{node}{
A numeric or character vector identifying a node in the phylogeny. Only species descending from this node will be kept in the new object, and the sites where they exist.
}
  \item{\dots}{
Further arguments to the subsample function
}
}

\value{
The return value is a new object of the same type as \code{x}
}

\author{
Michael Krabbe Borregaard
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{distrib_data}}, ~~~
}
\examples{
## Not run:
data(coquettes)
new <- subsample(coquettes, sites = which(richness(coquettes) > 3))
plot(new, show.tip = FALSE)
## End(Not run)
}

