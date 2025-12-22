\name{occurrences}
\alias{assemblage}
\alias{occurrences}

\title{
Access functions to the community matrix of \code{distrib_data} objects
}
\description{
Gives the species in a site, or the sites occupied by a species, in a \code{distrib_data} object
}
\usage{
occurrences(distrib_data, species, value = c("index", "names", "logical", "raw"))
assemblage(distrib_data, site, value = c("index", "names", "logical", "raw"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{distrib_data}{
An object of type \code{distrib_data}, \code{nodiv_data} or \code{nodiv_result}
}
  \item{species}{
The number or name of a species in the data set
}
  \item{site}{
The number or name of a site in the data set
}
  \item{value}{
The type of data to return, see below.
}
}

\value{
\code{type = "01"} gives a vector of 0s and 1s from the community matrix
\code{type = "logical"} gives a logical vector identifying species or sites
\code{type = "index"} gives the index number of species or sites in the community matrix
}

\author{
Michael Krabbe Borregaard
}

