\name{species_stat}
\alias{add_species_stat}
\alias{species_stat}
\title{
Species statistics of \code{nodiv} data objects 
}
\description{
Adds, or reads species statistics from objects in the \code{nodiv} package
}
\usage{
add_species_stat(distrib_data, species_stat, specs = NULL)
species_stat(distrib_data, statname = NULL, specs = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{distrib_data}{
An object of type \code{distrib_data}, \code{nodiv_data} or \code{nodiv_result}
}
  \item{species_stat}{
A species statistic, such as body size or cold tolerance. This can be a vector or a data.frame. If 
\code{specs} is not specified, the function tries to match \code{species_stat} to the site names of the 
\code{distrib_data} object. The function looks for vector names, rownames, or variables in the data.frame that match the species names. If species names are absent and the number of species_stats match, the species_stat is assumed to be in the correct order. 
}
  \item{specs}{
A character or numeric variable specifying the species of interest, or for which species_stat is specified
}
  \item{statname}{
The name of a species statistic in the \code{distrib_data} object. If statname is not given, a list of
names of species statistics is given
}
}

\value{
\code{add_species_stat} returns the object with the new species_stat inside. 
\code{species_stat} returns the named sitestatistic.
}

\author{
Michael Krabbe Borregaard
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{distrib_data}}
}

