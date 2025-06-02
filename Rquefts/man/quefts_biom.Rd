\name{quefts_biom}


\alias{quefts_biom}

\title{
biomass parameters
}

\description{
Crop biomass parameters

For a cereal crop you can generally assume that about 50\% of the total biomass is grain, and about 30\% is stem and 20\% is leaf biomass.
}

\usage{
quefts_biom()
}


\details{
\tabular{lll}{
	\tab \bold{Crop yield (biom)} \tab . \cr
	\tab \code{leaf_att, stem_att, store_att} \tab Attainable (in the absence of nutrient limitation), or target crop biomass (dry-matter, kg/ha) for leaves, stems and storage organs.\cr
	\tab \code{SeasonLength} \tab Length of the growing season (days)\cr
}
}


\value{
list
}


\examples{
b <- quefts_biom()
str(b)
}
