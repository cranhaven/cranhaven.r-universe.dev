\name{PLANTS}
\alias{PLANTS}
\docType{data}
\title{USGS PLANTS database data}
\description{
  This data set includes county-level occupancy for native and exotic plants
  in 47 US states
}
\usage{data(PLANTS)}
\format{
  \code{List} containing a \code{data.frame} for the native species in
  each state and a second \code{data.frame} for
  the exotic species in each state.  Each data frame is organized with
  counties as columns and species as rows.
}
\details{
  Per the USDA PLANTS Database website,
  species were listed as "native" if their L48 native status was listed
  as N, N?, NI, or NI? and "exotic" if their L48 native status was listed
  as GP, GP?, I, I?, N?I, W, or W?.  See http://plants.usda.gov/about_adv_search.html
  for descriptions of these terms.
  
  The USDA PLANTS Database does not include county-level data on Alaska
  or Maryland.  Hawaii was excluded from this data set because L48 native
  status did not apply to it.
}
\source{
  The USDA PLANTS Database can be found at http://plants.usda.gov/
  }
\references{
  USDA, NRCS. 2010. The PLANTS Database (http://plants.usda.gov, November 23, 2010). National Plant Data Center, Baton Rouge, LA 70874-4490 USA.

  Used as an example in Harris et al. 2011. "Occupancy is nine-tenths of the law"
  The American Naturalist. 
}
\keyword{datasets}