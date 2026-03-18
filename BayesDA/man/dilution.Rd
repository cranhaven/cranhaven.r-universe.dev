\name{dilution}
\alias{dilution}
\docType{data}
\title{ Serial Dilution Assay  }
\description{
  A serial dilution assay to study concentration of cockroach allergen 
  in homes of asthma sufferes.
}
\usage{data(dilution)}
\format{
  The format is:
List of 4
 \$ unknowns     : num [1:8, 1:10] matrix with optical densities for unknowns in each
                                  column
 \$ standards    : num [1:16] vector of standard solution optical densities
 \$ dil.unknowns : num [1:8] dilution factor for unknowns
 \$ dil.standards: num [1:16] dilution factor for standard solution
}
\details{
  Concentration of standard was 0.64
}
\examples{
data(dilution)
str(dilution)
unknowns <- dilution$unknowns
standards <- dilution$standards
dil.unknowns <- dilution$dil.unknowns
dil.standards <- dilution$dil.standards
plot(dil.standards, standards)
matplot(dil.unknowns, unknowns, type="b")
}
\keyword{datasets}
