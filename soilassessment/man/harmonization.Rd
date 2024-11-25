\name{harmonization}
\alias{harmonization}
\title{
Harmonization model for salt-affected soils
}
\description{
A generic model for harmonizing soil data for salt-affected soils.
}
\usage{
harmonization(x,A,B)
}
\arguments{
  \item{x}{
- is input data to harmonize such as electrical conductivity (ec) or ph.
}
  \item{A}{
 - is real number rate parameter or slope of the harmonization model
 }
  \item{B}{
- is real number constant (intercept) of the harmonization model
}
}
\details{
This is a generic linear model for harmonizing input soil data for assessing salt-affected soils.
}
\value{
a numeric output of harmonized ec or ph
}
\author{
Christian Thine Omto
}
\seealso{
\code{\link{ME_ECharmserve}}, \code{\link{ME_PHharmserve}}
}
\examples{
A = 1.08
B = 0.303
ec=2.45
harmonization(2.45,1.08,0.303)
}
