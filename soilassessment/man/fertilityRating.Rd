\name{fertilityRating}
\alias{fertilityRating}
\title{
A function for determining soil fertility levels for given soil property (fertility indicator)
}
\description{
This function determines the fertility levels given values of a soil property
}
\usage{
fertilityRating(value, indicator = "nitrogen")
}
\arguments{
  \item{value}{
numerical value of soil property
}
  \item{indicator}{
soil property as fertility indicator
}
}
\details{
The units for input values are: nitrogen (percent), phosphorus (mg/kg); potassium (cmol(+)/kg);carbon(percent);iron(mg/kg);zinc(mg/kg);manganese(mg/kg);boron(mg/kg);copper(mg/kg);sulfur(mg/kg); CEC(cmol(+)/kg)
}
\value{
soil fertility class code for the given soil property (fertility indicator)
}
\references{
FAO, 1976. A framework for land evaluation. FAO Soils Bulletin 32
Sanchez PA, Couto W, Buol SW. 1982. The fertility capability soil classification system: Interpretation, applicability, and modification

Sanchez PA, Palm CA, Buol SW. 2003. Fertility capability soil classification: a tool to help assess soil quality in the tropics. Geoderma 114, 157 –185.
}
\author{
Christian Thine Omuto
}
\seealso{
\code{\link{suitability}}, \code{\link{saltRating}}, \code{\link{fertilitySuit}}
}
\examples{
library(sp)
newmap=nutrindicator["iron"]
newmap$ironclass=fertilityRating(newmap$iron,"iron")
summary(newmap$iron)
spplot(newmap["ironclass"])
}
