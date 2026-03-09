\name{classCode}
\alias{classCode}
\title{
A function for displaying names of class codes of soil conditions in the soilassessment package
}
\description{
This function displays names of integer classes (or levels) of derived codes of soil conditions produced in the package
}
\usage{
classCode(value, indicator)
}
\arguments{
  \item{value}{
Integer value of the soil condition indicator
}
  \item{indicator}{
Soil condition whose class (x) is sought. The default = "fertility" if fertility is the soil condition
}
}
\details{
This is for interpretation of the integer codes of the soil conditions generated in the package
}
\value{
Name of the level of soil condition
}
\seealso{
\code{\link{classLUT}}, \code{\link{erodFUN}}, \code{\link{classnames}}
}
\examples{
classCode(2,"texture")
suitclas=classCode(4,"suitability")
levels(suitclas)
}
