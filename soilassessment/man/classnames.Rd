\name{classnames}
\alias{classnames}
\title{
A function to display class names and codes as used in the soilassessment package
}
\description{
This is a database function for displaying the class names and codes used in the soil assessment package
}
\usage{
classnames(indicator)
}
\arguments{
  \item{indicator}{
indicator of soil condition group of interest. Example: texture, suitability, drainage, fertility, erodibility
}
}
\value{
Table of soil condition code and name
}
\seealso{
\code{\link{classCode}}, \code{\link{classLUT}}
}
\examples{
x="texture"
classnames(x)
}
