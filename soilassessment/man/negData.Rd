\name{negData}
\alias{negData}
\title{
Correcting negative entries in classes for intenisty of salt-affected soils
}
\description{
Function to handle negative entries when assessing salt-affected soils
}
\usage{
negData(vg,x)
}
\arguments{
  \item{vg}{
tag for soil property. Default is "ec"
}
  \item{x}{
numeric value of soil property to check
}
}
\details{
Three tags for soil properties are allowed: "ec", "ph", "esp"
}
\value{
numeric value of soil property to correct. It return NA where negative "ec" or "esp" is involved or where ph<1 or ph>14
}
\author{
Christian Thine Omuto
}
\examples{
negData("ph",14)
}
