\name{saltClass}
\alias{saltClass}
\title{
A function to classify types of salt-affected soils using EC, PH, and ESP
}
\description{
This function determines the major classes of salt-affected soils using Electrical Conductivity (EC), soil reaction (pH), and Exchangeable Sodium Percent (ESP) according to FAO or USDA classification schemes
}
\usage{
saltClass(ec,ph,esp)
}
\arguments{
  \item{ec}{
Electrical Conductivity in dS/m of saturated soil paste extract or its equivalent
}
  \item{ph}{
soil reaction (pH)
}
  \item{esp}{
Exchangeable Sodium Percent
}
}
\value{
saltClass returns integer classes of salt problems in the soil. The classes are 1, 2, 3, 4, 5 corresponding to None, Saline, Saline-sodic, Sodic, and Alkaline categories.
}
\references{
FAO.2006. Guidelines for soil description. FAO. Rome

Richards, L. A. (ed.) 1954. Diagnosis and Improvement of Saline and Alkali Soils. U.S. Department Agriculture Handbook 60. U.S. Gov. Printing Office, Washington, DC.
}
\author{
Christian Thine Omuto
}
\note{
ESP is mandatory when using this function. The "error: 1 * ESP : non-numeric argument to binary operator" is flagged when ESP entry is missing. In case ESP is missing, saltRating is suggested.
}
\seealso{
\code{\link{saltRating}}, \code{\link{saltSeverity}}, \code{\link{classCode}}
}
\examples{
saltClass(6.12,7.84,1)
}
