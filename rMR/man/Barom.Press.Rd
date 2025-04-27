\name{Barom.Press}
\alias{Barom.Press}

\title{
Estimate Barometric Pressure
}
\description{
 Function to estimate barometric pressure based on altitude.
}
\usage{
Barom.Press(elevation.m, units = "atm")
}

\arguments{
  \item{elevation.m}{
Elevation in meters above sea level.
}
  \item{units}{
Output units for barometric pressure must be one of: \code{"atm"}, \code{"kpa"}, or \code{"mmHg"}.
}
}
\details{
This is just a simple conversion function. Plug and chug, as they say...
}
\value{
Returns numeric object of barometric pressure in specified units.
}
\references{
Mechtly, E. A., 1973: The International System of Units, Physical Constants and Conversion Factors. NASA SP-7012, Second Revision, National Aeronautics and Space Administration, Washington, D.C. \url{https://ntrs.nasa.gov/archive/nasa/casi.ntrs.nasa.gov/19730018242.pdf}.

U.S. Geological Survey (2011). Change to solubility equations for oxygen in water: Office of Water Quality Technical Memorandum 2011.03, accessed July 15, 2011, at \url{http://water.usgs.gov/admin/memo/QW/qw11.03.pdf}.
}
\author{
Tyler L. Moulton
}

\examples{
bar.pressure1 <- Barom.Press(elevation.m = 1000) # returns "atm"
bar.pressure2 <- Barom.Press(elevation.m = 1000, "kpa")
bar.pressure3 <- Barom.Press(elevation.m = 1000, "mmHg")
}
