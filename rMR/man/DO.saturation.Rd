\name{DO.saturation}
\alias{DO.saturation}

\title{
  Calculate Oxygen Saturation of Water
}
\description{
Calculate the percent saturation of oxygen in water given external temperature, barometric pressure, and recorded DO concentration in mg/L.
}
\usage{
DO.saturation(DO.mgl, temp.C,
elevation.m = NULL, bar.press = NULL,
bar.units = NULL,
salinity, salinity.units)
}
\arguments{
  \item{DO.mgl}{
Recorded DO concentration in mg/L.
}
  \item{temp.C}{
Temperature in degrees C.
}
  \item{elevation.m}{
Elevation in meters above sea level. EITHER \code{elevation.m} or \code{bar.press} must be specified.
}
  \item{bar.press}{
Barometric pressure in user defined units (bar.units)--defaults to \code{NULL}. EITHER \code{elevation.m} or \code{bar.press} must be specified.
}
  \item{bar.units}{
Units of barometric pressure, defaults to \code{NULL}. must be \code{"atm"}, \code{"kpa"} or \code{"mmHg"}
}
  \item{salinity}{
    Salinity, either reported in parts per thousand (\code{"pp.thou"}) or microsiemens/cm (\code{"us"}). 
}
  \item{salinity.units}{
    Salinity units, must be \code{"pp.thou"} or \code{"us"} 
}
}

    \value{
Returns numeric value of dissolved oxygen saturation.
}
    \references{
Mechtly, E. A., 1973: The International System of Units, Physical Constants and Conversion Factors. NASA SP-7012, Second Revision, National Aeronautics and Space Administration, Washington, D.C. \url{https://ntrs.nasa.gov/archive/nasa/casi.ntrs.nasa.gov/19730018242.pdf}.

U.S. Geological Survey (2011). Change to solubility equations for oxygen in water: Office of Water Quality Technical Memorandum 2011.03, accessed July 15, 2011, at \url{http://water.usgs.gov/admin/memo/QW/qw11.03.pdf}.
}
    \author{
Tyler L. Moulton
}


    \seealso{
    \code{\link{Eq.Ox.conc}},
    \code{\link{DO.unit.convert}}, 
}
    \examples{
DO.sat1 <- DO.saturation(DO.mgl = 5.5,
temp.C = 20, elevation.m = 1000)

DO.sat2 <- DO.saturation(DO.mgl = 5.5,
temp.C = 20, bar.press = 674.1, bar.units = "mmHg")

DO.sat1
DO.sat2

# Will ya look at that...
}