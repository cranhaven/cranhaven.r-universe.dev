\name{DO.unit.convert}
\alias{DO.unit.convert}

\title{
Convert Between Different Common Units of DO Concentration
}
\description{
Converts between different different units of DO concentration. Takes into account ambient temperature, pressure and salinity.
}
\usage{
DO.unit.convert(x, DO.units.in, DO.units.out, bar.units.in,
bar.press, temp.C, bar.units.out = "mmHg",
salinity = 0, salinity.units = "pp.thou")
}

\arguments{
  \item{x}{
    Value or object of class numeric to be converted.
}
  \item{DO.units.in}{
Units of dissolved oxygen concentration measured, i.e. to be converted from. Must be \code{"mg/L"}, \code{"PP"} (partial pressure), or \code{"pct"} (percent). If \code{"PP"}, the units of partial pressure must be equal to \code{bar.units.in}.
}
  \item{DO.units.out}{
    Units of dissolved oxygen concentration desired, i.e. to be converted to. Must be \code{"mg/L"}, \code{"PP"}, or {"pct"}.
}
  \item{bar.units.in}{
    Units of barometric pressure of user specified barometric pressure measurement. Must take value of \code{"atm"}, \code{"kpa"}, or \code{"mmHg"}.
}
  \item{bar.press}{
    Ambient barometric pressure measurement
}
  \item{temp.C}{
    Water temperature measured in degrees C
}
  \item{bar.units.out}{
    Used in internal calculation, only visible if output DO.units.out = \code{"PP"}. Must take value of \code{"atm"}, \code{"kpa"}, or \code{"mmHg"}.
}
  \item{salinity}{
    Salinity, either reported in parts per thousand (\code{"pp.thou"}) or microsiemens/cm (\code{"us"}) 
}
  \item{salinity.units}{
    Salinity units, must be \code{"pp.thou"} or \code{"us"} 
}
}

\value{
Numeric object representing dissolved oxygen concentration in the units specified by \code{DO.units.out}. 
}
\references{
Mechtly, E. A., 1973: The International System of Units, Physical Constants and Conversion Factors. NASA SP-7012, Second Revision, National Aeronautics and Space Administration, Washington, D.C. \url{https://ntrs.nasa.gov/archive/nasa/casi.ntrs.nasa.gov/19730018242.pdf}.

U.S. Geological Survey (2011). Change to solubility equations for oxygen in water: Office of Water Quality Technical Memorandum 2011.03, accessed July 15, 2011, at \url{http://water.usgs.gov/admin/memo/QW/qw11.03.pdf}.
}
\author{
Tyler L. Moulton
}
\note{
Use this function on entire data columns to convert them to desired units before analysing with functions like \code{MR.loops} and \code{get.pcrit}.
}



\seealso{
\code{\link{plot}}, 
\code{\link{plotRaw}}, 
\code{\link{cbind}}, 
\code{\link{Eq.Ox.conc}}, 
\code{\link{DO.saturation}}, 
}
\examples{
## on a single value ##

DO.pct<- DO.unit.convert(x= 125.6863, DO.units.in = "PP",
                DO.units.out = "pct",
                bar.units.in = "mmHg", bar.press = 750, temp.C =15)

## Apply to a column in a 'data.frame' class object ## 

## load data ##
data(fishMR)

## create time variable in POSIXct format ##
fishMR$std.time <- as.POSIXct(fishMR$Date.time,
                    format = "\%d/\%m/\%Y \%I:\%M:\%S \%p")

head(fishMR)

#note that DO data are in mg/L (DO.mgL) and
#that there is an instantaneous temperature column
#(temp.C) and a pressure column (Bar.Pressure.hpa)

DO.pct.col.a <- DO.unit.convert(fishMR$DO.mgL, DO.units.in = "mg/L",
                DO.units.out = "pct",
                bar.units.in = "kpa", bar.press = 101.3,
                temp.C = fishMR$temp.C,
                bar.units.out = "kpa")
DO.pct.col.b<- DO.unit.convert(fishMR$DO.mgL, DO.units.in = "mg/L",
                DO.units.out = "pct",
                bar.units.in = "kpa", bar.press = 101.3,
                temp.C = fishMR$temp.C)
head(DO.pct.col.a)
head(DO.pct.col.b)

# Now with df #

fishMR2 <- as.data.frame(cbind(fishMR, DO.pct.col.a))

par(mfrow = c(1,2))
plotRaw(data = fishMR, DO.var.name = "DO.mgL",
          start.time = "2015-07-03 06:15:00",
          end.time = "2015-07-03 08:05:00",
          main = "DO (mg/L) vs time",
          xlab = "time",
          ylab = "DO (mg/L)")
          

plotRaw(data = fishMR2, DO.var.name = "DO.pct.col.a",
          start.time = "2015-07-03 06:15:00",
          end.time = "2015-07-03 08:05:00",
          main = "DO (percent saturation) vs time",
          xlab = "time",
          ylab = "DO (percent saturation)")
}
