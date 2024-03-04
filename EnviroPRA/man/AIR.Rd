\name{AIR}
\alias{AIR}

\title{
Inhalation of airborne chemicals
}
\description{
Estimates the Intake rate by inhalation of airborne chemicals (vapor phase)  [mg/Kg*day]
}
\usage{
AIR(CA = 1, IR = 20, ET = 24, EF = 350, ED = 24, BW = 70, AT = 365 * 70)
}

\arguments{
  \item{CA}{
Chemical concentrtion in air [mg/m^3]
}
  \item{IR}{
Inhalation Rate [m^3/hour]
}
  \item{ET}{
Exposure time [hours/day]
}
  \item{EF}{
Exposure frequency [day/yr]
}
  \item{ED}{
Exposure duration [yr]
}
  \item{BW}{
Body weight [Kg]
}
  \item{AT}{
Averaging time [day] (Note that for No carcinogenic effects AT should be equal to 365*ED)
}
}

\value{
Intake rate by inhalation of airborne chemicals (vapor phase) I [mg/Kg*day]
}
\references{
US Environmental Protection Agency, 2011. Exposure Factors Handbook: 2011 Edition. U.S. Environmental Protection Agency, EPA/600/R-(September), pp 1466.
}
\author{
F. Barrio-Parra
}

\examples{
## Estimated absorbed dose for the estimation of carcinogenic effects using 
# the default variables (EPA 2011) for a chemical air concentration 
# of 0.2 mg/m^3

AIR ( CA=0.2)

# For a systemic effect:

AIR ( CA=0.2, AT=24*365)

# Specifying all the parameters for the carcinogenic case

AIR ( CA=0.2, IR=25, ET = 24, EF = 300, ED = 24, BW = 85)
}
\keyword{ methods }