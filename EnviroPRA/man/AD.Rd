\name{AD}
\alias{AD}
\title{
Dermal conctact with chemicals in soil
}
\description{
Estimates the Absorbed dose [mg/Kg*day] of chemicals through dermal contact with a soil 
}
\usage{
AD(CS = 1, SA = 2800, AF = 0.2, ABS = 0.001, EF = 350, ED = 24, BW = 70, AT = 365 * 70)
}

\arguments{
  \item{CS}{
Chemical concentration in soil [mg/Kg]
}
  \item{SA}{
Skin surface area available for contact [cm^2]
}
  \item{AF}{
Skin adherence factor [mg/cm^2]
}
  \item{ABS}{
Absorption factor (Chemical specific) [-]
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
Chemical Absorbed dose [mg/Kg*day]

}
\references{
US Environmental Protection Agency, 2011. Exposure Factors Handbook: 2011 Edition. U.S. Environmental Protection Agency, EPA/600/R-(September), pp 1466. 
}
\author{
F. Barrio-Parra
}

\examples{

## Estimated absorbed dose for the estimation of carcinogenic effects using 
# the default variables (EPA 2011) for a chemical soil concentration of 
# 0.2 mg/Kg

AD( CS=0.2)

# For a systemic effect:

AD( CS=0.2, AT=24*365)

# Specifying all the parameters for the carcinogenic case

AD( CS=0.2, SA=2300, AF=0.25, ABS=0.01, EF=150, ED=10, BW=80)
}
\keyword{ mehtods }