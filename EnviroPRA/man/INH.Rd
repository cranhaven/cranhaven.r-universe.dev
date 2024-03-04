\name{INH}
\alias{INH}

\title{
Inhalation of resuspended soil particles
}
\description{
Estimates the Intake rate of chemicals by inhalation of resuspended soil particles  [mg/Kg*day]
}
\usage{
INH(C = 10, EF = 350, ED = 24, PEF = 1.36^9, AT = 365 * ED)
}

\arguments{
  \item{C}{
Concentration of chemicals in soil(mg/kg)
}
  \item{EF}{
Exposure frequency (day/year)
}
  \item{ED}{
Exposure duration (years)
}
  \item{PEF}{
Particle emision factor meaning resuspended particles(m^3/kg)
}
  \item{AT}{
Averaging time [day] (Note that for No carcinogenic effects AT should be equal to 365*ED)
}
}

\value{
Chemical intake rate by inhalation of soil particles I [mg/Kg*day]
}
\references{
US Environmental Protection Agency, 2011. Exposure Factors Handbook: 2011 Edition. U.S. Environmental Protection Agency, EPA/600/R-(September), pp 1466.
}
\author{
F. Barrio-Parra
}

\examples{

# Estimated dose for the estimation of carcinogenic effects due to the 
# inhalation of soil particles  that contains 45 mg/Kg of As in a residencial 
# scenario (default EPA Maximum Reasonable Exposure parameters)

INH(C= 45, AT = 365*70)

# For non-carcinogenic effects:

INH(C= 45)
}
\keyword{ methods }
