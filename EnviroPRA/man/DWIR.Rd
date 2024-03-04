\name{DWIR}
\alias{DWIR}

\title{
Chemical intake by Drinking Water
}
\description{
Estimates the chemical Intake rate by Drinking Water [mg/Kg*day]
}
\usage{
DWIR(CW = 1, IRW = 2, EF = 350, ED = 24, BW = 80, AT = 365 * 70)
}

\arguments{
  \item{CW}{
Chemical concentrtion in water [mg/L]
}
  \item{IRW}{
Water Ingestion Rate [L/Day]
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
Chemical intake rate by drinking water I [mg/Kg*day]
}
\references{
US Environmental Protection Agency, 2011. Exposure Factors Handbook: 2011 Edition. U.S. Environmental Protection Agency, EPA/600/R-(September), pp 1466.
}
\author{
F. Barrio-Parra
}

\examples{
# Estimate the dermal absorbed dose during swiming in waters with a carcinogenic chemical
# (water concentration of 250 mg/m^3)

DWIR ( CW = 250)

# For a systemic effect:

DWIR ( CW= 250, AT=24*365)

# Specifying all the parameters for the carcinogenic case

DWIR ( CW=250, IR=1.5, EF = 300, ED = 24, BW = 85)
}
\keyword{ methods }