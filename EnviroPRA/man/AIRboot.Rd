\name{AIRboot}
\alias{AIRboot}

\title{
Inhalation of airborne chemicals by bootstrap
}
\description{
Estimates the Intake rate by inhalation of airborne chemicals (vapor phase)  [mg/Kg*day]
}
\usage{
AIRboot(n, CA, IR, ET, EF, ED, BW, AT)
}

\arguments{
  \item{n}{
Output vector length
}
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

\author{
F. Barrio-Parra
}


\examples{
# Carcinogenic effects

c <- rnorm( n= 10, mean = 0.2, sd = 0.05 )

b <- rnorm( n= 100, mean = 20, sd = 5 )

AIRboot (n = 1000, CA=c, IR=25, ET = 24, EF = 300, ED = 24, BW = b)
}
\keyword{ methods }