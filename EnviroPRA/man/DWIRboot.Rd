\name{DWIRboot}
\alias{DWIRboot}

\title{
Chemical intake by Drinking Water by bootstrap
}
\description{
Estimates the chemical Intake rate by Drinking Water [mg/Kg*day]
}
\usage{
DWIRboot(n, CW, IRW, EF, BW, ED, AT)
}

\arguments{
  \item{n}{
Output vector length
}
   \item{CW}{
Chemical concentrtion in water [mg/L]
}
  \item{IRW}{
Water Ingestion Rate [L/Day]
}
  \item{EF}{
Exposure frequency [day/yr]
}
  \item{BW}{
Body weight [Kg]
}
  \item{ED}{
Exposure duration [yr]
}
  \item{AT}{
Averaging time [day] (Note that for No carcinogenic effects AT should be equal to 365*ED)
}
}

\value{
Chemical intake rate by drinking water I [mg/Kg*day]
}

\author{
F. Barrio-Parra
}


\examples{
# Carcinogenic effects

c <- rnorm( n= 10, mean = 250, sd = 15 )

b <- rnorm( n= 100, mean = 20, sd = 5 )

DWIRboot (n = 1000, CW=c, IR=1.5, EF = 300, ED = 24, BW = b)
}

\keyword{ methods }
