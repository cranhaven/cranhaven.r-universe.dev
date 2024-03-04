\name{SIRboot}
\alias{SIRboot}

\title{
Chemical intake by accidental soil ingestion by bootstrap
}
\description{
Estimates the chemical Intake rate by accidental soil ingestion [mg/Kg*day]}
\usage{
SIRboot(n, CS, IR, FI, EF, ED, BW, AT)
}

\arguments{
  \item{n}{
Output vector length
}
  \item{CS}{
Chemical concentrtion in soil [mg/Kg]
}
  \item{IR}{
Soil Ingestion Rate [mg/Day]
}
  \item{FI}{
Fraction ingested from contaminated source [-]
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
Chemical intake rate by soil ingestion I [mg/Kg*day]
}

\examples{

# Carcinogenic effects

c <- rnorm( n= 10, mean = 22, sd = 2 )

b <- rnorm( n= 100, mean = 20, sd = 5 )

SIRboot (n = 1000, CS = c, BW = b, IR = 200, ED = 10, EF = 250)
}

\keyword{ methods }