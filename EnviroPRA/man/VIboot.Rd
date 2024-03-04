\name{VIboot}
\alias{VIboot}

\title{
Chemical intake by ingestion of vegetables by bootstrap
}
\description{
Estimates the chemical Intake rate by ingestion of contaminated fruits and vegetables [mg/Kg*day]
}
\usage{
VIboot( n, CF, IR, FI, EF, ED, BW, AT)
}

\arguments{
  \item{n}{
Output vector length
}
  \item{CF}{
Chemical concentrtion in food [mg/Kg]
}
  \item{IR}{
Vegetables Ingestion Rate [g / Kg * Day]
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
Body Weight [Kg]
}
  \item{AT}{
Averaging time [day] (For No carcinogenic effects AT = 365*ED)
}
}

\value{
A vector of Chemical intake rate by vegetable ingestion I [mg/Kg*day]
}

\examples{

# Assess the chemical intake by an adult that eats lettuce with a concentration of 2 mg/ Kg of a 
# chemical with non- carcinogenic effects in a maximum reasonable exposure scenario
# Figure out 10 data of Chemical concentration following a normal distribution (mean = 2, sd= 2)
# and 100 Body weight data that follow a normal distribution (mean = 70, sd = 15)

c <- rnorm( n= 10, mean = 2, sd = 2 )

b <- rnorm( n= 100, mean = 70, sd = 5 )

VIboot (n = 1000, CF = c, BW = b, AT = 365*24)
}

\keyword{ methods}