\name{VI}
\alias{VI}

\title{
Chemical intake by ingestion of vegetables
}
\description{
Estimates the chemical Intake rate by ingestion of contaminated fruits and vegetables [mg/Kg*day]
}
\usage{
VI(CF = 1, IR = 210, FI = 1, EF = 350, ED = 24, BW = 80, AT = 365 * 70)
}

\arguments{
  \item{CF}{
Chemical concentration in food [mg/Kg]
}
  \item{IR}{
Vegetables Ingestion Rate [g / Kg * Day]}
  \item{FI}{
Fraction ingested from contaminated source [-]
}
  \item{EF}{
Exposure frequency [day/yr]}
  \item{ED}{
Exposure duration [yr]}
  \item{BW}{
Body weight (kg)}
  \item{AT}{
Averaging time [day] (For No carcinogenic effects AT = 365*ED)}
}

\value{
Chemical intake rate by vegetable ingestion I [mg/Kg*day]

}
\references{
US Environmental Protection Agency, 2011. Exposure Factors Handbook: 2011 Edition. U.S. Environmental Protection Agency, EPA/600/R-(September), pp 1466.
}
\author{
F. Barrio-Parra}



\examples{

# Assess the chemical intake by an adult that eats lettuce with a concentration of 2 mg/ Kg 
# in a maximum reasonable exposure scenario for non- carcinogenic effects

VI (CF = 2, AT = 365*24)
}
\keyword{ methods }