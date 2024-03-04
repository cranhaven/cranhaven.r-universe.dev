\name{SIR}
\alias{SIR}

\title{
Chemical intake by accidental soil ingestion
}
\description{
Estimates the chemical Intake rate by accidental soil ingestion [mg/Kg*day]
}
\usage{
SIR(CS = 1, IR = 100, FI = 1, EF = 350, ED = 24, BW = 80, AT = 365 * 70)
}

\arguments{
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
\references{
US Environmental Protection Agency, 2011. Exposure Factors Handbook: 2011 Edition. U.S. Environmental Protection Agency, EPA/600/R-(September), pp 1466.
}
\author{
F. Barrio-Parra}


\examples{

# Ingestion rate for a children weighing 20 Kg who ingest 200 mg 
# of soil every day, 250 days per year during 10 years. 95-UCL of
# Arsenic in soil is 25 mg/Kg

# Carcinogenic effects

SIR ( CS = 25, BW = 20, IR = 200, ED = 10, EF = 250)

# Systemic effects

SIR ( CS = 25, BW = 20, IR = 200, ED = 10, EF = 250, AT = 365*10)
}

\keyword{ methods }