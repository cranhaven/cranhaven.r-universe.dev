\name{HIdermal}
\alias{HIdermal}

\title{
Hazard Index for dermal contact
}
\description{
Returns the Hazard Index for dermal exposure with chemicals (non carcinogenic effects)
}
\usage{
HIdermal(AD, RFD, GI)
}

\arguments{
  \item{AD}{
Absorbed dose [mg/Kg*day]
}
  \item{RFD}{
Reference dose [mg/Kg*day]
}
  \item{GI}{
Gastrointestinal Absorption factor (chemical specific) [-]
}
}

\value{
Hazard Index [-]
}

\author{
F. Barrio-Parra
}

\seealso{
AD {EnviroPRA}
}
\examples{

# Assess if there is non-carcinogenic risk for an dadult thorug dermal 
# contact exposed to a soil that contains 45 mg/Kg of As in a residencial 
# scenario (default EPA Maximum Reasonable Exposure parameters)

RfDAs = 3e-04

# Dermal Absorption Factor

ABSAs = 3e-02

# Gastrointestinal Absorption Factor

GIAs = 1

I = AD (CS = 45,ABS = ABSAs, AT= 24*365)

HIdermal (AD = I, RFD = RfDAs, GI = GIAs)
}

\keyword{ methods }
