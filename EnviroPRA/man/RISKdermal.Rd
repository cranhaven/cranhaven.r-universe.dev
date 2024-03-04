\name{RISKdermal}
\alias{RISKdermal}

\title{
Risk for dermal contact
}
\description{
Returns the Risk for dermal exposure with chemicals (carcinogenic effects)
}
\usage{
RISKdermal(AD, SF, GI)
}

\arguments{
  \item{AD}{
Absorbed dose [mg/Kg*day]}
  \item{SF}{
Slope Factor [(mg/Kg*day)^-1] (chemical specific)
}
  \item{GI}{
Gastrointestinal Absorption factor (chemical specific) [-]
}
}

\value{
Risk [-]

}

\author{
F. Barrio-Parra
}
\seealso{
AD {EnviroPRA}
}
\examples{
# Assess if there is carcinogenic risk for an dadult thorug dermal 
# contact exposed to a soil that contains 45 mg/Kg of As in a residencial 
# scenario (default EPA Maximum Reasonable Exposure parameters)

SFAs = 1.5

# Dermal Absorption Factor

ABSAs = 3e-02

# Gastrointestinal Absorption Factor

GIAs = 1

I = AD (CS = 45,ABS = ABSAs)

RISKdermal (AD = I, SF = SFAs, GI = GIAs)}

\keyword{ methods }