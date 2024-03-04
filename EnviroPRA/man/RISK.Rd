\name{RISK}
\alias{RISK}

\title{
Risk
}
\description{
Returns the Risk estimation (carcinogenic effects)
}
\usage{
RISK(I, SF)
}

\arguments{
  \item{I}{
Intake Rate [mg/Kg*day]
}
  \item{SF}{
Slope Factor [(mg/Kg*day)^-1] (chemical specific)
}
}

\value{
Risk [-]
}

\author{
F. Barrio-Parra}


\examples{
# Assessing if there is carcinogenic risk for an adult receptor that drinks water with 1000 ug/L 
# of hexaclorobence (Oral Slope Factor (IRIS data base) = 1.6 [mg/Kg*day]^-1) in a residencial 
# scenario (default EPA Maximum Reasonable Exposure parameters)


RISK (I = DWIR( CW=1), SF = 1.6) 
}
\keyword{ methods }