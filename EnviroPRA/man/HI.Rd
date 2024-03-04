\name{HI}
\alias{HI}

\title{
Hazard Index
}
\description{
Returns the Hazard Index (non carcinogenic effects)
}
\usage{
HI(I, RFD)
}

\arguments{
  \item{I}{
Intake Rate [mg/Kg*day]
}
  \item{RFD}{
Reference dose [mg/Kg*day]
}
}

\value{
Hazard Index [-]
}

\author{
F. Barrio-Parra
}

\examples{

# Assessing if there is systemic risk for an adult receptor that drinks water with 1000 ug/L 
# of hexaclorobence (Reference Dose (IRIS data base) = 8e-04 [mg/Kg*day]) in a residencial 
# scenario (default EPA Maximum Reasonable Exposure parameters)


HI (I = DWIR( CW=1, AT=24*365), RFD = 8e-04) 
}
\keyword{ methods }