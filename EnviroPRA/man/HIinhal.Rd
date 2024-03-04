\name{HIinhal}
\alias{HIinhal}

\title{
Hazard Index for inhalation of vapors
}
\description{
Returns the Hazard Index (systemic effects) for inhalation of vapors
}
\usage{
HIinhal(INH, RFC)
}

\arguments{
  \item{INH}{
Inhalated dose (mg/m^3)
}
  \item{RFC}{
Reference concentration (mg/m^3)
}
}

\value{
Hazard Index (non carcinogenic effects) [-]
}

\author{
F. Barrio-Parra
}

\seealso{
AIR {EnviroPRA}
}
\examples{

# Assess if there is systemic risk for the exposure of an adult 
# (Reasonable Maximum Exposure) to a Toluene air concentration of 2 mg/ m^3 

HIinhal (INH = AIR (CA = 2, AT = 365*24), RFC = 5)

}
\keyword{ methods }