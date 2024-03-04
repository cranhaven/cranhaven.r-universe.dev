\name{RISKInhal}
\alias{RISKInhal}

\title{
Risk for inhalation of vapors
}
\description{
Returns the risk (carcinogenic effects) for inhalation of vapors
}
\usage{
RISKInhal(URi, I)
}

\arguments{
  \item{URi}{
Inhalation Unit risk [(ug/m^3)^-1]
}
  \item{I}{
Inhalated dose (mg/m^3)
}

}

\value{
Risk [-]
}


\examples{
# Assess if there is cancer risk for the exposure of an adult 
# (Reasonable Maximum Exposure) to a benzene air concentration of 2 mg/ m^3 

RISKInhal ( I = AIR (CA = 2), URi = 7.8e-06)}

\keyword{ methods }
