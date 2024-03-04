\name{ADboot}
\alias{ADboot}

\title{
Dermal conctact with chemicals in soil by bootstrap
}
\description{
Dermal conctact with chemicals in soil by bootstrap
}
\usage{
ADboot(n, CS, SA, AF, ABS, EF, ED, BW, AT)
}

\arguments{
  \item{n}{
Output vector length
}
  \item{CS}{
Chemical concentrtion in soil [mg/Kg]
}
  \item{SA}{
Skin surface area available for contact [cm^2]
}
  \item{AF}{
Skin adherence factor [mg/cm^2]
}
  \item{ABS}{
Absorption factor (Chemical specific) [-]
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
Chemical Absorbed dose [mg/Kg*day]
}


\author{
F. Barrio-Parra
}



\examples{
# Carcinogenic effects

c <- rnorm( n= 10, mean = 0.2, sd = 0.05 )

b <- rnorm( n= 100, mean = 20, sd = 5 )

ADboot (n = 1000, SA=2300, AF=0.25, ABS=0.01,CS = c, BW = b, ED = 10, EF = 250)
  }