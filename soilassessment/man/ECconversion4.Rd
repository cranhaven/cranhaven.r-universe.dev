\name{ECconversion4}
\alias{ECconversion4}
\title{
A function for harmonizing salt measurements into equivalent electrical conductivity in dS/m
}
\description{
This function allows approximate conversion of other soil salt measurements into equivalent electrical conductivity (EC) in dS/m. These measurements include total soluble salts (TSS), total dissolved solids (TDS) and EC in mmho/cm
}
\usage{
ECconversion4(x,target)
}
\arguments{
  \item{x}{
is a numeric value of salt to convert to equivalent EC in dS/m
}
  \item{target}{
the target salt measurement to be converted into equivalent electrical conductivity (EC) in dS/m. It can be TDS (mg/l or ppm), TSS (mmol/l), EC in (mmho/cm)
}
}
\details{
The target is specified as TDS or TSS or mmho.
}
\value{
The output is a numeric value of equivalent electrical conductivity (EC) in dS/m
}
\author{
Christian Thine Omuto
}
\note{
TDS should be given in mg/l or ppm. TSS should be given in mmol/l. The function does not convert salt values between different measurement methods
}
\seealso{
\code{\link{ECconversion1}}, \code{\link{ECconversion2}}, \code{\link{pedoTransfer}}
}
\examples{
ECconversion4(200,"TSS")
ECconversion4(20,"TDS")
ECconversion4(2,"mmho")
}
