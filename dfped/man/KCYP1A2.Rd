\name{KCYP1A2}
\alias{KCYP1A2}

\title{
Fraction of adult CYP1A2 abundance according to age.
}

\description{
Compute the value of the fraction of adult CYP1A2 abundance according to the children age. It is described by a hyperbolic function.
}

\usage{
KCYP1A2(age)
}

\arguments{
  \item{age}{
The age of children.
}
}

\references{
    Petit, C., et al, (2016) Unified approach for extrapolation and bridging of adult information in early phase dose-finding paediatric studies, Statistical Methods in Medical Research, <doi:10.1177/0962280216671348>.

    Johnson T., Rostami-Hodjegan A. and Tucker G. (2006) Prediction of clearance of eleven drugs and associated variability in neonates, infants and children, Clin. Pharmacokinet., 45:931-956.
}

\author{ 
  Artemis Toumazi \email{artemis.toumazi@gmail.com},
  Caroline Petit \email{caroline.petit@crc.jussieu.fr}, 
  Sarah Zohar \email{sarah.zohar@inserm.fr}
}

\seealso{
\code{\link{KCYP2B6}}, \code{\link{KCYP2C8}}, \code{\link{KCYP2C9}}, \code{\link{KCYP2C18_19}}, \code{\link{KCYP2D6}}, \code{\link{KCYP2E1}}, \code{\link{KCYP3A4_5}}, \code{\link{KCYP3A}}
}

\examples{
age <- 1
KCYP1A2(age) 
}

