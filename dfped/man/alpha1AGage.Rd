\name{alpha1AGage}
\alias{alpha1AGage}

\title{
Concentration of alpha1-acid glycoprotein according to age.
}

\description{
Compute the value of alpha1-acid glycoprotein (alpha1AG) concentration (g/L) according to age (year) for children.
}

\usage{
alpha1AGage(age)
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
	\code{\link{concCh}}, \code{\link{albAge}}
}

\examples{
age <- 2
alpha1AGage(age)
}


