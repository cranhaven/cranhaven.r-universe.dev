\name{kickoffControl}
\alias{kickoffControl}

\title{
Control for presence of at least toxicities and efficacies for the good run of bCRM model.
}

\description{
An algorithm that control if we have at least one 0 and one 1 for both efficacy and toxicity.
}  

\usage{
kickoffControl(tox, currentDose, cohortSize, nbDoses)
}

\arguments{
	\item{tox}{
		The vector of toxicity outcomes.  
	}
	\item{currentDose}{
		The current dose of a patient. 
	}
	\item{cohortSize}{
		The size of the cohort; must be integer. 
	}
	\item{nbDoses}{
		The maximum number of the doses. 
	}    
} 

\references{
Petit, C., et al, (2016) Unified approach for extrapolation and bridging of adult information in early phase dose-finding paediatric
studies, Statistical Methods in Medical Research, <doi:10.1177/0962280216671348>. 
}

\author{ 
	Artemis Toumazi \email{artemis.toumazi@gmail.com},
	Caroline Petit \email{caroline.petit@crc.jussieu.fr},
	Sarah Zohar \email{sarah.zohar@inserm.fr}
} 


\examples{
\dontrun{
	tox <- c(0.1301477, 0.2774171, 0.4184642, 0.6486846, 0.8257219)
	currentDose <- 3
	cohortSize <- 1
	nbDoses <- 5 
	kickoffControl(tox, currentDose, cohortSize, nbDoses)
}
}


