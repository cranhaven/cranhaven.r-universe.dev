\name{weightCYPsum}
\alias{weightCYPsum}

\title{
Proportion of the molecule metabolised by the CYPs for a child according to age.
}

\description{
Proportion of the molecule metabolised by the CYPs. A weighted sum is computed. For each CYP, the proportion metabolised in adults is multiplied with the fraction of CYP (KCYP) available for a child according to age. 
} 

\usage{
weightCYPsum(age, percCYP)
}

\arguments{
    \item{age}{
		The age of child.
    }
    \item{percCYP}{
        Dataframe giving the percentage of the molecule metabolised for each cytochrome in adults. Dataframe with two column - column 1: CYP name, column 2: percentage of the molecule metabolised.
    }
}

\references{ 
    Petit, C., et al, (2016) Unified approach for extrapolation and bridging of adult information in early phase dose-finding paediatric studies, Statistical Methods in Medical Research, <doi:10.1177/0962280216671348>.
} 

\author{ 
    Artemis Toumazi \email{artemis.toumazi@gmail.com},
    Caroline Petit \email{caroline.petit@crc.jussieu.fr}, 
    Sarah Zohar \email{sarah.zohar@inserm.fr} 
} 

\seealso{
    \code{\link{Clchu}}, \code{\link{Fch}}
}

\examples{
age <- 2
perc_CYP <- data.frame("CYP3A4_5" = 0.7, "CYP1A2" = 0.3)
weightCYPsum(age, perc_CYP)
}


