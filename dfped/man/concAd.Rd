\name{concAd}
\alias{concAd}

\title{
Concentration of a specific molecule in plasma for the adult population. 
}

\description{
Compute the concentration of a specific molecule in plasma for the adult population according to the percentage binding with albumin and alpha1-acid glycoprotein. 
} 

\usage{
concAd(percAlb, percAlpha1AG)
}

\arguments{
    \item{percAlb}{
        Percentage of the molecule binding with albumin.
    }
    \item{percAlpha1AG}{
        Percentage of the molecule binding with alpha1-acid glycoprotein. 
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
    \code{\link{concCh}}  
}

\examples{
perc_alb <- 1
perc_alpha1AG <- 0 
concAd(perc_alb, perc_alpha1AG) 
}

