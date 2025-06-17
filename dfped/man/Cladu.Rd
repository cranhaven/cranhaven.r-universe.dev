\name{Cladu}
\alias{Cladu}

\title{
Clearance of the unbound fraction of a specific molecule for the adult population.
}

\description{
Compute the clearance of the unbound fraction of a specific molecule for the adult population.
}

\usage{
Cladu(Clad, fuAd, Fad)
}

\arguments{
    \item{Clad}{
        The apparent clearance for adults.
    }
    \item{fuAd}{ 
       Unbound bioavailability for adults for the molecule. 
    }
    \item{Fad}{
      	Bioavailability for adults.
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
    \code{\link{Clchu}}
}

\examples{
Cl_ad <- 3.95
F_ad <- 0.6
fu_ad <- 1
Cladu(Cl_ad, fu_ad, F_ad)
}

