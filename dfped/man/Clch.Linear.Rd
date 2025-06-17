\name{Clch.Linear}
\alias{Clch.Linear}

\title{
Paediatric clearance according to the linear adjustment (LA) for a specific age.
}

\description{
Compute the paediatric clearance according to the linear adjustment (LA) for a specific age.
}

\usage{
Clch.Linear(age, w, Clad, Wad)
}

\arguments{
    \item{age}{
        The age of child.
    }
    \item{w}{
        The weight of child.
    }
    \item{Clad}{
        The apparent clearance of adult.
    }
    \item{Wad}{
        Weight of adult (or average weight in the adult population).
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
    \code{\link{Clch.Allo}}, \code{\link{Clch.Mat}} 
}

\examples{
\dontrun{
    ########
    # Note: For this example we are using a paediatric database that we have including data of 
    # children from 0 to 19 years old.
    ########

    children <- read.csv("/Users/artemistoumazi/paediatric_data_p3m/children_0_19.csv")
    AGE <- children$Age
    W <- children$Weight
    W_ad <- 70
    Cl_ad <- 3.95

    Clch.Linear(AGE, W, Cl_ad, W_ad)
}
}

