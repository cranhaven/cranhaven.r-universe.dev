\name{doseRange}
\alias{doseRange}

\title{
Dose-range for the paediatric population according to adult clearance, adult doses and paediatric clearance.
}

\description{
This function gives the dose-range for paediatrics, given the adult apparent clearance, the paediatric apparent clearance (known or estimated) and the adult doses. The paediatric apparent clearance can be estimated using the maturation adjustment (through the function \code{\link{Clch.Mat}}), allometric adjustment (through the function \code{\link{Clch.Allo}}) or linear adjustment (through the function \code{\link{Clch.Linear}}).
}

\usage{
doseRange(Clch, Clad, doseAd)
}

\arguments{
    \item{Clch}{
        The paediatric apparent clearance which can be calculated using the maturation (\code{\link{Clch.Mat}}) or allocation (\code{\link{Clch.Allo}}) or 
        linear adjustment (\code{\link{Clch.Linear}}) functions for a specific age.
    }
    \item{Clad}{
        The clearance of adult.
    }
    \item{doseAd}{
        The dose which is given to adult. 
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
    \code{\link{skeleton}} 
}


\examples{
\dontrun{ 
    ########
    # Note: For this example we are using a paediatric database that we have including data of 
    # children from 0 to 19 years old.
    ########


    # Doses of adults
    doseAd <- data.frame("d1" = 100, "d2" = 150,"d3" = 200,"d4"= 250,"d5" =300)
    
    Cl_ad <- 3.95
    children <- read.csv("/Users/artemistoumazi/paediatric_data_p3m/children_0_19.csv")
    AGE <- children$Age
    W <- children$Weight
    W_ad <- 70
    Cl_ad <- 3.95
    F_ad <- 0.6
    Eg <- 0
    Eh <- 0.058
    f_abs <- F_ad/((1 - Eh)*(1-Eg))
    fu_ad <- 1
    perc_CYPh <- data.frame("CYP3A4_5" = 0.7, "CYP1A2" = 0.3)
    perc_CYPg <- data.frame("CYP3A4_5" = 1)
    perc_alb <- 1
    perc_alpha1AG <- 0 

    data_molecule <- list(F_ad, f_abs, Eg, Eh, fu_ad, perc_CYPg, perc_CYPh, perc_alb, 
                          perc_alpha1AG)

    # Compute the clearance of children using maturation adjustment via 
    # the function Clch.Mat().
    Clch_mat <- Clch.Mat(AGE, W, Cl_ad, W_ad, data_molecule)
    
    doseRange(Clch_mat, Cl_ad, doseAd)
}
}

