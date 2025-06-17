\name{Clchu}
\alias{Clchu}

\title{
Clearance of the unbound fraction of a specific molecule for the paediatric population.
}

\description{
Compute the clearance of the unbound fraction of a specific molecule for the paediatric population.
}

\usage{
Clchu(age, w, Clad, Wad, fabs, fuAd, Fad, Eg, Eh, percCYPh) 
}

\arguments{
    \item{age}{
        The age of children.
    }
    \item{w}{
        The weight of child.
    }
    \item{Clad}{
       The apparent clearance in the adult population.
    }
    \item{Wad}{
         The weight of adult (or average weight in the adult population).
    }
    \item{fabs}{
        Coefficient of absorption for the molecule.
    }
    \item{fuAd}{ 
       Unbound bioavailability for adults for the molecule.
    }
    \item{Fad}{
      	Bioavailability for adults.
    }
    \item{Eg}{
        Coefficient of intestinal extraction.
    }
    \item{Eh}{
		Coefficient of hepatic extraction.
    }
    \item{percCYPh}{
        Vector giving the percentage of the molecule metabolised for each cytochrome in the liver in adults. Dataframe with two column - column 1: CYP name, column 2: percentage of the molecule metabolised. 
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
    \code{\link{Cladu}}
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
    F_ad <- 0.6
    Eg <- 0
    Eh <- 0.058
    f_abs <- F_ad/((1 - Eh)*(1-Eg))
    fu_ad <- 1
    perc_CYPh <- data.frame("CYP3A4_5" = 0.7, "CYP1A2" = 0.3)

    Clchu(AGE, W, Cl_ad, W_ad, f_abs, fu_ad, F_ad, Eg, Eh, perc_CYPh)
}
}

