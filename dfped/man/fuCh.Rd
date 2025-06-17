\name{fuCh}
\alias{fuCh}

\title{
Unbound fraction of the molecule in the plasma for children.
}

\description{
Unbound fraction of the molecule in the plasma for children.
}

\usage{
fuCh(age, fuAd, percAlb, percAlpha1AG)
}

\arguments{
    \item{age}{
       The age of children.
    }
    \item{fuAd}{
        Unbound fraction of the molecule in adults.
    }
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
    \code{\link{Clch.Mat}}
}

\examples{
\dontrun{
    ########
    # Note: For this example we are using a paediatric database that we have including data of 
    # children from 0 to 19 years old.
    ########

    children <- read.csv("/Users/artemistoumazi/paediatric_data_p3m/children_0_19.csv")
    AGE <- children$Age
    fu_ad <- 1
    perc_alb <- 1
    perc_alpha1AG <- 0 

    fuCh(AGE, fu_ad, perc_alb, perc_alpha1AG)
}
}

