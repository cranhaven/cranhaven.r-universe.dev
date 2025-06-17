\name{skeleton}
\alias{skeleton}
\alias{plotEstimates}

\title{
Build a working model.            
}

\description{
The construction of the working model's skeleton.
}

\usage{
skeleton(doseChildren, doseAdult, dataTox, dataAuc = NULL, Clad,
         Clch, nbSimu, graph = TRUE)
}

\arguments{  
    \item{doseChildren}{
        The paediatric dose level.
    } 
    \item{doseAdult}{ 
        The adult dose level.
    }  
    \item{dataTox}{ 
        The database of the toxicities.
    } 
    \item{dataAuc}{ 
        The database of the AUC; defaults to NULL. 
    } 
    \item{Clad}{
        The clearance of the adults.
    } 
    \item{Clch}{
        Paediatric clearance (known or estimated). An estimate can be computed using maturation adjustment (MA), allometric adjustment (AA) or linear adjustment (LA) for a specific group of age.
    }
    \item{nbSimu}{
        The number of simulation using in meta analysis function \code{\link{metaPhase}}.
    }
    \item{graph}{
        A choice to plot the estimates using the function \code{\link{plotEstimates}} in the end of the working model. Indicates graph = TRUE to plot or otherwise graph = FALSE; defaults to TRUE.
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
    \code{\link{plotEstimates}}, \code{\link{metaPhase}}
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
    perc_CYPg <- data.frame("CYP3A4_5" = 1)
    perc_alb <- 1
    perc_alpha1AG <- 0 

    data_molecule <- list(F_ad, f_abs, Eg, Eh, fu_ad, perc_CYPg, perc_CYPh, 
                          perc_alb, perc_alpha1AG)
    Clch_mat <- Clch.Mat(AGE, W, Cl_ad, W_ad, data_molecule)

    ####################################
    ########## WORKING MODEL ###########
    ####################################

    children <- data.frame(children, Clch_mat)

    ########## Children from 2 to 5 years old 
    children2_5 <- children[children$Age >= 2 & children$Age <= 5  ,]                          
    Cl_ch <-  mean(children2_5$Clch_mat)                    

    # Doses for paediatric using maturation adjustment
    dCh_mat_2_5 <- c(30, 45, 55, 70, 85)

    Cl_ad <- 3.95
    AUCThomas <- c(20,40, 60)
    probaToxThomas <- c(0.1,0.25, 0.55)


    ################# Non-parametric PAVA estimate ###################

    # data from the publications of toxicity in the erlotinib
    pardos_2006 <- rbind(c(100,0/3, 3), c(150, 1/3,3), c(200, 0/3, 3), c(250, 3/6, 6))
    thepot_2014 <- rbind(c(100, 0/5, 5), c(150,3/25, 25))
    calvo_2007 <- rbind(c(150, 1/25, 25))
    raizer_2010 <- rbind(c(150,11/99, 99))
    vanDenBent_2009 <- rbind( c(200, 6/54, 54))
    sheikh_2012 <- rbind(c(150, 0.544, 307)) 
    rocheNTC00531934 <- rbind(c(150, 0.186, 59)) 


    dataTox <- rbind(pardos_2006, thepot_2014, calvo_2007, raizer_2010, vanDenBent_2009, 
                     rocheNTC00531934, sheikh_2012)
    dataTox <- data.frame(dataTox)
    colnames(dataTox) <- c("doses", "proba", "nbPatients")
    nbTox <- dataTox$proba*dataTox$nbPatients
    dataTox <- data.frame(dataTox, nbTox)

    data_auc <- data.frame(AUCThomas, probaToxThomas )
    dose_children <- dCh_mat_2_5[1:4]
    dose_adult <- c(100,150,200, 250)
    graph <- TRUE

    skeleton(dose_children, dose_adult, dataTox, data_auc, Cl_ad, Cl_ch, nbSimu = 10, 
             graph = TRUE)
}
}
