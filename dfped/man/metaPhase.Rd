\name{metaPhase}
\alias{metaPhase}

\title{
Meta-analysis function of dose-finding studies proposed by Zohar et al, (2011).
}

\description{ 
A function of meta-analysis for dose-finding studies in clinical trials proposed by Zohar et al, (2011). 
} 

\usage{
metaPhase(dataTox, doses, nbSimu)
}

\arguments{
  \item{dataTox}{
    A database of the toxicity outcomes for each patient; must be a dataframe.
}
  \item{doses}{
    The drug's dose levels.
}
  \item{nbSimu}{
    The number of simulations.
}
}

\references{
Petit, C., et al, (2016) Unified approach for extrapolation and bridging of adult information in early phase dose-finding paediatric studies, Statistical Methods in Medical Research, <doi:10.1177/0962280216671348>.
    
Zohar, S., et al, (2011) An approach to meta-analysis of dose-finding studies, Statistics in Medicine. 
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
    doses <- c(100,150,200, 250)
    nbSimu <- 10

    metaPhase(dataTox, doses, nbSimu)
}
}

