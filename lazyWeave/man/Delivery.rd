\name{Delivery}
\alias{Delivery}
\title{Obstetric Delivery Log}
\description{Description of 100 hypothetical deliveries from a midwestern hospital.

  These data are simulated in a way to be similar to data collected from a 
  midwestern hospital.  However they are not actual clinical data.  
}

\usage{data(Delivery)}

\format{A data frame with 100 rows and 15 variables.  
  \tabular{ll}{
    maternal.id       \tab Mother's ID number (repeats for twins) \cr
    child.id          \tab Child ID; given by appending a letter to the maternal id.  The letter represents the birth order when multiples are delivered.\cr
    maternal.age      \tab Mother's age at time of delivery.\cr
    grava             \tab Total number of pregnancies the mother has experienced (including current pregnancy)\cr
    para              \tab Total number of prior live births\cr
    ga.weeks          \tab Gestational age; the whole number of weeks the mother has been pregnant.\cr
    ga.days           \tab Gestational age; a number between 0 and 6.  This may be added to \code{ga.weeks} to get the total gestational age.\cr
    delivery.type     \tab Denotes if the delivery was vaginal or cesarean.\cr
    child.sex         \tab Biological sex of the child\cr
    vaginal.type      \tab Denotes if a vaginal delivery was spontaneous (natural) or instrument assisted\cr
    wt.gram           \tab Birth weight in grams\cr
    apgar1            \tab 1 minute APGAR score\cr
    apgar5            \tab 5 minute APGAR score\cr
    laceration        \tab Binary indicator for laceration.  0 = No, 1 = Yes.  Only applies to vaginal deliveries\cr
    laceration.degree \tab Description of the severity of laceration.\cr
  }    
}

\source{Simulation of Midwestern Hospital Delivery Log.}

\keyword{ datasets }
