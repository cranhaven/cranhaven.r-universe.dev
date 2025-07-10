\name{FGP}
\alias{FGP}
\title{Final Percentage of Germination}
\usage{
FGP(nger, Nseeds)
}

\arguments{
  \item{nger}{ It is a \code{Vector} object containg the accumulated number of seeds geminated. }
  \item{Nseeds}{ The total amount of seeds used for the germination or emergence test. }
}


\description{
Calulated the Final Percentage of Germination.
}

\examples{
nger <- c(0,2,4,15,25,38,45,50,50,50)
FGP(nger,50)
}
