\name{Toxdata}
\alias{Toxdata}
\docType{data}

\title{
A toxicity database.
}

\description{
A dataframe consisting 60000 observations of 6 different variables.
}

\usage{data("Toxdata")}

\format{
  A data frame with 60000 observations on the following 6 variables.
  \describe{
    \item{\code{simulation}}{a numeric vector including the simulation.}
    \item{\code{dose}}{a numeric vector of the dose levels.}
    \item{\code{Trial}}{a numeric vector which gives the number of the trial.}
    \item{\code{n}}{a numeric vector of the number of allocation.}
    \item{\code{x}}{a numeric vector giving the number of toxicities.}
    \item{\code{trueptoxic}}{a numeric vector consisting the true probability of toxicity.}
  }
}

\author{
Artemis Toumazi <artemis.toumazi@gmail.com>, Sarah Zohar <sarah.zohar@inserm.fr>, Anand N. Vidyashankar <avidyash@gmu.edu>, Jie Xu <jxu13@gmu.edu> and Moreno Ursino <moreno.ursino@inserm.fr>
}

\references{
Zohar, S., Katsahian, S. and O'Quigley, J. (2011), An approach to meta-analysis of dose-finding studies. Statist. Med.
}

\examples{
data("Toxdata")  
## maybe str(Toxdata): plot(Toxdata) ...
}

\keyword{datasets}
