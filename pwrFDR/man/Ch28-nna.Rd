\name{nna}
\alias{nna}
\title{
  A helper function-- turns a missing column into 'NA's inside of a with statement
}
\description{
  A helper function-- turns a missing column into 'NA's inside of a with
  statement. Included at the user level because its useful in setting up
  batch jobs, especially since the 'pwrFDR' return argument list varies
  depending on the manner called.
}
\usage{
  nna(x)
}
\arguments{
  \item{x}{
    A named numeric vector component of a data frame which may or may not be present.
  }
}
\value{
    Either the values in the component \code{x} of the data.frame or NA\'s of equal length
}
\author{
  Grant Izmirlian <izmirlian at nih dot gov>
}
\examples{
  sim.1 <- pwrFDR(effect.size=0.8, n.sample=60, lambda=0.90, r.1=0.05, N.tests=450, alpha=0.15,
                  method="sim", FDP.control.method="BHCLT")

  sim.2 <- pwrFDR(effect.size=0.8, n.sample=60, lambda=0.90, r.1=0.05, N.tests=450, alpha=0.15,
                  method="sim", FDP.control.method="both", control=list(sim.level=2))

  with(detail(sim.1)$reps, cbind(R.st/100, nna(R.R)/450))

  with(detail(sim.2)$reps, cbind(R.st/100, nna(R.R)/450))
}


