\name{pathwaysMet}
\alias{pathwaysMet}
\title{
Metabolic Pathways
}

\description{This function finds the metabolite related pathways.}

\usage{
pathwaysMet(doc)
}

\arguments{
\item{doc}{A list of metabolites information produced by \code{\link{readMet}}function.}
}

\value{
A data frame contains the pathways associated with each metabolite.
}


\seealso{
  \code{\link{readMet}},  \code{\link{taxonomyMet}}, \code{\link{enzymesMet}}, \code{\link{diseasesMet}}
   }
   
   
\examples{

\donttest{
data(Metabolites)
pat=pathwaysMet(Metabolites$readMet)
}
}

