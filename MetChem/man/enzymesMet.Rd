\name{enzymesMet}

\alias{enzymesMet}

\title{
Metabolite-associated Enzymes
}

\description{This function finds the metabolite related enzymes.}

\usage{
enzymesMet(doc)
}


\arguments{
  \item{doc}{A list of metabolites information produced by \code{\link{readMet}} function.}
}

\value{
A data frame contains the enzymes associated with each metabolite.
}

\seealso{
  \code{\link{pathwaysMet}} ,  \code{\link{taxonomyMet}},  \code{\link{diseasesMet}}
   }



\examples{

data(Metabolites)
enz=enzymesMet(Metabolites$readMet)

}

