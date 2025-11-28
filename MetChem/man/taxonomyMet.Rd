\name{taxonomyMet}
\alias{taxonomyMet}

\title{
Metabolite Taxonomy
}
\description{This function finds the metabolite related taxonomy.}

\usage{
taxonomyMet(doc)
}

\arguments{
  \item{doc}{A list of metabolites information produced by \code{\link{readMet}} function.}
}

\value{
A data frame contains the taxonomy of each metabolite.
}


\seealso{\code{\link{readMet}}, \code{\link{propertiesMet}}, \code{\link{enzymesMet}}, \code{\link{diseasesMet}}
   }
   

\examples{

data(Metabolites)
tax=taxonomyMet(Metabolites$readMet)

}


