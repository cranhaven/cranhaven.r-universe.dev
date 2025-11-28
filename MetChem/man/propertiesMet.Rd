\name{propertiesMet}
\alias{propertiesMet}
\title{
Physical Proprieties of metabolites
}
\description{
This function finds the Physical Proprieties of metabolites.
}
\usage{
propertiesMet(doc)
}
\arguments{
  \item{doc}{A list of metabolites information produced by \code{\link{readMet}} function.}
}

\value{
A data frame contains the properties associated with each metabolite.
}


\seealso{\code{\link{readMet}}, \code{\link{taxonomyMet}}, \code{\link{substituentsMet}}, \code{\link{propertiesMet}}
   }

\examples{

data(Metabolites)
pro=propertiesMet(Metabolites$readMet)

}
