\name{diseasesMet}

\alias{diseasesMet}

\title{Metabolite-associated Diseases}

\description{This function correlates metabolites to associated diseases.}

\usage{
diseasesMet(doc)
}

\arguments{
  \item{doc}{A list of metabolites information produced by \code{\link{readMet}} function.}
}

\value{
A data frame contains the diseases associated with each metabolite.
}


\seealso{\code{\link{pathwaysMet}},   \code{\link{taxonomyMet}},   \code{\link{enzymesMet}}
   }
   

\examples{

data(Metabolites)
dis=diseasesMet(Metabolites$readMet)

}

