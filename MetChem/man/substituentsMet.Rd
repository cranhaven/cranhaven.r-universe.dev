\name{substituentsMet}
\alias{substituentsMet}
\title{
Metabolite substituents 
}

\description{This function finds the metabolite related substituents.}

\usage{
substituentsMet(doc)
}
\arguments{
  \item{doc}{A list of metabolites information produced by \code{\link{readMet}} function}.
}

\value{
A data frame contains the substituents of each metabolite.
}


\seealso{
  \code{\link{readMet}}, \code{\link{nameMet}}, \code{\link{propertiesMet}}
}


\examples{

data(Metabolites)
sub=substituentsMet(Metabolites$readMet)

}

