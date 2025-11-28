\name{WMCSA}
\alias{WMCSA}
\title{
Weighted Metabolite Chemical Structural Analysis
}
\description{
Summarize metabolites concetration in each of identified clusters using the module eigenvalue (eigen-metabolite) for calculating module membership measures. 
}
\usage{
WMCSA(data,cl)
}

\arguments{
\item{data}{
dataset of different metabolite concentration in differnt samples.
  }

  \item{cl}{The output of the \code{\link{allbranches}} function  containing the module memberships.}
}

\value{
This function returns a matrix as output represent similarity score of metabolites within the same module among different samples.

}

\seealso{\code{\link{KODAMA.chem.sim}}, \code{\link{tree.cutting}}
}
\examples{
\donttest{

data(Metabolites)

SMILES=Metabolites$SMILES
names(SMILES)=Metabolites$name
res=KODAMA.chem.sim(SMILES)  
cl=allbranches(res$hclust)
ww=WMCSA(Metabolites$concentration,cl)


}
}

