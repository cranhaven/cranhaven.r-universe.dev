\name{chemical.dissimilarity}
\alias{chemical.dissimilarity}
\title{
Chemical dissimilarity.
}
\description{
This function calculates the structural dissimilarity between different metabolites using the simplified molecular-input line-entry system (SMILE) of each metabolite as input.
}
\usage{

chemical.dissimilarity (smiles,method="tanimoto",type="extended")

}

\arguments{
\item{smiles}{
A vector of smile notations.
}
\item{method}{
The method used to calculated the distance between molecular fingerprint ("tanimoto" as default). For more information see  \code{\link[fingerprint]{fp.sim.matrix}} function. 
}
\item{type}{
The type of fingerprint applied to the SMILEs ("extended" as default). For more information see  \code{\link[rcdk]{get.fingerprint}} function.
}
}

\value{
  A list contains distance between fingerprints .

}

\seealso{
  \code{\link[fingerprint]{fp.sim.matrix}}, \code{\link[rcdk]{get.fingerprint}}, 
}
\examples{

data(Metabolites)
d=chemical.dissimilarity(Metabolites$SMILES[1:50])

}


