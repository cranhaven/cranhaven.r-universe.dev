\name{features}
\alias{features}
\title{
Cluster features extraction 
}
\description{
This function finds features associated with each cluster.
}
\usage{

features(doc,cla,cl,HMDB_ID)
}
\arguments{
  \item{doc}{The output of the \code{\link{readMet}} function.}
  \item{cla}{The output of \code{\link{diseasesMet}}, \code{\link{enzymesMet}}, \code{\link{pathwaysMet}}, \code{\link{propertiesMet}}, \code{\link{substituentsMet}}, or \code{\link{taxonomyMet}}  functions.}
  \item{cl}{The output of the \code{\link{allbranches}} function  containing the module memberships.}
  \item{HMDB_ID}{A vector of HMDB IDs associated with their chemical name.}
}

\value{
  A list of p-value calculated using Fisher test for cluster associted features.
}

\seealso{
  \code{\link{KODAMA.chem.sim}}, \code{\link{tree.cutting}}, \code{\link{substituentsMet}}
}

\examples{
\donttest{
data(Metabolites)
SMILES=Metabolites$SMILES
names(SMILES)=Metabolites$name
HMDB=Metabolites$HMDB
names(HMDB)=Metabolites$name
res=KODAMA.chem.sim(SMILES)
cl=allbranches(res$hclust)
cla=substituentsMet(Metabolites$readMet)
f=features(Metabolites$readMet,cla,cl,HMDB)
}
}
