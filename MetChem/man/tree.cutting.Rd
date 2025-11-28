\name{tree.cutting}
\alias{tree.cutting}
\title{
Optimal cluster number calculation.
}
\description{
This function helps to estimate the optimal cluster number that fit the metabolites dataset. It applies different optimal cluster number calculating algorithms to cut clutering tree of \code{\link{hclust}} function. and return a list contains index corresponding to each cluster number.
}
\usage{
tree.cutting (res,max_nc=20)
}                            
\arguments{
\item{res}{
A list produced by \code{\link{KODAMA.chem.sim}} function.
}
\item{max_nc}{
The maximum number of cluster (default = 20).
}
}

\value{
A list contains the calculation for each clustering of Rousseeuw's Silhouette index.

}


\seealso{
  \code{\link{KODAMA.chem.sim}}, \code{\link{WMCSA}}
}
\examples{
\donttest{
data(Metabolites)

res=KODAMA.chem.sim(Metabolites$SMILES)
clu=tree.cutting(res,max_nc = 30)
plot(clu$min_nc:clu$max_nc,clu$res.S)

}
}

