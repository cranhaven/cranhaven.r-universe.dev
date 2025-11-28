\name{allbranches}
\alias{allbranches}
\title{
Cut a Tree into Groups of Data
}
\description{
Cuts a tree as resulting from \code{\link{hclust}} function, into groups (a.k.a. modules).
}
\usage{

allbranches(hh,minlen=5)

}

\arguments{
\item{hh}{
a tree as produced by \code{\link{hclust}} function. 
}
\item{minlen}{
The minimum number of elements in each module. 
}
}

\value{
  A list contains vectors of module memberships.

}

\seealso{
  \code{\link{cutree}}, \code{\link{hclust}}, \code{\link{clusters.detection}}
}

\examples{

data(Metabolites)

data=Metabolites$readMet$concentration
hh=hclust(dist(data),method="ward.D")
res=allbranches(hh) 


}

