\name{minP.adj.pvalue}

\alias{minP.adj.pvalue}

\title{
Adjusted p-values for the multiple test. 
}
\description{
Computes the adjusted p-values for multiple test (minP). 
}
\usage{
minP.adj.pvalue(p.0, size, defaultPattern)
}

\arguments{
  \item{p.0}{
A vector with the PD values under H0
}
  \item{size}{
A vector with the sizes of the rating classes  
}
  \item{defaultPattern}{
A vector with the observed default pattern
}
}
\details{
Compute the adjusted p-values for the multiple test. 

}
\value{
A list with
\item{par }{The parameters used}
\item{raw.p.value }{The raw p-values}
\item{minp.adj.p.value }{The adjusted p-valus}
}


\examples{
mpa<-minP.adj.pvalue(p.0=c(0.001, 0.004), size=c(1000, 1000), 
                defaultPattern=c(0, 0))
}
