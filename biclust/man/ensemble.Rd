\name{ensemble}
\alias{ensemble}

\title{
Ensemble Methods for Bicluster Algorithms
}
\description{
  Calculates an ensemble of biclusters from different parameter setting of
  possible different bicluster algorithms. 
}
\usage{
ensemble(x, confs, rep = 1, maxNum = 5, similar = jaccard2, thr = 0.8, simthr =0.7,
  subs = c(1, 1), bootstrap = FALSE, support = 0, combine=firstcome, ...)
}

\arguments{
  \item{x}{
Data Matrix}
  \item{confs}{
Matrix containing parameter sets
}
  \item{rep}{
Number of repetitions for each parameter set
}
  \item{maxNum}{
Maximum number of biclusters taken from each run
}
  \item{similar}{
Function to produce a similarity matrix of bicluster
}
  \item{thr}{
Threshold for similarity
}
  \item{simthr}{
Proportion of row column combinations in bicluster
}
  \item{subs}{
Vector of proportion of rows and columns for subsampling. Default c(1,1)
  means no subsampling.
}
  \item{bootstrap}{
Should bootstrap sampling be used (logical: replace=bootstrap).
}
  \item{support}{
Which proportion of the runs must contain the bicluster to have enough support to report it (between 0 and 1).
}
  \item{combine}{
Function to combine the single bicluster only firstcome and hcl for hierarchical clustering are possible at the moment.
}
  \item{...}{
Arguments past to the combine function.
}

}
\details{
Two different kinds (or both combined) of ensembling is
  possible. Ensemble of repeated runs or ensemble of runs on subsamples. 
}
\value{
Return an object of class Biclust
}

\author{
Sebastian Kaiser 
\email{sebastian.kaiser@stat.uni-muenchen.de}
}

\seealso{
 \code{\link{Biclust-class}}, \code{\link{plaid.grid}},  \code{\link{bimax.grid}}
}
\examples{
\dontshow{
data(BicatYeast)
x <- binarize(BicatYeast)
ensemble.bimax <- ensemble(x,bimax.grid(),rep=10,maxNum=2,thr=0.5, subs = c(0.8,0.8))
ensemble.bimax
}
\dontrun{
data(BicatYeast)
ensemble.plaid <- ensemble(BicatYeast,plaid.grid()[1:5],rep=1,maxNum=2, thr=0.5, subs = c(1,1))
ensemble.plaid
x <- binarize(BicatYeast)
ensemble.bimax <- ensemble(x,bimax.grid(),rep=10,maxNum=2,thr=0.5, subs = c(0.8,0.8))
ensemble.bimax
}
}

\keyword{ cluster }
