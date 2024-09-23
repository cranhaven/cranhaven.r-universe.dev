\name{golubMerge}
\alias{golubMerge}
\alias{golubMerge.exprs}
\alias{sample.labels}
\title{Data from the Golub et al (1999) Paper}
\usage{
data("golubMerge")
}
\description{
The data are from Golub et al. These are the combined training samples and test samples. There are 47 patients with acute lymphoblastic leukemia (ALL) and 25 patients with acute myeloid leukemia (AML). The samples were assayed using Affymetrix Hgu6800 chips and data on the expression of 7129 genes (Affymetrix probes) are available. The data were obtained from the Web site listed below and transformed slightly. Two objects are in the workspace when the data is loaded: \code{golubMerge.exprs} and \code{sample.labels}.
}
\format{
A matrix with 7129 rows (for the genes) and 72 columns (for the patients).
}

\note{The data also appear in the Bioconductor package \code{golubEsets} in a different format. See Schlattmann (2009) for details on how to handle this type of data.}

\examples{
\dontrun{
## microarray analysis example
data(golubMerge)
idxALL <- which(sample.labels== "ALL")
idxAML <- which(sample.labels == "AML")
pvals <- apply(golubMerge.exprs, 1, function(x){t.test(x[idxAML],x[idxALL])[[3]]})
zvals <- qnorm(1-pvals)
hist(zvals,100)
### Z-values are gaussian distributed, mix identifies a mixture of gaussians. 
mix <- mixalg(obs=zvals, family="gaussian", startk=25)
hist(mix)

### get False discovery rate (Not-differential expressed genes are in component 1)
getFDR(mix, threshold=.4)
}
}


\source{This data is a variant of the data from the Bioconductor \code{golubEsets} package.}
\references{
 Molecular Classification of Cancer: Class Discovery and Class Prediction by Gene Expression Monitoring, Science, 531-537, 1999, T. R. Golub and D. K. Slonim and P. Tamayo and C. Huard and M. Gaasenbeek and J. P. Mesirov and H. Coller and M.L. Loh and J. R. Downing and M. A. Caligiuri and C. D. Bloomfield and E. S. Lander
 
Schlattmann, P. (2009). \emph{Medical Applications of Finite Mixture Models.} Berlin: Springer.
}


\keyword{datasets}
