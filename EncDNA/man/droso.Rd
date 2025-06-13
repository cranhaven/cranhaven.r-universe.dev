\name{droso}
\alias{droso}
\docType{data}
\title{
An example dataset consisting of true and false donor splice sites of Drosophila melanogaster. 
}
\description{
The dataset contains 400 true donor sites, 400 false donor sites and 50 false sites as test set. Each sequence is of 40 nucleotides long. The conserved di-nucleotides GT are present at $21^{st}$ and $22^{nd}$ positions respectively.
}
\usage{data("droso")}
\format{
The dataset belongs to the class \code{DNAStringSet}.
}

\source{
This is a sample dataset which was collected from the dataset originally developed by Reese \emph{et al}. (1997). The dataset can also be accessed at \url{http://www.fruitfly.org/sequence/drosophila-datasets.html }.
}
\references{
Reese, M. G., Eeckman, F. H., Kulp, D. and Haussler, D. (1997). Improved splice site detection in Genie. \emph{Journal of Computational Biology}, 4(3): 311-323.
}
\examples{
data(droso)
}
\keyword{datasets}
