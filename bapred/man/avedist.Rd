\name{avedist}
\alias{avedist}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Average minimal distance between batches
}
\description{
This metric is concerned with the minimal distances between pairs of batches.
}
\usage{
avedist(xba, batch)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xba}{
matrix. The covariate matrix, raw or after batch effect adjustment. observations in rows, variables in columns.
}
  \item{batch}{
factor. Batch variable. Each factor level (or 'category') corresponds to one of the batches. For example, if there are four batches, this variable would have four factor levels and observations with the same factor level would belong to the same batch.
}
}
\details{
For two batches j and j* (see next paragraph for the case with more batches): 1) for each observation in batch j the euclidean 
distance to the nearest observation in batch j* is calculated; 2) the roles 
of j and j* are switched and 1) is re-performed; 3) the average is taken over all n_j + n_j* minimal distances. 

For more than two batches: 1) for all possible pairs of batches: calculate the metric as described above; 2) calculate
the weighted average of the values in 1) with weights proportional to the sum of the sample sizes in the two respective batches. 

The variables are standardized before the calculation to make the metric independent of scale.
}
\value{
Value of the metric
}
\references{
Lazar, C., Meganck, S., Taminau, J., Steenhoff, D., Coletta, A., Molter,C., Weiss-Solís, D. Y., Duque, R., Bersini, H., Nowé, A. (2012). Batch effect removal methods for microarray gene expression data integration: a survey. Briefings in Bioinformatics 14(4):469-490, <\doi{10.1093/bib/bbs037}>.

Hornung, R., Boulesteix, A.-L., Causeur, D. (2016). Combining location-and-scale batch effect adjustment with data cleaning by latent factor adjustment. BMC Bioinformatics 17:27, <\doi{10.1186/s12859-015-0870-z}>.
}
\author{
Roman Hornung
}
\note{
The smaller the values of this metric, the better.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
data(autism)

avedist(xba=X, batch=batch)
}
