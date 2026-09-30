\name{kldist}
\alias{kldist}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Kullback-Leibler divergence between density of within and between batch pairwise distances
}
\description{
This metric estimates the Kullback-Leibler divergences between the distributions 
of the within and that of the between batch euclidian distances of pairs of 
observations. These distributions should be similar in the abscence of stronger 
batch effects.
}
\usage{
kldist(xba, batch)
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
For two batches j and j* (see next paragraph for the case with more batches): 1) the distances between all pairs of observations in batch j - denoted as \{dist_j\} - and 
the distances between all such pairs in batch j* - denoted as \{dist_j*\} - are calculated; 
2) for each observation in j the distances to all observations in j* are calculated, 
resulting in n_j x n_j* distances denoted as \{dist_jj*\}; calculate the 
Kullback-Leibler divergence between the densities of \{dist_j\} and \{dist_jj*\} and that 
between the densities of \{dist_j*\} and \{dist_jj*\} - using the k-nearest neighbours based 
method by Boltz et al (2009) with k=5; 3) take the weighted mean of the values of 
these two divergences with weights proportional to n_j and n_j*. 

For more than two batches: 1) for all possible pairs of batches: calculate the metric as described above; 2) calculate
the weighted average of the values in 1) with weights proportional to the sum of the sample sizes in the two respective batches. 

The variables are standardized before the calculation to make the metric independent of scale.
}
\value{
Value of the metric
}
\references{
Lee, J. A., Dobbin, K. K., Ahn, J. (2014). Covariance adjustment for batch effect in gene expression data. Statistics in Medicine 33:2681-2695, <\doi{10.1002/sim.6157}>.

Boltz, S., Debreuve, E., Barlaud, M. (2009). High-dimensional statistical measure for region-of-interest tracking. Transactions in Image Processing 18(6):1266-1283, <\doi{10.1109/TIP.2009.2015158}>.

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

kldist(xba=X, batch=batch)
}
