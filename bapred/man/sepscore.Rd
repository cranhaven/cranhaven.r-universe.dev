\name{sepscore}
\alias{sepscore}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Separation score as described in Hornung et al. (2016)
}
\description{
This metric described in Hornung et al. (2016) was derived from the mixture score 
presented in Lazar et al. (2012). In contrast to the mixture score the 
separation score does not measure the degree of mixing but the degree of 
separation between the batches. Moreover it is less dependent on the 
relative sizes of the involved batches.
}
\usage{
sepscore(xba, batch, k = 10)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xba}{
matrix. The covariate matrix, raw or after batch effect adjustment. observations in rows, variables in columns.
}
  \item{batch}{
factor. Batch variable. Each factor level (or 'category') corresponds to one of the batches. For example, if there are four batches, this variable would have four factor levels and observations with the same factor level would belong to the same batch.
}
  \item{k}{
integer. Number of nearest neighbors.
}
}
\details{
For two batches j and j* (see next paragraph for the case with more batches): 1) for each observation in batch j its k nearest neighbours in both batches j and j* 
simultaneously with respect to the euclidean distance are determined. Here, the proportion 
of those of these nearest neighbours, which belong to batch j* is calculated; 2) the 
average - denoted as MS_j - is taken over the thus obtained n_j proportions. This value 
is the mixture score as in Lazar et al. (2012); 3) to obtain a measure for the separation 
of the two batches the absolute difference between MS_j and its value expected in the 
absence of batch effects is taken: |MS_j - n_j* /(n_j + n_j* - 1)|; 4) the separation score 
is defined as the simple average of the latter quantity and the corresponding quantity when 
the roles of j and j* are switched. If the supplied number \code{k} of nearest neighbours 
is larger than n_j + n_j*, \code{k} is set to n_j + n_j* - 1 internally. 

For more than two batches: 1) for all possible pairs of batches: calculate the metric as described above; 2) calculate
the weighted average of the values in 1) with weights proportional to the sum of the sample sizes in the two respective batches.
}
\value{
Value of the metric
}
\references{
Hornung, R., Boulesteix, A.-L., Causeur, D. (2016). Combining location-and-scale batch effect adjustment with data cleaning by latent factor adjustment. BMC Bioinformatics 17:27, <\doi{10.1186/s12859-015-0870-z}>.

Lazar, C., Meganck, S., Taminau, J., Steenhoff, D., Coletta, A., Molter,C., Weiss-Solís, D. Y., Duque, R., Bersini, H., Nowé, A. (2012). Batch effect removal methods for microarray gene expression data integration: a survey. Briefings in Bioinformatics 14(4):469-490, <\doi{10.1093/bib/bbs037}>.
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

# Random subset of 150 variables:
set.seed(1234)
Xsub <- X[,sample(1:ncol(X), size=150)]

# In cases of batches with more than 20 observations
# select 20 observations at random:
subinds <- unlist(sapply(1:length(levels(batch)), function(x) {
  indbatch <- which(batch==x)
  if(length(indbatch) > 20)
    indbatch <- sort(sample(indbatch, size=20))
  indbatch
}))
Xsub <- Xsub[subinds,]
batchsub <- batch[subinds]
ysub <- y[subinds]



sepscore(xba=Xsub, batch=batchsub, k=5)

params <- ba(x=Xsub, y=ysub, batch=batchsub, method = "ratiog")
Xsubadj <- params$xadj

sepscore(xba=Xsubadj, batch=batchsub, k=5)

params <- ba(x=Xsub, y=ysub, batch=batchsub, method = "combat")
Xsubadj <- params$xadj

sepscore(xba=Xsubadj, batch=batchsub, k=5)
}
