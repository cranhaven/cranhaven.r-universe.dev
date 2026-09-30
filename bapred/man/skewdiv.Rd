\name{skewdiv}
\alias{skewdiv}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Skewness divergence score
}
\description{
This metric presented in Shabalin et al. (2008) is concerned with the 
dissimilarity across batches of the skewnesses of the observation-wise 
empirical distributions of the data.
}
\usage{
skewdiv(xba, batch)
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
For two batches j and j* (see next paragraph for the case with more batches): 1) for each observation 
calculate the difference between the mean and the median of the data as a measure for 
the skewness of the distribution of the variable values; 2) determine the area between 
the two batch-wise empirical cumulative density functions of the values out of 1). The value 
obtained in 2) can be regarded as a measure for the disparity of the batches with respect 
to the skewness of the observation-wise empirical distributions. 

For more than two batches: 1) for all possible pairs of batches: calculate the metric as described above; 2) calculate
the weighted average of the values in 1) with weights proportional to the sum of the sample sizes in the two respective batches. 

The variables are standardized before the calculation to make the metric independent of scale. 
}
\value{
Value of the metric
}
\references{
Shabalin, A. A., Tjelmeland, H., Fan, C., Perou, C. M., Nobel, A. B. (2008). Merging two gene-expression studies via cross-platform normalization. Bioinformatics 24(9):1154-1160, <\doi{10.1093/bioinformatics/btn083}>.
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

skewdiv(xba=X, batch=batch)

params <- ba(x=X, y=y, batch=batch, method = "ratiog")
Xadj <- params$xadj

skewdiv(xba=Xadj, batch=batch)

params <- ba(x=X, y=y, batch=batch, method = "combat")
Xadj <- params$xadj

skewdiv(xba=Xadj, batch=batch)
}
