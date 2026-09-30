\name{svaba}
\alias{svaba}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Batch effect adjustment using SVA
}
\description{
Performs batch effect adjustment using Surrogate Variable Analysis (SVA) and additionally returns information necessary for addon batch effect adjustment with frozen SVA.
}
\usage{
svaba(x, y, batch, nbf = NULL, algorithm = "fast")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
matrix. The covariate matrix. Observations in rows, variables in columns.
}
  \item{y}{
factor. Binary target variable. Has to have two factor levels, where each of them correponds to one of the two classes of the target variable.
}
  \item{batch}{
factor. Batch variable. Each factor level (or 'category') corresponds to one of the batches. For example, if there are four batches, this variable would have four factor levels and observations with the same factor level would belong to the same batch.
}
  \item{nbf}{
integer. Number of latent factors to estimate.
}
  \item{algorithm}{
character. If \code{method = "fast"} the "approximate fSVA algorithm" will be used in frozen SVA. If \code{method = "exact"} the "exact fSVA algorithm" will be used. See Parker et al. (2014).
}
}
\details{
This is essentially a wrapper function of the function \code{sva()} from the Bioconductor package of the same name.
}
\value{
\code{svaba} returns an object of class \code{svatrain}.
An object of class "\code{svatrain}" is a list containing the following components: 
\item{xadj}{ matrix of adjusted (training) data }
\item{xtrain}{ the unadjusted covariate matrix. Used in frozen SVA. }
\item{ytrain}{ binary target variable. Used in frozen SVA. }
\item{svobj}{ output of the function \code{sva()}. Used in frozen SVA. }
\item{algorithm}{ algorithm to use in frozen SVA }
\item{nbatches}{ number of batches }
\item{batch}{ batch variable }
}
\references{
Leek, J. T., Storey, J. D. (2007). Capturing Heterogeneity in Gene Expression Studies by Surrogate Variable Analysis. PLoS Genetics 3:1724-1735, <\doi{10.1371/journal.pgen.0030161}>.

Parker, H. S., Bravo, H. C., Leek, J. T. (2014). Removing batch effects for prediction problems with frozen surrogate variable analysis. PeerJ 2:e561, <\doi{10.7717/peerj.561}>.
}
\author{
Roman Hornung
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



params <- svaba(x=Xsub, y=ysub, batch=batchsub)
}
