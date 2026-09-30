\name{diffexprm}
\alias{diffexprm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Measure for performance of differential expression analysis (after batch effect adjustment)
}
\description{
This metric is similar to the idea presented in Lazar et al (2012) which consists 
in comparing the list of the most differentially expressed genes obtained using a batch 
effect adjusted dataset to the list obtained using an independent dataset. For each 
batch the following is done by \code{diffexprm}: 1) the respective batch is left out
and batch effect adjustment 
is performed using the remaining batches; 2) differential expression analysis is performed 
once using the left-out batch and once using the remaining batch-effect adjusted data; 
3) the overlap between the two lists of genes found differentially expressed in the 
two subsets is measured. See below for further details.
}
\usage{
diffexprm(x, batch, y, method = c("fabatch", "combat", "sva", 
  "meancenter", "standardize", "ratioa", "ratiog", "none"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
matrix. The covariate matrix. Observations in rows, variables in columns.
}
  \item{batch}{
factor. Batch variable. Each factor level (or 'category') corresponds to one of the batches. For example, if there are four batches, this variable would have four factor levels and observations with the same factor level would belong to the same batch.
}
  \item{y}{
factor. Binary target variable. Has to have two factor levels, where each of them correponds to one of the two classes of the target variable.
}
  \item{method}{
character. Method for batch effect adjustment. The following are supported: \code{fabatch}, \code{combat}, \code{fsva}, \code{meancenter}, \code{standardize}, \code{ratioa}, \code{ratiog} and \code{none}
}
}
\details{
The following procedure is performed: 1) For each batch j leave this batch out and 
perform batch effect adjustment on the rest of the dataset. Derive two lists of the 5 percent 
of variables which are most differentially expressed (see next paragraph): one using the batch 
effect adjusted dataset - where batch j was left out - and one using the data from batch j. 
Calculate the number of variables appearing in both lists and divide this number by the common 
length of the lists. 2) Calculate a weighted average of the values obtained in 1) with weights 
proportional to the number of observations in the corresponding left-out batches. 

Differential expression is measured as follows. For each variable a randomized p-value out 
of the Whitney-Wilcoxon rank sum test is drawn, see Geyer and Meeden (2005) for details. 
Then those 5 percent variables are considered differentially expressed, which are associated 
with the smallest p-values.
}
\value{
Value of the metric
}
\references{
Hornung, R., Boulesteix, A.-L., Causeur, D. (2016). Combining location-and-scale batch effect adjustment with data cleaning by latent factor adjustment. BMC Bioinformatics 17:27, <\doi{10.1186/s12859-015-0870-z}>.

Lazar, C., Meganck, S., Taminau, J., Steenhoff, D., Coletta, A., Molter,C., Weiss-Solís, D. Y., Duque, R., Bersini, H., Nowé, A. (2012). Batch effect removal methods for microarray gene expression data integration: a survey. Briefings in Bioinformatics 14(4):469-490, <\doi{10.1093/bib/bbs037}>.

Geyer, C. J., Meeden, G., D. (2005). Fuzzy and randomized confidence intervals and p-values (with discussion). Statistical Science 20(4):358-387, <\doi{10.1214/088342305000000340}>.
}
\author{
Roman Hornung
}
\note{
The larger the values of this metric, the better.
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



diffexprm(x=Xsub, batch=batchsub, y=ysub, method = "ratiog")
diffexprm(x=Xsub, batch=batchsub, y=ysub, method = "none")
}
