\name{pvcam}
\alias{pvcam}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Proportion of variation induced by class signal estimated by Principal Variance Component Analysis
}
\description{
Principal Variance Component Analysis (PVCA) (Li et al, 2009) allows the estimation of the contribution of several sources of variability. \code{pvcam} uses it to estimate the proportion of variance in the data explained by the class signal. See below for a more detailed explanation of what the function does.
}
\usage{
pvcam(xba, batch, y, threshold = 0.6)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xba}{
matrix. The covariate matrix, raw or after batch effect adjustment. observations in rows, variables in columns.
}
  \item{batch}{
factor. Batch variable. Each factor level (or 'category') corresponds to one of the batches. For example, if there are four batches, this variable would have four factor levels and observations with the same factor level would belong to the same batch.
}
  \item{y}{
factor. Binary target variable. Has to have two factor levels, where each of them correponds to one of the two classes of the target variable.
}
  \item{threshold}{
numeric. Minimal proportion of variance explained by the principal components used.
}
}
\details{
In PVCA, first principal component analysis is performed on the n x n covariance matrix between the
observations. Then, using a random effects model
the principal components are regressed on arbitrary factors of variability, such
as "batch" and "(phenotype) class". Ultimately, estimated proportions of variance induced by each factor and that of the residual variance are obtained. 
In \code{pvcam} the factors included into the model are: "batch", "class" and the interaction of these two into.
The metric calculated by \code{pvcam} is the proportion of variance
explained by "class".

\code{pvcam} uses a slightly altered version of the function \code{pvcaBatchAssess()} from the Bioconductor package \code{pvca}.
The latter was altered to take the covariate data as a \code{matrix} instead of as an object of class \code{ExpressionSet}.
}
\value{
Value of the metric
}
\references{
Li, J., Bushel, P., Chu, T.-M., Wolfinger, R.D. (2009). Principal variance components analysis: Estimating batch effects in microarray gene expression data. In: Scherer, A. (ed) Batch Effects and Noise in Microarray Experiments: Sources and Solutions, John Wiley & Sons, Chichester, UK, <\doi{10.1002/9780470685983.ch12}>.
}
\author{
Roman Hornung
}
\note{
Higher values of this metric indicate a better preservation or exposure, respectively, of the biological signal of interest. 
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
data(autism)

Xadj <- ba(x=X, y=y, batch=batch, method = "combat")$xadj

pvcam(xba = X, batch = batch, y = y)
pvcam(xba = Xadj, batch = batch, y = y)
}
