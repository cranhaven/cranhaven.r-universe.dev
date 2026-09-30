\name{rmaaddon}
\alias{rmaaddon}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Addon RMA normalization using ``documentation by value'' (Kostka & Spang, 2008)
}
\description{
Performs RMA with addon quantile normalization by using documentation by value (Kostka & Spang, 2008).
}
\usage{
rmaaddon(params, affybatchtest)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{params}{
object of class \code{rmatrain}. The normalized training data together with the information necesssary for addon normalization.
}
  \item{affybatchtest}{
object of class \code{AffyBatch}, that is Affymetrix GeneChip probe level data. Test data to be used for addon normalization.
}
}
\details{
This function uses code from the off-CRAN package \code{docval}, version 1.0.
}
\value{
The covariate matrix of the test data after addon normalization. Observations in rows, variables in columns.
}
\references{
Kostka, D., Spang, R. (2008). Microarray based diagnosis profits from better documentation of gene expression signatures. PLoS Computational Biology 4(2):e22, <\doi{10.1371/journal.pcbi.0040022}>.
}
\author{
Roman Hornung
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
\dontrun{

# Read in example data from ArrayExpress-webpage:

library("ArrayExpress")

expFiles <- getAE("E-GEOD-62837", path = tempdir(), type = "raw")

rawfiles <- file.path(tempdir(), expFiles$rawFiles)

library("affy")
# Training data:
affybatchtrain <- ReadAffy(filenames=rawfiles[1:3])
# Test data:
affybatchtest <- ReadAffy(filenames=rawfiles[4:5])

try(file.remove(file.path(tempdir(), expFiles$rawFiles)))
try(file.remove(file.path(tempdir(), expFiles$processedFiles)))
try(file.remove(file.path(tempdir(), expFiles$sdrf)))
try(file.remove(file.path(tempdir(), expFiles$idf)))
try(file.remove(file.path(tempdir(), expFiles$adf)))
try(file.remove(file.path(tempdir(), expFiles$rawArchive)))
try(file.remove(file.path(tempdir(), expFiles$processedArchive)))


# RMA normalization with documentation by value:
rmaparams <- rmatrain(affybatchtrain)
Xtrainnorm <- rmaparams$xnorm
dim(Xtrainnorm)

# RMA addon normalization:
Xtestaddonnorm <- rmaaddon(rmaparams, affybatchtest)
dim(Xtestaddonnorm)
}
}
