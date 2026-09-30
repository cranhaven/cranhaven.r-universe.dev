\name{rmatrain}
\alias{rmatrain}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
RMA normalization with ``documentation by value'' (Kostka & Spang, 2008)
}
\description{
Performs RMA normalization and returns the normalized dataset together with information necessary for addon RMA normalization (Kostka & Spang, 2008) using \code{\link{rmaaddon}}.
}
\usage{
rmatrain(affybatchtrain)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{affybatchtrain}{
object of class \code{AffyBatch}. Affymetrix GeneChip probe level data.
}
}
\details{
This function uses code from the off-CRAN package \code{docval}, version 1.0.
}
\value{
\code{rmatrain} returns an object of class \code{rmatrain}.
An object of class "\code{rmatrain}" is a list containing the following components: 
\item{xnorm}{ matrix of RMA normalized (training) data. Observations in rows, variables in columns. }
\item{rmadoc, sumdoc.rma, nfeature}{ information necessary for addon RMA normalization }
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
}
}
