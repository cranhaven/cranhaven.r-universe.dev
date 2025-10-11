\name{simulatedData}
\alias{simulatedData}
\alias{GeneSets}
\alias{objectMApathSim}
\alias{study1Ex}
\alias{study2Ex}
\alias{study1Pheno}
\alias{study2Pheno}
\docType{data}
\title{GSEMA synthetic data}
\description{
\itemize{
    \item study1Ex, study2Ex: two expression matrices.
    \item study1Pheno, study2Pheno: two phenodata objects.
    \item GeneSets: a list of gene sets with each element are the genes that
    belong to a pathway.
    \item objectMApathSim: the meta-analysis object created from the different
    expression matrices and phenodatas.
}
}
\usage{data(simulatedData)}
\format{
    arrays (\code{study1Ex}, \code{study2Ex}), 
    data.frames (\code{study1Pheno}, \code{study2Pheno}),
    list vectors (\code{GeneSets}) and list of nested lists
    (\code{objectMApathSim}).
}
\source{
    study1Ex, study2Ex, study1Pheno and study2Pheno are synthetic.
    GeneSets was created from the information of MsigDB  database by adding
    a simulated pathway with simulated genes

    objectMApathSim was created after using createObjectMA GSEMA function to
    the different studies objects and with the information of GeneSets object

    maObjectDif was created after using createObjectMA DExMA function
    to the listExpressionSets object.
    maObject was obtained after using allSameID function to maObjectDif
    function.
}

\references{
Liberzon A, Birger C, Thorvaldsdóttir H, Ghandi M, Mesirov JP, Tamayo P.
The Molecular Signatures Database (MSigDB) hallmark gene set collection.
Cell Syst. 2015;1: 417–425
}
\keyword{objectMApath}
