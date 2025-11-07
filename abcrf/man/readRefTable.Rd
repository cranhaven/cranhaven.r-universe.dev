\name{readRefTable}
\alias{readRefTable}

\title{Read a reference table simulated from DIYABC}

\description{
\code{readRefTable} reads a reference table simulated from DIYABC thanks to a .bin and a .txt file, respectively containing the reference table and its header.
}

\usage{
readRefTable(filename = "reftable.bin", header = "header.txt", N = 0)
}
\arguments{
\item{filename}{a .bin file from DIYABC containing the reference table.}
\item{header}{a .txt file containing the header of the reference table in \code{filename}.}
\item{N}{an integer indicating the number of observations to extract from the reference table. The default is 0 indicating that the whole reference table is used. Warning: if N is specified, nrecscen returns is obsolete.}
}
\value{
A list with the following components:
\item{nrec}{number of individuals of the reference table,}
\item{nscen}{number of scenarios in the reference table,}
\item{nrecscen}{number of individuals by scenario,}
\item{nparam}{number of parameters by scenario,}
\item{scenarios}{a vector of factor containing the scenario indices,}
\item{params}{a matrix with the parameters,}
\item{stats}{a matrix with the summary statistics.}
}
\references{
Cornuet J.-M., Pudlo P., Veyssier J., Dehne-Garcia A., Gautier M., Leblois R., Marin J.-M. and Estoup A. (2014)
\emph{DIYABC v2.0: a software to make Approximate Bayesian Computation inferences about population history using Single Nucleotide Polymorphism, DNA sequence and microsatellite data} Bioinformatics
\doi{10.1093/bioinformatics/btn514}
}
