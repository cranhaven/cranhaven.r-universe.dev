\name{samplesCoda}
\alias{samplesCoda}
\title{Writing files in CODA format}
\description{This function writes files in CODA format to be processed or imported, e.g, by some other software.}
\usage{
samplesCoda(node, stem, beg = samplesGetBeg(), 
    end = samplesGetEnd(), firstChain = samplesGetFirstChain(), 
    lastChain = samplesGetLastChain(), thin = samplesGetThin())
}
\arguments{
  \item{node}{Character vector of length 1, name of a variable in the model.}
  \item{stem}{The filestem of the CODA files to be generated. See details.}
  \item{beg, end}{Arguments to select a slice of monitored values corresponding to iterations \code{beg:end}.}
  \item{firstChain, lastChain}{Arguments to select a sub group of chains.}
  \item{thin}{to only use every \code{thin}-th value of the stored sample.}
}
\details{
    Example for argument \code{stem}: 
    If \code{stem = "c:/myFolder/foo"}, the resulting files 
    are called \file{fooCODAchain1.txt}, ..., \file{fooCODAchainN.txt}, and 
    \file{fooCODAindex.txt}. They are written into the \code{tempdir()} 
    and copied to the path \file{"c:/myFolder"}.

    If the variable of interest is an array, slices of the array can be selected using the notation 
    \code{variable[lower0:upper0, lower1:upper1, ...]}. 
    
    If the arguments are left at their defaults the whole sample for all chains will be used for output.}
\value{
Prints \sQuote{CODA files written}.
}
\note{If the MCMC simulation has an adaptive phase it will not be possible to make inference using values 
    sampled before the end of this phase.}
\seealso{\code{\link{BRugs}}, \code{\link{help.WinBUGS}}}
\keyword{interface}
\keyword{IO}
\keyword{file}
\concept{coda}
\concept{sample}

