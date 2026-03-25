\name{write.met}
\alias{write.met}
\title{Write Metabolomics Dataset of Standard Structure}
\description{Write a metabolomics file based on the metabolites identified to be retained or removed using \code{\link{met_proc}}. Requires the filepath for the original metabolomics file in order to extract row and column information. Will take in this original file and the results of the \code{\link{met_proc}} function to write a file that contains only the retained or removed metabolites.}
\usage{
write.met(res, filename, origfile, headrow = 3, metidcol=1, fvalue=8, 
sep=",", type="keep")
}
\arguments{
  \item{res}{The result output from \code{\link{met_proc}} function.}
  \item{filename}{The name and path for new metabolomics file.}
  \item{origfile}{The name and path for the original metabolomics file.}
  \item{headrow}{The row number that contains the header line in the original metabolomics file. Default is \code{3}.}
  \item{metidcol}{The column number that contains the metabolite ID in the original metabolomics file. Default is \code{1}.}
  \item{fvalue}{The column number where data begins in the original metabolomics file. Default is \code{8}.}
  \item{sep}{File delimiter for both the original metabolomics file and the new file. Default is \code{","}.}
  \item{type}{Either \sQuote{keep} or \sQuote{remove} to determine whether the retained metabolites or removed metabolites should be written to the file. Default is \code{"keep"}.}
}

\value{
Writes a file to \code{filename} that is of the same structure as the original metabolomics file but only containing either the retained or removed metabolites.
}

\examples{
library(MetProc)
#Read in metabolomics data
metdata <- read.met(system.file("extdata/sampledata.csv", package="MetProc"),
headrow=3, metidcol=1, fvalue=8, sep=",", ppkey="PPP", ippkey="BPP")

#Separate likely artifacts from true signal using default settings
results <- met_proc(metdata,plot=FALSE)

#Write the retained metabolites to current directory
write.met(results,'sample_retained.csv',
system.file("extdata/sampledata.csv", package="MetProc"),
headrow=3,metidcol=1,fvalue=8,sep=",",type='keep')
}
\seealso{
See \code{\link{MetProc-package}} for examples of running the full process.
}
