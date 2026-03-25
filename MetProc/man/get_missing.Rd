\name{get_missing}
\alias{get_missing}
\title{Compute Missing Rates of Biological Samples and Pooled Plasma Samples}
\description{Computes two missing rates per metabolite: 1. Missing rate of biological samples and 2. Missing rate of pooled plasma samples. Requires a metabolomics data matrix from \code{\link{read.met}} function as well as the indicies of pooled plasma and biological samples from \code{\link{get_group}}. Returns a list with the two missing rates across all metabolites}
\usage{get_missing(df, ppind, sampind)}
\arguments{
  \item{df}{The metabolomics dataset, ideally read from the \code{\link{read.met}} function. Each column represents a sample and each row represents a metabolite. Columns should be labeled with some unique prefix denoting whether the column is from a biological sample or pooled plasma sample. For example, all pooled plasma samples may have columns identified by the prefix \dQuote{PPP} and all biological samples may have columns identified by the prefix \dQuote{X}. Missing data must be coded as NA. Columns must be ordered by injection order.}
  \item{ppind}{The indices of the pooled plasma samples.}
  \item{sampind}{The indices of the biological samples.}
}
\value{A list with the missing rates of the pooled plasma samples and biological samples for all metabolites in dataframe. The keys are:
\item{ppmiss}{The pooled plasma missing rate for each metabolite}
\item{sampmiss}{The biological sample missing rate for each metabolite}
}
\examples{
library(MetProc)

#Read metabolomics data
metdata <- read.met(system.file("extdata/sampledata.csv", package="MetProc"),
headrow=3, metidcol=1, fvalue=8, sep=",", ppkey="PPP", ippkey="BPP")

#Get groups based on samples and pooled plasma
grps <- get_group(metdata,'PPP','X') 

#Get the missing rates of each category for all metabolites
missrate <- get_missing(metdata,grps[['pp']],grps[['sid']]) 
}
\seealso{
See \code{\link{MetProc-package}} for examples of running the full process.
}