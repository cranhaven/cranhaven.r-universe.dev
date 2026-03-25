\name{subset_met}
\alias{subset_met}
\title{Group Metabolites based on Pooled Plasma Missing Rate}
\description{Separates metabolites into groups based on pooled plasma missing rates so that different thresholds of metrics can be applied to each group.}
\usage{
subset_met(df, miss, numsplit = 5, mincut = 0.02, maxcut = 0.95)
}
\arguments{
  \item{df}{The metabolomics dataset, ideally read from the \code{\link{read.met}} function. Each column represents a sample and each row represents a metabolite. Columns should be labeled with some unique prefix denoting whether the column is from a biological sample or pooled plasma sample. For example, all pooled plasma samples may have columns identified by the prefix \dQuote{PPP} and all biological samples may have columns identified by the prefix \dQuote{X}. Missing data must be coded as NA. Columns must be ordered by injection order.}
  \item{miss}{Vector of missing rates of equal length to number of rows in \code{df} representing the pooled plasma missing rate for each metabolite.}
  \item{numsplit}{The number of equal sized sections to divide metabolites into based on missing rate of pooled plasma columns. Divides the range of missing rates between \code{mincut} and \code{maxcut} into equal sections. Default is \code{5}.}
  \item{mincut}{A cutoff to specify that any metabolite with pooled plasma missing rate less than or equal to this value should be retained. Default is \code{0.02}.}
  \item{maxcut}{A cutoff to specify that any metabolite with pooled plasma missing rate greater than this values should be removed. Default is \code{0.95}.}
}
\value{A list consisting of a number of elements equal to \code{numsplit}. Each element contains a matrix of the given metabolite group based on the pooled plasma missing rate. The list keys are simple integers corresponding to the split number.}

\examples{
library(MetProc)

#Read in metabolomics data
metdata <- read.met(system.file("extdata/sampledata.csv", package="MetProc"),
headrow=3, metidcol=1, fvalue=8, sep=",", ppkey="PPP", ippkey="BPP")

#Get indices of pooled plasma and samples
groups <- get_group(metdata,"PPP","X")

#Calculate a pooled plasma missing rate and sample missing rate
#for each metabolite in data
missrate <- get_missing(metdata,groups[['pp']],groups[['sid']])

#Group metabolites into 5 groups based on pooled plasma
#missing rate
subsets <- subset_met(metdata,missrate[['ppmiss']],5,.02,.95)
}
\seealso{
See \code{\link{MetProc-package}} for examples of running the full process.
}

