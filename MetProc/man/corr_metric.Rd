\name{corr_metric}
\alias{corr_metric}
\title{Calculate Correlation of Missing Rates between Pooled Plasma and Biological Samples}
\description{Calculates the correlation of missing rates between the two flanking pooled plasma samples and intervening biological samples for each block in the injection order. A block is defined as a set of biological samples and their flanking pooled plasma samples. See \code{\link{sampledata}} for an example of the data format and block structure. Requires 2 arguments as input: 1. The metabolomics dataset formatted from the \code{\link{read.met}} function and 2. A list of 2 elements output from the \code{\link{get_group}} function containing column indices of pooled plasma samples and biological samples, respectively. If either pooled plasma or biological samples are entirely absent or entirely present, the function will return NA for the metric of that metabolite as the standard deviation of a vector will be 0.}
\usage{corr_metric(df, grps)}
\arguments{
  \item{df}{The metabolomics dataset, ideally read from the \code{\link{read.met}} function. Each column represents a sample and each row represents a metabolite. Columns should be labeled with some unique prefix denoting whether the column is from a biological sample or pooled plasma sample. For example, all pooled plasma samples may have columns identified by the prefix \dQuote{PPP} and all biological samples may have columns identified by the prefix \dQuote{X}. Missing data must be coded as NA. Columns must be ordered by injection order.}
  \item{grps}{A list of 2 elements from the \code{\link{get_group}} function. Element \code{"pp"} should contain indices of pooled plasma columns and \code{"sid"} should contain indicies of biological sample columns.}
}
\value{Returns a vector of equal length to the number of rows in \code{df} (representing metabolites) with the correlation of missing rates between flanking pooled plasma and intervening biological samples across all blocks.}
\examples{
library(MetProc)
#Read metabolomics data
metdata <- read.met(system.file("extdata/sampledata.csv", package="MetProc"),
headrow=3, metidcol=1, fvalue=8, sep=",", ppkey="PPP", ippkey="BPP")

#Get indices of samples and pooled plasma
grps <- get_group(metdata,'PPP','X') 

#get correlation metrics of metabolites
corrs <- corr_metric(metdata,grps) 
}
\seealso{
See \code{\link{MetProc-package}} for examples of running the full process.
}

