\name{get_group}
\alias{get_group}
\title{Retrieve Index of Biological Samples and Pooled Plasma Samples}
\description{Takes a metabolomics data matrix and retrieves the column indices of biological samples and pooled plasma samples. Columns must be ordered by injection order and each column ID should have a unique prefix designating the particular type of sample it is. For example, \dQuote{PPP}' to designate pooled plasma samples and \dQuote{X} to designate biological samples. Generally if data is read into R using the \code{\link{read.met}} function, columns will be labeled appropriately.}
\usage{get_group(df, ppkey = "PPP", sidkey = "X")}
\arguments{
  \item{df}{The metabolomics dataset, ideally read from the \code{\link{read.met}} function. Each column represents a sample and each row represents a metabolite. Columns should be labeled with some unique prefix denoting whether the column is from a biological sample or pooled plasma sample. For example, all pooled plasma samples may have columns identified by the prefix \dQuote{PPP} and all biological samples may have columns identified by the prefix \dQuote{X}. Missing data must be coded as NA. Columns must be ordered by injection order.}
  \item{ppkey}{The unique prefix of pooled plasma samples. Default is \code{"PPP"}.}
  \item{sidkey}{The unique prefix of biological samples. Default is \code{"X"}.}
}
\value{A list of length 2 with the following keys:\cr
\item{pp}{A vector with column indices of pooled plasma}
\item{sid}{A vector with column indices of samples}
}
\examples{
library(MetProc)
#Read metabolomics data
metdata <- read.met(system.file("extdata/sampledata.csv", package="MetProc"),
headrow=3, metidcol=1, fvalue=8, sep=",", ppkey="PPP", ippkey="BPP")

#Get groups based on samples and pooled plasma
grps <- get_group(metdata,'PPP','X') 
}
\seealso{
See \code{\link{MetProc-package}} for examples of running the full process.
}

