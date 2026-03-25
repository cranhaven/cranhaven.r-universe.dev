\name{run_metric}
\alias{run_metric}
\title{Calculate Longest Run of Blocks where Data is Present}
\description{For each metabolite, data is split into blocks that consist of the preceding pooled plasma sample and following biological samples in an injection order. For each block, data is deemed present in biological samples if the missing rate is less than \code{scut}. An entire block is deemed to have data present if both the preceding pooled plasma and folllowing biolgical samples are both considered to have data present. The length of the longest consecutive run of blocks with data present is returned for each metabolite.}
\usage{
run_metric(df, grps, scut = 0.5)
}
\arguments{
  \item{df}{The metabolomics dataset, ideally read from the \code{\link{read.met}} function. Each column represents a sample and each row represents a metabolite. Columns should be labeled with some unique prefix denoting whether the column is from a biological sample or pooled plasma sample. For example, all pooled plasma samples may have columns identified by the prefix \dQuote{PPP} and all biological samples may have columns identified by the prefix \dQuote{X}. Missing data must be coded as NA. Columns must be ordered by injection order.}
  \item{grps}{A group list from the \code{\link{get_group}} function}
  \item{scut}{The cutoff missing rate to determine if data is present in a group of biological samples. If the missing rate of the biological samples is greater than or equal to this missing rate threshold, data will be considered absent from the block of biological samples. Default is \code{0.5}.}
}
\value{Returns a vector containing the longest consecutive run of blocks with data present for each metabolite}

\examples{
library(MetProc)

#Read in metabolomics data
metdata <- read.met(system.file("extdata/sampledata.csv", package="MetProc"),
headrow=3, metidcol=1, fvalue=8, sep=",", ppkey="PPP", ippkey="BPP")

#Get indices of pooled plasma and samples
grps <- get_group(metdata,'PPP','X')

#Get the longest run metric for each metabolite
runs <- run_metric(metdata,grps,scut=.5)
}
\seealso{
See \code{\link{MetProc-package}} for examples of running the full process.
}

