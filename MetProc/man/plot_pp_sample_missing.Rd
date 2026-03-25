\name{plot_pp_sample_missing}
\alias{plot_pp_sample_missing}
\title{Plot Pooled Plasma and Biological Sample Missing Rates}
\description{Calculates the missing rate of the pooled plasma columns and biological sample columns for each metabolite. Plots a scatterplot showing the correspondence between the two.}
\usage{
plot_pp_sample_missing(df, ppkey = "PPP", sidkey = "X")
}
\arguments{
  \item{df}{The metabolomics dataset, ideally read from the \code{\link{read.met}} function. Each column represents a sample and each row represents a metabolite. Columns should be labeled with some unique prefix denoting whether the column is from a biological sample or pooled plasma sample. For example, all pooled plasma samples may have columns identified by the prefix \dQuote{PPP} and all biological samples may have columns identified by the prefix \dQuote{X}. Missing data must be coded as NA. Columns must be ordered by injection order.}
  \item{ppkey}{The unique prefix of pooled plasma samples. Default is \code{"PPP"}.}
  \item{sidkey}{The unique prefix of biological samples. Default is \code{"X"}.}
}
\value{Returns a scatterplot comparing the pooled plasma missing rate to the sample missing rate}
\examples{
library(MetProc)

#Read in metabolomics data
metdata <- read.met(system.file("extdata/sampledata.csv", package="MetProc"),
headrow=3, metidcol=1, fvalue=8, sep=",", ppkey="PPP", ippkey="BPP")

#Plot the pooled plasma missing rate against the sample missing rate
plot_pp_sample_missing(metdata,ppkey='PPP',sidkey='X')
}
\seealso{
See \code{\link{MetProc-package}} for examples of running the full process.
}
