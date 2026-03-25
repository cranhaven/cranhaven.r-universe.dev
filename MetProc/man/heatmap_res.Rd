\name{heatmap_res}
\alias{heatmap_res}
\title{Plot Patterns of Missing Data Across Metabolites}
\description{Generates a heatmap to show patterns of missing data for metabolites. Useful to visualize the block structure of data to compare differences between removed metabolites and retained metabolites.}
\usage{heatmap_res(df, ppkey = "PPP", sidkey = "X", missratecut = .01,title)}
\arguments{
  \item{df}{The metabolomics dataset, ideally read from the \code{\link{read.met}} function. Each column represents a sample and each row represents a metabolite. Columns should be labeled with some unique prefix denoting whether the column is from a biological sample or pooled plasma sample. For example, all pooled plasma samples may have columns identified by the prefix \dQuote{PPP} and all biological samples may have columns identified by the prefix \dQuote{X}. Missing data must be coded as NA. Columns must be ordered by injection order.}
  \item{ppkey}{Unique prefix of pooled plasma columns. Default is \code{"PPP"}.}
  \item{sidkey}{Unique prefix of biological sample columns. Default is \code{"X"}.}
  \item{missratecut}{The missing rate limit for displaying a metabolite. Only metabolites with overall missing rates equal to or greater than this cutoff will be plotted. Useful for avoiding plotting too many metabolites as the heatmap generation can be an expensive computation. If a metabolite has a very small missing rate, plotting is uninformative as all data is present. Default set to \code{0.01}.}
  \item{title}{The title of the heatmap plotted}
}
\value{Returns a heatmap illustrating the patterns of missing data for metabolites.}
\examples{
library(MetProc)

#Read in metabolomics data
metdata <- read.met(system.file("extdata/sampledata.csv", package="MetProc"),
headrow=3, metidcol=1, fvalue=8, sep=",", ppkey="PPP", ippkey="BPP")

#Get the good versus bad metabolites
results <- met_proc(metdata)

#Plot Removed metabolites
#Similarly run for retained metabolites but
#replacing 'remove' with 'keep'
heatmap_res(results[['remove']],missratecut=.02,title='Removed Metabolites')
}
\seealso{
See \code{\link{MetProc-package}} for examples of running the full process.
}
