\name{plot_metric}
\alias{plot_metric}
\title{Plot Distribution of Metabolite Quality Metrics for Each Split of Data}
\description{For a given number of splits of data based on pooled plasma missing rate, calculate the longest run metric (\code{\link{run_metric}}) and the correlation metric (\code{\link{corr_metric}}) for metabolites in each group. Plot the distribution of these metrics for each group color coding those that exceed thresholds.}
\usage{
plot_metric(df,ppkey='PPP',sidkey='X',numsplit=5,mincut=.02,maxcut=0.95,
scut=0.5,cor_rates=c(.6,.65,.65,.65,.6),runlengths=c(NA,15,15,15,NA),
histcolors=c('white'))
}
\arguments{
  \item{df}{The metabolomics dataset, ideally read from the \code{\link{read.met}} function. Each column represents a sample and each row represents a metabolite. Columns should be labeled with some unique prefix denoting whether the column is from a biological sample or pooled plasma sample. For example, all pooled plasma samples may have columns identified by the prefix \dQuote{PPP} and all biological samples may have columns identified by the prefix \dQuote{X}. Missing data must be coded as NA. Columns must be ordered by injection order.}
  \item{ppkey}{The unique prefix of pooled plasma samples. Default is \code{"PPP"}.}
  \item{sidkey}{The unique prefix of biological samples. Default is \code{"X"}.}
  \item{numsplit}{The number of equal sized sections to divide metabolites into based on missing rate of pooled plasma columns. Divides the range of missing rates between \code{mincut} and \code{maxcut} into equal sections. Default is \code{5}.}
  \item{mincut}{A cutoff to specify that any metabolite with pooled plasma missing rate less than or equal to this value should be retained. Default is \code{0.02}.}
  \item{maxcut}{A cutoff to specify that any metabolite with pooled plasma missing rate greater than this value should be removed. Default is \code{0.95}.}
  \item{scut}{The cutoff of missingness to consider a metabolite as having data present in a given biological sample block. Relevant only to \code{\link{run_metric}} computation. Default is \code{0.5}.}
  \item{cor_rates}{A vector of length equal to \code{numsplit}. Each value represents the cutoff of the correlation metric in that section. Any metabolite with a value greater than or equal to the cutoff is deemed an artifact and anything less than the cutoff is deemed a true metabolite. If any value is set to NA, the correlation metric will not be considered for that group. Default is \code{c(.6, .65, .65, .65, .6)}.}
  \item{runlengths}{A vector of length equal to \code{numsplit}. Each values represents the cutoff for the longest run metric in that section. Any metabolite with a run greater than or equal to the cutoff is an artifact and anything less than the cutoff is a true metabolite. If any value is set to NA, the longest run metric will not be considered for that group. Default is \code{c(NA, 15, 15, 15, NA)}.}
  \item{histcolors}{A vector of length equal to \code{numsplit}. Each value represents the color to use for that group. If no color is provided, they will be colored white.}
}
\value{Returns histograms showing the correlation metric and longest run metric distributions for each group of the metabolites based on pooled plasma missing rate.}
\examples{
library(MetProc)

#Read in metabolomics data
metdata <- read.met(system.file("extdata/sampledata.csv", package="MetProc"),
headrow=3, metidcol=1, fvalue=8, sep=",", ppkey="PPP", ippkey="BPP")

#Plot distributions of the two metrics for each group
plot_metric(metdata,ppkey='PPP',sidkey='X',numsplit=5,mincut=0.02,maxcut=0.95,
scut=0.5,cor_rates=c(.6,.65,.65,.65,.6),runlengths=c(NA,15,15,15,NA),
histcolors=c('red','yellow','green','blue','purple'))
}
\seealso{
See \code{\link{MetProc-package}} for examples of running the full process.
}
