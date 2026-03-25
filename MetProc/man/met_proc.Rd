\name{met_proc}
\alias{met_proc}
\title{Separates Metabolites into Likely True Metabolites and Likely Measurement Artifacts}
\description{Takes a metabolomics data matrix and processes metabolites into likely artifacts versus likely true metabolites. Biological samples should follow a randomized injection order with pooled plasma samples interspersed. Columns of data should be samples and rows are metabolites. Columns must be ordered by injection order. Metabolites are first grouped by missing rate of pooled plasma and then processed based on metrics of blocky structure to identify likely artifacts. Specifically, \code{\link{corr_metric}} and \code{\link{run_metric}} are used to quantify the degree to which structure is present in the patterns of missing data. Must pass all thresholds to be considered a true metabolite.}
\usage{
met_proc(df, numsplit = 5, cor_rates = c(0.6, 0.65, 0.65, 0.65, 0.6),
runlengths = c(NA, 15, 15, 15, NA), mincut = 0.02, maxcut = 0.95, scut = 0.5,
ppkey = "PPP", sidkey = "X", missratecut=0.01, histcolors=c('white'), plot=TRUE,
outfile='MetProc_output')
}
\arguments{
  \item{df}{The metabolomics dataset, ideally read from the \code{\link{read.met}} function. Each column represents a sample and each row represents a metabolite. Columns should be labeled with some unique prefix denoting whether the column is from a biological sample or pooled plasma sample. For example, all pooled plasma samples may have columns identified by the prefix \dQuote{PPP} and all biological samples may have columns identified by the prefix \dQuote{X}. Missing data must be coded as NA. Columns must be ordered by injection order.}
  \item{numsplit}{The number of equal sized sections to divide metabolites into based on missing rate of pooled plasma columns. Divides the range of missing rates between \code{mincut} and \code{maxcut} into equal sections. Default is \code{5}.}
  \item{cor_rates}{A vector of length equal to \code{numsplit}. Each value represents the cutoff of the correlation metric in that section. Any metabolite with a value greater than or equal to the cutoff is considred an artifact and anything less than the cutoff is considered a true metabolite. If any value is set to NA, the correlation metric will not be considered for that group. One cutoff per group. Default is \code{c(.6, .65, .65, .65, .6)}.}
  \item{runlengths}{A vector of length equal to \code{numsplit}. Each values represents the cutoff for the longest run metric in that section. Any metabolite with a run greater than or equal to the cutoff is considered an artifact and anything less than the cutoff is considred a true metabolite. If any value is set to NA, the longest run metric will not be considered for that group. One cutoff per group. Default is \code{c(NA, 15, 15, 15, NA)}.}
  \item{mincut}{A cutoff to specify that any metabolite with pooled plasma missing rate less than or equal to this value should be retained. Default is \code{0.02}.}
  \item{maxcut}{A cutoff to specify that any metabolite with pooled plasma missing rate greater than this value should be removed. Default is \code{0.95}.}
  \item{scut}{The cutoff of missingness to consider a metabolite as having data present in a given biological sample block. Relevant only to \code{\link{run_metric}} computation. Default is \code{0.5}.}
  \item{ppkey}{The unique prefix of pooled plasma columns. Default is \code{"PPP"}.}
  \item{sidkey}{The unique prefix of biological samples columns. Default is \code{"X"}.}
  \item{missratecut}{A parameter for heatmap plots when \code{plot=TRUE}. Only metabolites with missing rates (across pooled plasma and biological samples) equal to or greater than this cutoff will be plotted. Useful to avoid plotting too may metabolites in an effort to save time. If a metabolite has a very small missing rate, plotting is uninformative as all data is present. Default is \code{0.01}.}
  \item{plot}{Indicate whether you would like to obtain plots of missingness patterns and distributions of calculated metrics. Plots will be output as a PDF. Default is \code{TRUE}.}
  \item{histcolors}{A vector of length equal to \code{numsplit}. Each value represents the color to use for that group in the histograms of the longest run and correlation metrics for each subset of metabolites. If no color is provided, they will be colored white.}
  \item{outfile}{Name and path of the file to store images if \code{plot=TRUE}. Do not include file extension in the name. Default is \code{"MetProc_output"} which will save a file called MetProc_output.pdf in the current working directory.}
}
\value{
  \item{keep}{A dataframe of the retained metabolites}
  \item{remove}{A dataframe of the removed metabolites}
 If \code{plot = True}, a PDF file will be saved containing the correspondence between pooled plasma missing rate and sample missing rate, the distribution of the correlation metric and longest run metric in each of the groups based on pooled plasma missing rates, and heatmaps displaying the patterns of present/missing data for both the removed and retained metabolites.
}
\details{The function uses a four step process:\cr\cr
1. Retain all metabolites with pooled plasma missing rate below \code{mincut} and remove all metabolites with pooled plasma missing rate above \code{maxcut}.\cr
2. Split the remaining metabolites into \code{numsplit} groups that are defined by pooled plasma missing rates. The \code{numsplit} groups will divide the range of pooled plasma missing rates evenly.\cr
3. For each group of metabolites based on pooled plasma missing rates from step 2, calculate the correlation metric with \code{\link{corr_metric}}. Any metabolite below the cutoff for that group, defined by \code{cor_rates}, will be retained and any metabolite above will be removed.\cr
4. For each group of metabolites based on pooled plasma missing rates from step 2, calculate the longest run metric with \code{\link{run_metric}}. Any metabolite below the cutoff for that group, defined by \code{runlengths}, will be retained and any metabolite above will be removed.
}
\examples{
library(MetProc)

#Read in metabolomics data
metdata <- read.met(system.file("extdata/sampledata.csv", package="MetProc"),
headrow=3, metidcol=1, fvalue=8, sep=",", ppkey="PPP", ippkey="BPP")

#Separate likely artifacts from true signal using default settings
results <- met_proc(metdata,plot=FALSE)

#Separate likely artifacts from true signal using custom cutoffs and criteria
#Uses 5 groups of metabolites based on the pooled plasma missing rate, applies
#custom metric thersholds, sets the minimum pooled plasma missing rate to 0.05,
#sets the maximum pooled plasma missing rate to 0.95, sets the missing rate
#to consider a block of samples present at 0.6
results <- met_proc(metdata, numsplit = 5, cor_rates = c(0.4,.7,.75,.7,.4),
runlengths = c(80, 10, 12, 10, 80), mincut = 0.05, maxcut = 0.95, scut = 0.6,
ppkey = 'PPP', sidkey = 'X', plot = FALSE)

#Uses default criteria for running met_proc, but plots the results
#and saves them in a PDF in the current directory.
#Colors of the histograms set by histcolors.
#Adding plots may substantially increase running time if many
#samples are included
results <- met_proc(metdata, plot = TRUE, missratecut = 0.001, 
histcolors = c('red','yellow','green','blue','purple'))
}
\seealso{See \code{\link{run_metric}} for details on the longest run metric.\cr See \code{\link{corr_metric}} for details on the correlation metric.\cr See \code{\link{MetProc-package}} for examples of running the full process.
}
