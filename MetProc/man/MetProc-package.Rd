\name{MetProc-package}
\alias{MetProc-package}
\alias{MetProc}
\docType{package}
\title{Separate Untargeted Metabolites into Likely Artifacts and Likely True Metabolites}
\description{Package to separate metabolites from an untargeted metabolomics experiment into likely artifacts versus likely true metabolites. The general strategy is to compare missing rates of pooled plasma samples and missing rates of biological samples across an injection order. With a randomized injection order for biological samples, generally metabolites that are present for only certain sections of the entire run (exhibiting a block structure) are likely artifacts whereas metabolites with random patterns of missingness are likely true metabolites. The package uses 3 main metrics to separate metabolites and provides tools to plot patterns of missing data across injection order to visualize differences in likely artifacts compared to true metabolites. Details of the separation process and applied metrics can be found in the details section of \code{\link{met_proc}}.}
\details{
\tabular{ll}{
Package: \tab MetProc\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2016-05-18\cr
License: \tab GPL (>= 2)\cr
}
If data is formatted appropriately (see \code{\link{sampledata}} for an example), generally only need to use the \code{\link{read.met}} function followed by the \code{\link{met_proc}} function to output a separate dataframe for likely true metabolites and likely measurement artifacts.
}
\author{
Mark Chaffin

Maintainer: Mark Chaffin <mac8278@mail.harvard.edu>
}

\examples{
library(MetProc)
#Read in metabolomics dataset
metdata <- read.met(system.file("extdata/sampledata.csv", package="MetProc"),
headrow = 3, metidcol = 1, fvalue = 8, sep = ",", ppkey = "PPP", ippkey = "BPP") 

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
#and saves them in a PDF in the current directory. Adding plots
#may substantially increase running time if many samples are 
#included
results <- met_proc(metdata, plot = TRUE, missratecut = 0.001, 
histcolors = c('red','yellow','green','blue','purple'))

#Write the retained metabolites to current directory
write.met(results,'sample_retained.csv',
system.file("extdata/sampledata.csv", package="MetProc"),
headrow=3,metidcol=1,fvalue=8,sep=",",type='keep')

#Write the removed metabolites to current directory
write.met(results,'sample_removed.csv',
system.file("extdata/sampledata.csv", package="MetProc"),
headrow=3,metidcol=1,fvalue=8,sep=",",type='remove')
}
