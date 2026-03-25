\name{read.met}
\alias{read.met}
\title{Read in a Metabolomics Dataset of Standard Structure}
\description{Read a metabolomics file. The file must be structured in a specific way. The columns of the file designate samples and the rows designate metabolites. The first n rows may contain any information. However, starting at row n+1 there must be a header line with column labels. The remaining rows are designated as one per metabolite. One column should contain the ID of each metabolite. Other columns can be included, but starting at some column, and continuously after this point, each sample or pooled plasma sample should be given its own column sorted by injection order. All pooled plasma columns should have a unique prefix differentiating them from biological samples. Up to 2 types of pooled plasma samples can be included in the file -- each with a unique prefix. This may be useful when both a pooled plasma control generated from biological samples and a commercially available pooled plasma standard are used. All biological samples may have a designating prefix or simply lack a prefix designating pooled plasma samples. If no prefix designates the biological samples, a prefix of \dQuote{X} will be used for biological samples in subsequent analysis. Missing data must be coded as NA.}
\usage{
read.met(data, headrow = 3, metidcol=1, fvalue=8, sep=",", ppkey='PPP',
ippkey = 'BPP', sidkey="none")
}
\arguments{
  \item{data}{The metabolomics dataset file. The columns of the file designate samples and the rows designate metabolites. The first n rows can contain any information. However, starting at row n+1 there must be a header line with column labels. The remaining rows are designated as one per metabolite. One column should contain the ID of each metabolite. Other columns can be included, but starting at some column, and continuously after this point, each biological sample or pooled plasma sample should be given it's own column sorted by injection order. All pooled plasma columns should have a unique prefix differentiating them from samples. Up to 2 types of pooled plasma samples can be included in the file -- each with a unique prefix. All biological samples may have a designated prefix or simply lack the the prefix designating pooled plasma samples. If no prefix designates the biological samples, a prefix of \dQuote{X} will be used for biological samples in subsequent analysis. Missing data must be coded as NA. See file \code{\link{sampledata}} for an example.}
  \item{headrow}{The row number that contains the header line. Default is \code{3}.}
  \item{metidcol}{The column number that contains the metabolite ID. Default is \code{1}.}
  \item{fvalue}{The column number where data begins. Default is \code{8}.}
  \item{sep}{File delimiter. Default is \code{","}.}
  \item{ppkey}{The unique prefix of biological sample-based pooled plasma columns. Default is \code{"PPP"}.}
  \item{ippkey}{The unique prefix of standard pooled plasma columns. Default is \code{"BPP"}.}
  \item{sidkey}{The unique prefix of biological samples in the csv file. If \sQuote{none} provided as value, any column that does not contain the prefix of \code{ppkey} or \code{ippkey} will be considered a biological sample and given the prefix \sQuote{X} for subsequent use. Default is \code{"none"}.}
}
\value{
A matrix with the metabolomics data fully loaded. Should have the number of rows equal to the number of metabolites and columns equal to the number of samples + pooled plasma samples.
}

\examples{
library(MetProc)

#Read in metabolomics data
metdata <- read.met(system.file("extdata/sampledata.csv", package="MetProc"),
headrow=3, metidcol=1, fvalue=8, sep=",", ppkey="PPP", ippkey="BPP")
}
\seealso{
See \code{\link{MetProc-package}} for examples of running the full process.
}
