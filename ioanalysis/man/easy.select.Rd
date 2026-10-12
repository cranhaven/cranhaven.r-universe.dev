\name{easy.select}
\alias{easy.select}

\title{Region and Sector Selection Interface}
\description{This is a user interface, answering prompts to significantly simplify choosing sectors and regions in large models. You can either search through the regions and sectors using keywords, partial phrases, or partial words. There is alternatively an option to select across the comprehensive list of all regions and then sectors. Once selections are made, you can view and edit the list once selections are made. Outputs a matrix to be input into other functions to help identify desired region-sector combinations.}

\usage{easy.select(io)}
\arguments{
  \item{io}{An \code{InputOutput} object. See \code{\link{as.inputoutput}}.}
}
\details{\code{easy.select} calls upon the \code{RS_label} object in \code{io} to sort through regions and sectors. The regions should be in the first column and sectors should be in the second.}
\value{\item{EasySelect}{ A numeric vector of class \code{EasySelect} that can be used to identify desired elements for future functions.}}

\author{John J. P. Wade}

\seealso{\code{\link{as.inputoutput}}}

