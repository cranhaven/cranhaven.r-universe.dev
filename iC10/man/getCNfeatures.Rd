\name{getCNfeatures}
\alias{getCNfeatures}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Internal function for matching copy number features.
}
\description{
This function should not be called directly
}
\usage{
getCNfeatures(CN, Probes, Map, by.feat, ref, Synonyms)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{CN}{
 CN features matrix
}
  \item{Probes}{
Vector with the probes to match
}
  \item{Map}{
data.frame with the genomic description of the features to match
}
  \item{by.feat}{
"probe" or "gene", indicating if match should be done by probe position or gene name.
}
 \item{ref}{
hg18 or hg19 (only relevant if matching is done by probe position).
}
 \item{Synonyms}{
data.frame with available synonym gene names to match (only relevant if matching is done by gene name).
}
}
\value{
A matrix with the copy number features
}
\author{
Oscar M Rueda
}
