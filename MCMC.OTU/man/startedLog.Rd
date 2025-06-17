\name{startedLog}
\alias{startedLog}
\title{
prepares OTU counts data for PCA analysis using started-log transform
}
\description{
Normalizes counts based on total counts per sample, adds a constant number
(argument logstart) to get rid of zeroes, then log-transforms the data.  
}
\usage{
startedLog(data,count.columns,logstart=0.1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
OTU counts table. Rows: samples, columns: OTUs.
}
  \item{count.columns}{
numbers of colums containing OTU counts.
}
  \item{logstart}{
number to add to zero counts before log transform. With 'logstart=0', the zero count datapoints become 'NA' (useful for analysis of pairwise correlations between OTUs).
}
}
\value{
A table of log-transformed normalized values.
}
\references{
Started Log transform: 
Rocke DM and Durbin B Bioinformatics 2003 19:966-972 (DOI: 10.1093/bioinformatics/btg107)
MCMC.OTU:
Elizabeth A. Green, Sarah W. Davies, Mikhail V. Matz, Monica Medina Next-generation sequencing reveals cryptic Symbiodinium diversity within Orbicella faveolata and Orbicella franksi at the Flower Garden Banks, Gulf of Mexico. PeerJ 2014 https://peerj.com/preprints/246/ 
}
\author{
Mikhail V. Matz, University of Texas at Austin
<matz@utexas.edu>
}
