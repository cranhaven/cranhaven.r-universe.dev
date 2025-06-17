\name{logLin}
\alias{logLin}
\title{
prepares OTU counts data for PCA analysis using log-linear-hybrid transform
}
\description{
Normalizes counts based on total counts per sample, then uses log-linear-hybrid
transformation on the data. 
}
\usage{
logLin(data,count.columns,k=10,zero.na=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
OTU counts table. Rows: samples, columns: OTUs.
}
  \item{count.columns}{
numbers of colums containing OTU counts.
}
  \item{k}{
count threshold to transition from linear to log transform.
}
  \item{zero.na}{
With 'zero.na=TRUE', the original zero count datapoints become 'NA' 
(useful for analysis of pairwise correlations between OTUs).
}
}
\value{
A table of transformed normalized values.
}
\references{
Log-linear-hybrid transform: 
Rocke DM and Durbin B Bioinformatics 2003 19:966-972 (DOI: 10.1093/bioinformatics/btg107)
MCMC.OTU:
Elizabeth A. Green, Sarah W. Davies, Mikhail V. Matz, Monica Medina Next-generation sequencing reveals cryptic Symbiodinium diversity within Orbicella faveolata and Orbicella franksi at the Flower Garden Banks, Gulf of Mexico. PeerJ 2014 https://peerj.com/preprints/246/ 
}
\author{
Mikhail V. Matz, University of Texas at Austin
<matz@utexas.edu>
}
