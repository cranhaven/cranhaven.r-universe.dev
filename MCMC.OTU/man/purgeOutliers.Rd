\name{purgeOutliers}
\alias{purgeOutliers}
\title{
Removes outlier samples and OTUs.
}
\description{
Samples are removed if their total counts fall below the specified z-score cutoff (default -2.5); OTUs are removed if their sum of counts is less than specified fraction of total counts (default 0.001), or if they are observed in less than specified fraction of all samples (default 0.2). Samples are removed first, then OTUs.
}
\usage{
purgeOutliers(data, count.columns, sampleZcut=(-2.5), otu.cut=0.001, zero.cut=0)
}
\arguments{
  \item{data}{
A data frame containing counts, with samples as rows and OTUs as columns
}
  \item{count.columns}{
Vector of OTU column numbers.
}
  \item{sampleZcut}{
z-score cutoff for removing low-coverage outlier samples.
}
  \item{otu.cut}{
cutoff for a fraction of total counts the OTU has to represent. This would depend on the methodology and nature of OTUs; for coral symbionts (Symbiodinium sp) the reasonable cutoff is 0.001 (Quigley et al, PLoS ONE 2014)
}
  \item{zero.cut}{
fraction of samples in which the OTU is found.  
}
}
\value{
The data table with outlier samples and OTUs removed.
}
\references{
Elizabeth A. Green, Sarah W. Davies, Mikhail V. Matz, Monica Medina Next-generation sequencing reveals cryptic Symbiodinium diversity within Orbicella faveolata and Orbicella franksi at the Flower Garden Banks, Gulf of Mexico. PeerJ 2014 https://peerj.com/preprints/246/ 
}
\author{
Mikhail V. Matz, University of Texas at Austin
<matz@utexas.edu>
}
