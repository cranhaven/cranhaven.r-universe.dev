\name{signifOTU}
\alias{signifOTU}
\title{
Finds differentially represented OTUs.
}
\description{
Trawls otuWise summary tables in the data returned by OTUsummary
and returns the names of OTUs that have p-values better than 
the specified cutoff.
}
\usage{
signifOTU(otu.summary, p.cutoff=0.05)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{otu.summary}{
The output of OTUsummary function.
}
  \item{p.cutoff}{
p-value cutoff to report OTUs.
}
}
\value{
A vector of significant OTUs.
}
\references{
Elizabeth A. Green, Sarah W. Davies, Mikhail V. Matz, Monica Medina Next-generation sequencing reveals cryptic Symbiodinium diversity within Orbicella faveolata and Orbicella franksi at the Flower Garden Banks, Gulf of Mexico. PeerJ 2014 https://peerj.com/preprints/246/ 
}
\author{
Mikhail V. Matz, University of Texas at Austin
<matz@utexas.edu>
}
