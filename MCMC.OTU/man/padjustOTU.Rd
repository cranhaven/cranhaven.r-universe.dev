\name{padjustOTU}
\alias{padjustOTU}
\title{
Adjusts p-values in the OTU summary for multiple comparisons.
}
\description{
Collects all p-values from the summary, performs FDR correction (Benjamini-Hochberg),
returns the summary with adjusted p-values.
}
\usage{
padjustOTU(otu.summary, method="BH")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{otu.summary}{
The output of OTUsummary function.
}
  \item{method}{
Method of adjustment for multiple comparisons (see ?p.adjust)
}
}
\value{
An OTU summary list (see ?OTUsummary) with adjusted p-values
}
\references{
Elizabeth A. Green, Sarah W. Davies, Mikhail V. Matz, Monica Medina Next-generation sequencing reveals cryptic Symbiodinium diversity within Orbicella faveolata and Orbicella franksi at the Flower Garden Banks, Gulf of Mexico. PeerJ 2014 https://peerj.com/preprints/246/ 
}
\author{
Mikhail V. Matz, University of Texas at Austin
<matz@utexas.edu>
}
