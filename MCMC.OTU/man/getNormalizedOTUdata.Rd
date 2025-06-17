\name{getNormalizedOTUdata}
\alias{getNormalizedOTUdata}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Extracts mcmc.otu model predictions
}
\description{
Generates a table of model-derived log2-transformed OTU abundances 
without sample effects (i.e., corresponding to predicted normalized data)
}
\usage{
getNormalizedOTUdata(model, data, log.base=10)
}
\arguments{
  \item{model}{
mcmc.otu model: the output of mcmc.otu function fitted with pr=TRUE option 
}
  \item{data}{
The dataset that was analysed to generate the model (output of otuStack function)
}
  \item{log.base}{
base of the logarithm to use
}
}
\value{
The function returns a list of two data frames. The first one, normData, is the model-predicted log2-transformed OTU abundances table. It has one column per OTU and one row per sample. The second data frame, conditions, is a table of experimental conditions corresponding to the normData table.
}
\references{
Elizabeth A. Green, Sarah W. Davies, Mikhail V. Matz, Monica Medina Next-generation sequencing reveals cryptic Symbiodinium diversity within Orbicella faveolata and Orbicella franksi at the Flower Garden Banks, Gulf of Mexico. PeerJ 2014 https://peerj.com/preprints/246/ 
}
\author{
Mikhail V. Matz, University of Texas at Austin
<matz@utexas.edu>
}
