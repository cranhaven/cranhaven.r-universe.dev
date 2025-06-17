\name{otuStack}
\alias{otuStack}
\title{
Prepares OTU counts data for MCMC model fitting using mcmc.otu().
}
\description{
Stacks all OTU columns into one (count), creates a column for OTU names and
columns for experimental conditions. Adds a fake OTU called "summ" - sum of all
counts per sample.
}
\usage{
otuStack(data, count.columns, condition.columns)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
The input data table: one column per OTU, plus columns containg experimental 
conditions and the required column 'sample' denoting biological replicates.
See green.data included with the package.
}
  \item{count.columns}{
vector of numbers identifyng OTU counts columns
}
  \item{condition.columns}{
vector of numbers identifyng columns containing experimental conditions
}
}
\value{
A dataset that can be fed into mcmc.otu() function.
}
\references{
Elizabeth A. Green, Sarah W. Davies, Mikhail V. Matz, Monica Medina Next-generation sequencing reveals cryptic Symbiodinium diversity within Orbicella faveolata and Orbicella franksi at the Flower Garden Banks, Gulf of Mexico. PeerJ 2014 https://peerj.com/preprints/246/ 
}
\author{
Mikhail V. Matz, University of Texas at Austin
<matz@utexas.edu>
}
\examples{

# Symbiodinium sp diversity in two coral species at two reefs (banks)
data(green.data)
green.data

# stacking the data table
gs=otuStack(green.data,count.columns=c(4:8),condition.columns=c(1:3))
head(gs,30)

}
