\name{data}
\alias{golub}
\alias{sim_class}
\alias{sim_profile}
\alias{testcmr}
\title{
Data sets for the clusterCons package
}
\description{
These data sets are used by the examples in the package function descriptions and allow the user to explore the functionality of the package
}
\usage{
data(golub);
data(sim_class);
data(sim_profile);
data(testcmr);
}

\format{
golub : data.frame of gene expression values for 999 genes for 38 leukemia patients (1-27) ALL and (28-38) AML.
sim_class : data.frame of 200 simulated gene expression values for 30 conditions where there are 4 discrete classes of expression profile, for testing clustering with the transposed data (clustering by column).
sim_profile : data.frame of 120 simulated gene expression values for 4 conditions where there are 4 discrete classes of expression profile, for testing general clustering  (clustering by row). 
testcmr : list of consensus and merge matrix results from a \code{\link{cluscomp}} run (see \code{\link{consmatrix-class}} and \code{\link{mergematrix-class}}). 
}

\references{
Merged consensus clustering to assess and improve class discovery with microarray data. Simpson TI, Armstrong JD and Jarman AP. BMC Bioinformatics 2010, 11:590.

Molecular classification of cancer: class discovery and class prediction by gene expression monitoring. Golub, TR and Slonim, DK and Tamayo, P and Huard, C and Gaasenbeek, M and Mesirov, JP and Coller, H and Loh, ML and Downing, JR and Caligiuri, MA and Bloomfield, CD and Lander, ES. Science 1999, 286:531-537
}

\author{
Dr. T. Ian Simpson
\email{ian.simpson@ed.ac.uk}
}

\examples{
#cluster by class
data(sim_class);
cutree(agnes(t(sim_class)),4);

#cluster by profile
data(sim_profile);
cutree(agnes(sim_profile),4);
}