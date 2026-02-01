\name{detect.err}
\alias{detect.err}

\title{Identiying likely genotyping error}

\description{ Calculates a LOD score for each genotype, measuring the evidence for genotyping errors. This uses \code{calc.errorlod} function from R/\pkg{qtl} package.}

\usage{
detect.err(netgwas.map,  err.prob= 0.01, cutoff= 4, 
          pop.type= NULL, map.func= "haldane")
}

\arguments{
	\item{netgwas.map}{An object of class \code{netgwasmap} object (The output of \code{\link{netmap}} or \code{\link{netmap}} functions).}
	\item{err.prob}{Assumed genotyping error rate used in the calculation of the penetrance Pr(observed
genotype | true genotype).}
	\item{cutoff}{Only those genotypes with error LOD scores above this cutoff will be listed.}
	\item{pop.type}{Character string specifying the population type of the genotype data. Accepted values are "DH" (doubled haploid), "BC" (backcross), "RILn" (non-advanced RIL population with n generations of selfing) and "ARIL" (advanced RIL) (see Details).}
	\item{map.func}{Character string defining the distance function used for calculation of genetic
distances. Options are "kosambi", "haldane", and "morgan". Default is "haldane".}
	}

\value{
A data.frame with 4 columns, whose rows correspond to the genotypes that are possibly in error.
The four columns give the chromosome number, individual number, marker name, and error LOD
score.
}

\examples{
\dontrun{
sim <- simRIL(d=25, n=200, g=5, cM=100, selfing= 2)
 # to use the same genotyping coding as R/qtl package (See details)
sim$data <- (sim$data) + 1 

 #Estimate linkage groups and order markers within each LG
out <- netmap(sim$data, cross = "inbred")
map <- out$map; map
plot(out)

# A list of genotyoing error
detect.err(out, pop.type = "RIL2")
}
}