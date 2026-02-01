\name{cal.pos}
\alias{cal.pos}

\title{Estimate genetic map distances }

\description{
Calculation of genetic map distances for an estimated markers order from either \code{net.map} or \code{buildMap} functions. This function is only for diploid populations. We note that the output of \code{net.map} and \code{buildMap} functions include estimated linkage groups and estimated markers order within each linkage group. 
}

\usage{
cal.pos (netgwasmap, pop.type= NULL , map.func = "haldane", chr )
}

\arguments{
	\item{netgwasmap}{A \code{netgwasmap} object. The output of \code{\link{netmap}} or \code{\link{buildMap}} functions.}
	\item{pop.type}{Character string specifying the population type of the genotype data. Accepted values are "DH" (doubled haploid), "BC" (backcross), "RILn" (non-advanced RIL population with n generations of selfing) and "ARIL" (advanced RIL) (see Details). }
	\item{map.func}{Character string defining the distance function used for calculation of genetic
distances. Options are "kosambi", "haldane", and "morgan". Default is "haldane".}
	\item{chr}{A character string of linkage group names that require calculating of their genetic map distances.}
	}

\details{
 In \pkg{qtl} package, the genotype data for a backcross is coded as NA = missing, 1 = AA, 2 = AB. For an F2 intercross, the coding is NA = missing, 1 = AA, 2 = AB, 3 = BB, 4 = not BB (i.e. AA or AB), 5 = not AA (i.e. AB or BB).
 
If \code{pop.typ = "RILn"} the number of generations of selfing is limited to 20 to ensure sensible input. The constructed object is returned as a R/qtl \code{cross} object with the appropriate class structure. For \code{"RILn"} populations the constructed object is given the class \code{"bcsft"} by using the \pkg{qtl} package conversion function \code{convert2bcsft} with arguments \code{F.gen = n} and \code{BC.gen = 0}. For \code{"ARIL"} populations the constructed object is given the class \code{"riself"}.

This function uses the Viterbi algorithm implemented in \code{argmax.geno} of the \pkg{qtl} package to estimate genetic distances. Initial conservative estimates of the map distances are calculated from inverting recombination fractions outputted from \code{est.rf}. These are then passed to \code{argmax.geno} and imputation of missing allele scores is performed along with re-estimation of map distances. This is an adapted version of \code{quickEst} function from \pkg{ASMap} package.
}

\value{
The \code{netgwas} constructed linkage map is returned as a \code{R/qtl} cross object. The object is a list with usual components \code{"pheno"} and \code{"geno"}.

 \item{geno}{The \code{"geno"} element contains \code{data} and \code{map} for separated linkage groups which have been constructed using \code{net.map} function. }
 \item{pheno}{Character string containing the genotype names.}
}

\author{ 
  Pariya Behrouzi \cr
  Maintainer: Pariya Behrouzi \email{pariya.behrouzi@gmail.com}\cr
}

\examples{
\dontrun{
sim <- simRIL(d=25, n=200, g=5, cM=100, selfing= 2)
 # to use the same genotyping coding as qtl package (See details)
sim$data <- (sim$data) + 1 

 #Estimate linkage groups and order markers within each LG
out <- netmap(sim$data, cross = "inbred")
map <- out$map; map

plot(out)

 #Calculate map positions and convert the map to cross object from qtl package
pos.map <- cal.pos(netgwasmap = out, pop.type= "RIL2", map.func = "haldane" )
plotMap(pos.map)
}
}