\name{netgwas2cross}
\alias{netgwas2cross}

\title{ netgwasmap object to cross object}

\description{
Convertes \code{netgwasmap} object from  \code{net.map} or \code{buildMap} functions to \code{cross} object from R/\pkg{qtl} package.
}

\usage{
   netgwas2cross(netgwasmap, pop.type= NULL, map.func = "haldane")
}

\arguments{
  \item{netgwasmap}{A \code{netgwasmap} object. The output of \code{\link{netmap}} or \code{\link{buildMap}} functions.}
  \item{pop.type}{Character string specifying the population type of the genotype data. Accepted values are "DH" (doubled haploid), "BC" (backcross), "RILn" (non-advanced RIL population with n generations of selfing) and "ARIL" (advanced RIL).}
  \item{map.func}{Character string defining the distance function used for calculation of genetic distances. Options are "kosambi", "haldane", and "morgan". Default is "haldane".}
}

\details{
  If \code{pop.typ = "RILn"} the number of generations of selfing is limited to 20 to ensure sensible input. The constructed object is returned as a R/qtl \code{cross} object with the appropriate class structure. For \code{"RILn"} populations the constructed object is given the class \code{"bcsft"} by using the \pkg{qtl} package conversion function \code{convert2bcsft} with arguments \code{F.gen = n} and \code{BC.gen = 0}. For \code{"ARIL"} populations the constructed object is given the class \code{"riself"}.
  
  In R/\pkg{qtl} package, the genotype data for a backcross is coded as NA = missing, 1 = AA, 2 = AB. For an F2 intercross, the coding is NA = missing, 1 = AA, 2 = AB, 3 = BB, 4 = not BB (i.e. AA or AB), 5 = not AA (i.e. AB or BB).
}

\value{
  The \code{netgwas.map} object is returned as a \code{cross} object form R/\pkg{qtl}. The object is a list with usual components \code{"pheno"} and \code{"geno"}.
  
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
    # to use the same genotyping coding as R/qtl package (See details)
    sim$data <- (sim$data) + 1 
    
    #Estimate linkage groups and order markers within each LG
    out <- netmap(sim$data, cross = "inbred")
    map <- out$map; map
    
    plot(out)
    
    #Calculate map positions and convert the map to cross object from qtl package
    map <- netgwas2cross(netgwasmap = out, pop.type= "RIL2", map.func = "haldane" )
    plotMap(map)
  }
}