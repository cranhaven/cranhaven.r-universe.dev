#' @title Principal Coordinates of Phylogenetic Structure
#' 
#' @description Function to generate Principal Coordinates of Phylogenetic Structure (PCPS).
#' 
#' @details The function obtains a matrix containing phylogeny-weighted species composition 
#' (\code{\link{matrix.p}}) and is submitted to principal coordinates analysis (PCoA). 
#' This method generates the principal coordinates of phylogenetic structure 
#' (PCPS) (Duarte, 2011).
#' 
#' The sequence species show up in the community data matrix must be the 
#' same as they show up in the phylogenetic distance matrix. The 
#' function \code{\link{organize.pcps}} organizes the data, placing the matrices of 
#' community and phylogenetic distance in the same order. The use of  
#' organize.pcps is not requered for run this function, but is recommended. In this way 
#' the arguments comm and phylodist can be specified them as normal arguments or by passing
#' them with the object returned by the function \code{\link{organize.pcps}} using, in this
#' case only the argument comm. Using the object returned by organize.pcps, the comm argument 
#' is used as an alternative way of entering to set all data.frames/matrices, and therefore 
#' the phylodist argument must not be specified.
#' 
#' The function summary or the function scores.pcps re-scales the correlation values 
#' for obtain the scores for \code{\link{biplot}} graphics. The function plot draws a 
#' simple biplot and represent clades as "spider" graphs (see \code{\link{ordispider}}).
#' 
#' @encoding UTF-8
#' @import SYNCSA
#' @importFrom stats cor
#' @importFrom vegan ordispider wcmdscale ordilabel vegdist
#' @importFrom graphics plot points text
#' @aliases pcps print.pcps summary.pcps print.summarypcps plot.pcps scores.pcps
#' @param comm Community data, with species as columns and sampling units as rows. 
#' This matrix can contain either presence/absence or abundance data.
#' Alternatively comm can be an object of class metacommunity.data, an alternative
#' way to set all data.frames/matrices. When you use the class metacommunity.data the argument
#' phylodist must not be specified. See details.
#' @param phylodist Matrix containing phylogenetic distances between species.
#' @param checkdata Logical argument (TRUE or FALSE) to check if species
#' sequence in the community data follows the same order as the one in the phylodist 
#' matrix (Default checkdata = TRUE).
#' @param method Dissimilarity index, as accepted by \code{\link{vegdist}} (Default dist="bray").
#' @param squareroot Logical argument (TRUE or FALSE) to specify if use square root of 
#' dissimilarity index (Default squareroot = TRUE).
#' @param correlations Logical argument (TRUE or FALSE) to specify if are calculed the correlations
#' between each PCPS and each species in matrix P (Default correlations = TRUE).
#' @param object An object of class pcps.
#' @param x An object of class pcps.
#' @param choices Axes for re-scaling. Choices must have length equal to two (Default choices = c(1, 2)).
#' @param display Display text or points for the sampling units, partial match to "text" or "points" (Default display = "text").
#' @param groups Factor giving the groups (Clades) for each species  (Default groups = NULL).
#' @param showlabel Label the groups by their names in the centroid of the object.
#' @param ... Other parameters for the respective functions.
#' @return \item{P}{Phylogeny-weighted species composition matrix.} \item{values}{The eigenvalues, 
#' relative eigenvalues and cumulative relative eigenvalues.} \item{vectors}{The principal coordinates
#' of phylogenetic structure (PCPS).} \item{correlations}{Correlations between a PCPS axis and 
#' phylogenetically weighted species abundances or frequencies.} \item{scores}{Scores for biplot graphics.}
#' @note \strong{IMPORTANT}: The sequence species show up in the community data matrix MUST be the 
#' same as they show up in the phylogenetic distance matrix. See details and \code{\link{organize.pcps}}.
#' @author Vanderlei Julio Debastiani <vanderleidebastiani@@yahoo.com.br>
#' @seealso \code{\link{matrix.p}}, \code{\link{wcmdscale}}, \code{\link{ordispider}}, \code{\link{ordilabel}}
#' @references Duarte, L.S. (2011). Phylogenetic habitat filtering influences forest nucleation 
#' in grasslands. Oikos, 120, 208:215.
#' @keywords PCPS
#' @examples
#'
#' data(ADRS)
#' res<-pcps(ADRS$community, ADRS$phylo)
#' res
#' summary(res)
#' summary(res, choices = c(1, 2))$scores
#' plot(res, display = "text", groups = c(rep("Clade-A", 2), rep("Clade-B", 4)))
#'
#' @export
pcps <- function(comm, phylodist, checkdata = TRUE, 
                 method = "bray", squareroot = TRUE, correlations = TRUE){
  res <- list(call = match.call())
  if (inherits(comm, "metacommunity.data")) {
    if (!missing(phylodist)) {
      stop("\n When you use an object of class metacommunity.data the argument phylodist must not be specified. \n")
    }
    phylodist <- comm$phylodist
    comm <- comm$community
  }
  list.warning <- list()
  if(checkdata){
    organize.temp <- organize.pcps(comm, phylodist = phylodist, check.comm = TRUE)
    if(!is.null(organize.temp$stop)){
      organize.temp$call <- match.call()
      return(organize.temp)
    }
    list.warning <- organize.temp$list.warning
    comm <- organize.temp$community
    phylodist <- organize.temp$phylodist
  }
  if(length(list.warning)>0){
    res$list.warning <- list.warning
  }
	P <- SYNCSA::matrix.p(comm, phylodist, notification = FALSE)$matrix.P
	res$P <- P
	ord.P <- wcmdscale.org(P, method = method, squareroot = squareroot, eig = TRUE, correlations = correlations)
	res$values <- ord.P$values
	res$vectors <- ord.P$vectors
	colnames(res$vectors) <- paste("pcps.", seq_len(ncol(res$vectors)), sep = "")
	row.names(res$values) <- colnames(res$vectors)
	if(correlations){
	  res$correlations <- ord.P$correlations
	  colnames(res$correlations) <- paste("pcps.", seq_len(ncol(res$vectors)), sep = "")
		rownames(res$correlations) <- rownames(res$correlations, do.NULL = FALSE, prefix = "spp.")
	}
	class(res) <- "pcps"
	return(res)
}