#' @title Auxiliar function to generate sets of null P matrix or null PCPS
#' 
#' @description Auxiliar function to generate sets of null P matrix or null PCPS used 
#' in \code{\link{matrix.p.sig}} or \code{\link{pcps.sig}}. The result are long lists with 
#' permuted matrices.
#' 
#' @encoding UTF-8
#' @include pcps.R
#' @import SYNCSA
#' @importFrom stats fitted
#' @aliases matrix.p.null
#' @param comm Community data, with species as columns and sampling units as rows. This matrix 
#' can contain either presence/absence or abundance data.
#' @param phylodist Matrix containing phylogenetic distances between species.
#' @param runs Number of matrix will be generated (Default runs = NULL).
#' @param calcpcps Logical argument (TRUE or FALSE) to specify if generate the PCPS (Default calcpcps = FALSE).
#' @param method Dissimilarity index, as accepted by \code{\link{vegdist}} (Default dist = "bray").
#' @param squareroot Logical argument (TRUE or FALSE) to specify if use square root of 
#' dissimilarity index (Default squareroot = TRUE).
#' @param adjpcps Logical argument (TRUE or FALSE) to specify if return fitted PCPS (Default adjpcps = FALSE).
#' @param choices Numeric vector to choose the PCPS to adjust (Default pcps.choices = NULL).
#' @return \item{call}{The arguments used.}
#' \item{P.obs}{Observed phylogeny-weighted species composition matrix.}
#' \item{pcps.obs}{Observed principal coordinates of phylogenetic structure (PCPS).}
#' \item{permutation.site}{A matrix with sequence of permutation for site shuffle null model, each permutation in one row.}
#' \item{permutation.taxa}{A matrix with sequence of permutation for taxa shuffle null model, each permutation in one row.}
#' \item{P.null.site}{A list with each permuted P matrix according with site shuffle null model.}
#' \item{P.null.taxa}{A list with each permuted P matrix according with taxa shuffle null model.}
#' \item{pcps.null.site}{A list with each permuted PCPS according with site shuffle null model.}
#' \item{pcps.null.taxa}{A list with each permuted PCPS according with taxa shuffle null model.}
#' \item{pcps.null.taxa.adj}{A list with each permuted PCPS (adjusted) according with taxa shuffle null model.}
#' @author Vanderlei Julio Debastiani <vanderleidebastiani@@yahoo.com.br>
#' @seealso \code{\link{matrix.p}}, \code{\link{pcps}}, \code{\link{matrix.p.sig}}, \code{\link{pcps.sig}}
#' @keywords PCPS
#' @export
matrix.p.null <- function(comm, phylodist, runs = NULL, calcpcps = FALSE, method = "bray", squareroot = TRUE, adjpcps = FALSE, choices = NULL){
  RES <- list(call = match.call())
  p.n.site <- function(samp, P){
    MP.null <- SYNCSA::permut.row.matrix(P, seqpermutation = samp)$permut.matrix
    return(MP.null)
  }
  p.n.taxa <- function(samp, comm, phylodist){
    MP.null <- SYNCSA::matrix.p(comm, phylodist[samp, samp], notification = FALSE)$matrix.P
    return(MP.null)
  }
  PtoPCPS <- function(P, method, squareroot){
    ord.P <- wcmdscale.org(P, method = method, squareroot = squareroot, eig = FALSE, correlations = FALSE)$vectors
    colnames(ord.P) <- paste("pcps.", seq_len(ncol(ord.P)), sep = "")
    return(ord.P)
  }
  PCPSfitted <- function(Mnull, Mobs, choices){
    res <- stats::fitted(vegan::procrustes(Mobs[, choices, drop = FALSE], Mnull[, choices, drop = FALSE], symmetric = TRUE))
    colnames(res) <- colnames(Mobs)[choices]
    return(res)
  }
  RES$P.obs <- SYNCSA::matrix.p(comm, phylodist, notification = FALSE)$matrix.P
  if(calcpcps){
    RES$pcps.obs <- PtoPCPS(RES$P.obs, method = method, squareroot = squareroot)
  }
  if(!is.null(runs)){
    seqpermutation.site <- SYNCSA::permut.vector(nrow(comm), nset = runs)
    RES$permutation.site <- seqpermutation.site
    seqpermutation.site <- lapply(seq_len(nrow(seqpermutation.site)), function(i) seqpermutation.site[i,])
    seqpermutation.taxa <- SYNCSA::permut.vector(ncol(comm), nset = runs)
    RES$permutation.taxa <- seqpermutation.taxa
    seqpermutation.taxa <- lapply(seq_len(nrow(seqpermutation.taxa)), function(i) seqpermutation.taxa[i,])
    RES$P.null.site <- lapply(seqpermutation.site, p.n.site, P = RES$P.obs)
    RES$P.null.taxa <- lapply(seqpermutation.taxa, p.n.taxa, comm = comm, phylodist = phylodist)
    if(calcpcps){
      RES$pcps.null.site <- sapply(RES$P.null.site, PtoPCPS, simplify = FALSE, method = method, squareroot = squareroot)
      RES$pcps.null.taxa <- sapply(RES$P.null.taxa, PtoPCPS, simplify = FALSE, method = method, squareroot = squareroot)
      if(adjpcps){
        if(is.null(choices)){
          stop("choices must not be null when adjpcps is TRUE")
        }
        RES$pcps.null.taxa.adj <- sapply(RES$pcps.null.taxa, PCPSfitted, simplify = FALSE, Mobs = RES$pcps.obs, choices = choices)
      }
    }
  }
  return(RES)
}