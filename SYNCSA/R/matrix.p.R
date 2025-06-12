#' @title Matrix P
#'
#' @description Function to obtain a matrix containing phylogeny-weighted species
#' composition. For more details, see \code{\link{syncsa}}.
#'
#' @encoding UTF-8
#' @param comm Community data, with species as columns and sampling units as
#' rows. This matrix can contain either presence/absence or abundance data.
#' @param phylodist Matrix containing phylogenetic distance between species.
#' Must be a complete matrix (not a diagonal resemblance matrix).
#' @param notification Logical argument (TRUE or FALSE) to specify if
#' notifications for missing observations are to be shown (Default notification =
#' TRUE).
#' @return \item{matrix.w}{Standardized community matrix, where rows are
#' communities and columns species. Row totals (communities) = 1.}
#' \item{matrix.q}{Standardized matrix containing the degree of belonging of
#' species in relation to each other. Row totals (species) = 1.}
#' \item{matrix.P}{Phylogeny-weighted species composition matrix. Row totals
#' (communities) = 1.}
#' @note \strong{IMPORTANT}: Species sequence in the community data
#' matrix MUST be the same as the one in the phylogenetic distance matrix. See
#' \code{\link{organize.syncsa}}.
#' @author Vanderlei Julio Debastiani <vanderleidebastiani@@yahoo.com.br>
#' @seealso \code{\link{syncsa}}, \code{\link{organize.syncsa}}, \code{\link{belonging}},
#' \code{\link{matrix.t}}, \code{\link{matrix.x}}
#' @references Pillar, V.D.; Duarte, L.d.S. (2010). A framework for
#' metacommunity analysis of phylogenetic structure. Ecology Letters, 13,
#' 587-596.
#' @keywords SYNCSA
#' @examples
#' data(ADRS)
#' matrix.p(ADRS$community, ADRS$phylo)
#' @export
matrix.p<-function (comm, phylodist, notification = TRUE)
{
  comm <- as.matrix(comm)
  phylodist <- as.matrix(phylodist)
  matrix.w <- sweep(comm, 1, rowSums(comm, na.rm = TRUE), "/")
  w.NA <- apply(matrix.w, 2, is.na)
  matrix.w[w.NA] <-0
  if(notification){
    if(any(w.NA)){
      warning("Warning: NA in community data", call.=FALSE)
    }
    if(any(is.na(phylodist))){
      warning("Warning: NA in phylodist data", call.=FALSE)
    }
  }
  similar.phy <- 1 - (phylodist/max(phylodist))
  matrix.phy <- 1/colSums(similar.phy)
  matrix.q <- sweep(similar.phy, 1, matrix.phy, "*")
  matrix.P <- matrix.w %*% matrix.q
  # matrix.P <- matmult.syncsa(matrix.w, matrix.q)
  return(list(matrix.w = matrix.w, matrix.q = matrix.q, matrix.P = matrix.P))
}
