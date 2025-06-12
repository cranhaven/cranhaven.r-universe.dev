#' @title Degree of belonging of species
#'
#' @description Function to obtain a matrix containing the degrees of belongings of each
#' and every species. The degree of belonging of each species is defined based on
#' its ecological or phylogenetic resemblance to every other
#' species in the community. For more details, see \code{\link{matrix.p}},
#' \code{\link{matrix.x}} and \code{\link{syncsa}}.
#'
#' @encoding UTF-8
#' @param dis Matrix containing distance between species.
#' @param standardize Logical argument (TRUE or FALSE) to specify if dis must
#' be standardized in values ranging from 0 to 1 (Default standardize = TRUE).
#' @return Standardized matrix containing the degree of belonging of species in
#' relation to each other. Row totals (species) = 1.
#' @author Vanderlei Julio Debastiani <vanderleidebastiani@@yahoo.com.br>
#' @seealso \code{\link{matrix.p}}, \code{\link{matrix.x}},
#' \code{\link{syncsa}}
#' @references Pillar, V.D.; Duarte, L.d.S. (2010). A framework for
#' metacommunity analysis of phylogenetic structure. Ecology Letters, 13,
#' 587-596.
#' @keywords SYNCSA
#' @examples
#' data(ADRS)
#' belonging(ADRS$phylo)
#' @export
belonging <- function (dis, standardize = TRUE)
{
  distance <- as.matrix(dis)
  if(standardize){
    distance <- distance/max(distance)
  }
  similarity <- 1-distance
  stats <- 1/colSums(similarity)
  res <- sweep(similarity, 1, stats, "*")
  return(res)
}
