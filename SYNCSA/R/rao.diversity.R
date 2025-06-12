#' @title Rao's quadratic entropy
#'
#' @description Calculates Rao's quadratic entropy, functional and phylogenetic redundancy.
#'
#' @details Rao's quadratic entropy is a measure of diversity of ecological communities
#' defined by Rao (1982) and is based on the proportion of the abundance of
#' species present in a community and some measure of dissimilarity among them.
#' The dissimilarity range from 0 to 1 and is based on a set of specified
#' functional traits or in the phylogenetic dissimilarity.
#'
#' For the trait data, the function calculates the square root of the
#' one-complement of Gower`s similarity index, in order to have a dissimilarity
#' matrix with Euclidean metric properties. Gower`s index ranges from 0 to 1
#' and can handle traits measured indifferent scales. When the species are
#' completely different in terms of their traits, Rao quadratic entropy is
#' equivalent to the Gini-Simpson index. Traits data can be numeric, factor or
#' ordered factor. For this be considered traits data must be of data.frame
#' class and containing each variable type determined. For additional details and
#' requirements of function please see \code{\link{gowdis}}.
#'
#' Functional redundancy is defined purely as the difference between species
#' diversity and Rao`s quadratic entropy based on their functional
#' dissimilarity (de Bello et al. 2007). The same definition is used for
#' phylogenetic redundancy.
#'
#' Package \strong{SYNCSA} requires that the species and community sequence in
#' the data.frame or matrix must be the same for all dataframe/matrices.
#' The function \code{\link{organize.syncsa}} organizes the data for the functions
#' of the package, placing the matrices of community, traits, phylogenetic distance
#' in the same order. The function use of function organize.syncsa is not requered
#' for run the functions, but is recommended. In this way the arguments comm, traits,
#' phylodist, as well as the argument put.together, can be specified them as normal
#' arguments or by passing them with the object returned by the function
#' \code{\link{organize.syncsa}} using, in this case only the argument comm.
#' Using the object returned by organize.syncsa, the comm argument is used as an alternative
#' way of entering to set all data.frames/matrices, and therefore the other arguments
#' (traits, phylodist and put.together) must be null.
#'
#'
#' @encoding UTF-8
#' @importFrom FD gowdis
#' @param comm Community data, with species as columns and sampling units as
#' rows. This matrix can contain either presence/absence or abundance data.
#' Alternatively comm can be an object of class metacommunity.data, an alternative
#' way to set all data.frames/matrices. When you use the class metacommunity.data the arguments
#' traits, phylodist and put.together must be null. See details.
#' @param traits Data frame or matrix data of species described by traits, with traits as
#' columns and species as rows (Default traits = NULL).
#' @param phylodist Matrix containing phylogenetic distance between species
#' (Default phylodist = NULL).
#' @param checkdata Logical argument (TRUE or FALSE) to check if species
#' sequence in the community data follows the same order as the one in the
#' trait and in the phylodist matrices (Default checkdata = TRUE).
#' @param ord Method to be used for ordinal variables, see \code{\link{gowdis}}
#' (Default ord = "metric").
#' @param put.together List to specify group of traits. Each group specify receive the
#' same weight that one trait outside any group, in the way each group is considered
#' as unique trait (Default put.together = NULL). This argument must be a list, see
#' examples in \code{\link{syncsa}}.
#' @param standardize Logical argument (TRUE or FALSE) to specify if standardize phylogenetic
#' distance to range into range 0 to 1. (Default standardize = TRUE).
#' @param ... Parameters for \code{\link{gowdis}} function.
#' @return \item{Simpson}{Gini-Simpson index within each community (equivalent
#' to Rao quadratic entropy with null, crisp, similarities).}
#' \item{FunRao}{Rao quadratic entropy within each community, considering trait distance.}
#' \item{FunRedundancy}{Functional redundancy in each community.}
#' \item{PhyRao}{Rao quadratic entropy within each community, considering phylogenetic distance.}
#' \item{PhyRedundancy}{Phylogenetic redundancy in each community.}
#' @note \strong{IMPORTANT}: The sequence species show up in community data
#' matrix MUST be the same as they show up in traits and phylodist matrices.
#' See details and \code{\link{organize.syncsa}}.
#' @author Vanderlei Julio Debastiani <vanderleidebastiani@@yahoo.com.br>
#' @seealso \code{\link{organize.syncsa}}, \code{\link{gowdis}},
#' \code{\link{syncsa}}
#' @references de Bello, F.; Leps, J.; Lavorel, S. & Moretti, M. (2007).
#' Importance of species abundance for assessment of trait composition: an
#' example based on pollinator communities. Community Ecology, 8, 163:170.
#'
#' Pillar, V.D.; Blanco, C.C.; Muler, S.C.; Sosinski, E.E.; Joner, F. & Duarte,
#' L.d.S. (2013). Functional redundancy and stability in plant communities.
#' Journal of Vegetation Science, 24, 963:974.
#'
#' Rao, C.R. (1982). Diversity and dissimilarity coefficients: a unified
#' approach. Theoretical Population Biology, 21, 24:43.
#' @keywords SYNCSA
#' @examples
#' data(ADRS)
#' rao.diversity(ADRS$community)
#' rao.diversity(ADRS$community, traits = ADRS$traits)
#' @export
rao.diversity <- function(comm, traits = NULL, phylodist = NULL, checkdata = TRUE, ord = "metric",
                          put.together = NULL, standardize = TRUE, ...)
{
  diver.internal <- function(community, distance){
    if(any(is.na(distance))){
      distance.na <- ifelse(is.na(distance), 0, 1)
      inter.na <- community%*%distance.na
      adjustment <- rowSums(sweep(community, 1, inter.na, "*", check.margin = FALSE))
      distance[is.na(distance)] <- 0
      inter <- community%*%distance
      res <- rowSums(sweep(community, 1, inter, "*", check.margin = FALSE))
      res <- ifelse(adjustment>0, res/adjustment, res)
    } else{
      inter <- community%*%distance
      res <- rowSums(sweep(community, 1, inter, "*", check.margin = FALSE))
    }
    return(res)
  }
  res <- list(call = match.call())
  if (inherits(comm, "metacommunity.data")) {
    if (!is.null(traits) | !is.null(phylodist) | !is.null(put.together)) {
      stop("\n When you use an object of class metacommunity.data the arguments traits, phylodist and put.together must be null. \n")
    }
    traits <- comm$traits
    phylodist <- comm$phylodist
    put.together <- comm$put.together
    comm <- comm$community
  }
  list.warning <- list()
  if(checkdata){
    organize.temp <- organize.syncsa(comm, traits = traits, phylodist = phylodist, check.comm = TRUE)
    if(!is.null(organize.temp$stop)){
      organize.temp$call <- match.call()
      return(organize.temp)
    }
    list.warning <- organize.temp$list.warning
    comm <- organize.temp$community
    traits <- organize.temp$traits
    phylodist <- organize.temp$phylodist
  }
  if(length(list.warning)>0){
    res$list.warning <- list.warning
  }
  if(any(is.na(comm))){
    stop("\n community data with NA\n")
  }
  comm <- as.matrix(comm)
  N <- nrow(comm)
  S <- ncol(comm)
  dist.1 <- 1 - diag(x = rep(1, S))
  if (!is.null(traits)) {
    traits <- as.data.frame(traits)
    m <- ncol(traits)
    weights <- rep(1, m)
    make.names <- is.null(colnames(traits))
    colnames(traits) <- colnames(traits, do.NULL = FALSE, prefix = "T")
    names(weights) <- colnames(traits)
    if(!is.null(put.together)){
      if(!inherits(put.together, "list")){
        stop("\n put.together must be a object of class list\n")
      }
      if(make.names){
        for(k in 1:length(put.together)){
          put.together[[k]] <- paste("T", put.together[[k]], sep = "")
        }
      }
      if(max(table(unlist(put.together)))>1){
        stop("\n The same trait appears more than once in put.together\n")
      }
      if(length(setdiff(unlist(put.together), colnames(traits)))>0){
        stop("\n Check traits names in put.together\n")
      }
      for(k in 1:length(put.together)){
        weights[put.together[[k]]] <- 1/length(put.together[[k]])
      }
    }
    dist.functional <- sqrt(as.matrix(FD::gowdis(x=traits, asym.bin = NULL, ord = ord, w = weights, ...)))
    if (checkdata) {
      if(any(is.na(dist.functional))){
        # stop("\n traits with too much NA \n")
        warning("Warning: NA in distance between species", call. = FALSE)
      }
    }
  }
  if (!is.null(phylodist)) {
    dist.phylogenetic <- as.matrix(phylodist)
    if (checkdata) {
      if(any(is.na(dist.phylogenetic))){
        # stop("\n phylodist with NA \n")
        warning("Warning: NA in phylodist", call. = FALSE)
      }
    }
    if(standardize){
      dist.phylogenetic <- dist.phylogenetic/max(dist.phylogenetic, na.rm = TRUE)
    }
  }
  comm <- sweep(comm, 1, rowSums(comm, na.rm = TRUE), "/")
  SD <- diver.internal(comm, dist.1)
  res$Simpson <- SD
  if (!is.null(traits)){
    FD <- diver.internal(comm, dist.functional)
    res$FunRao <- FD
    res$FunRedundancy <- SD-FD
  }
  if (!is.null(phylodist)){
    PD <- diver.internal(comm, dist.phylogenetic)
    res$PhyRao <- PD
    res$PhyRedundancy <- SD-PD
  }
  return(res)
}
