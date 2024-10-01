#' Checks if `phy` is a `phylo` object and/or a chronogram.
#' @param phy A `phylo` object.
#' @param brlen Boolean. If `TRUE` it checks if `phylo` object has branch lengths.
#' @param dated Boolean. If `TRUE` it checks if `phylo` object is ultrametric.
#' @return Nothing
#' @export
is_phylo <- function(phy = NULL, brlen = FALSE, dated = FALSE) {
  if (!inherits(phy, "phylo")) {
    stop("tree is not a phylo object")
  }
  if (brlen) {
    if (!phylo_has_brlen(phy = phy)) {
      stop("tree must have branch lengths")
    }
  }
  if (dated) {
    if (!ape::is.ultrametric(phy, option = 2)) {
      warning("branch lengths in tree should be proportional to time")
      stop("tree must be ultrametric") # really?  # Think how to incorporate trees with extinct taxa
    }
  }
}


#' Convert spaces to underscores in trees.
#'
#' @inheritParams is_phylo
#' @return A `phylo` object.
phylo_tiplabel_space_to_underscore <- function(phy) {
  phy$tip.label <- gsub(" ", "_", phy$tip.label)
  return(phy)
}

#' Convert underscores to spaces in trees.
#'
#' @inheritParams is_phylo
#' @return A `phylo` object.
phylo_tiplabel_underscore_to_space <- function(phy) {
  # a better name for this function would be underscore_to_blank
  # add method .phylo
  # change tip and node labels
  phy$tip.label <- gsub("_", " ", phy$tip.label)
  # make sure there is only one consecutive blank at a time
  return(phy)
}
