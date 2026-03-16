#' Combine species in vegetation matrix
#'
#' @name comb.species
#' @export
#' @param x (veg) vegetation matrix of class veg
#' @param sel (character) vector of species to combine
#' @param newname (character) name of the new taxon
#' @param refl (character) Name of taxonomic reference list
#'

comb.species <- function (x, sel, newname, refl) {
  if (!"veg" %in% class(x)) warning(paste("Object", x, "not of class \"veg\""))
  if (missing(sel)) stop(paste("Either vegetation matrix or the vector of species names to combine is missing."))
  if (missing(refl)) refl <- attr(x, "taxreflist")
  if (is.null(refl)) stop("Please specify appropriate taxonomic reference list.")
  if (missing(newname))  newname <- sel[1]
  occtaxa <- sel[sel %in% colnames(x)]
  if (length(occtaxa) == 0) stop("Selected species do not occur in names of vegetation matrix.")
  if (length(occtaxa) > 0) {
    cat("The following names are combined to the new name:", newname, "\n")
    print(occtaxa)
    n <- x[, names(x) %in% occtaxa]
    y <- x[, !names(x) %in% occtaxa]
    result <- cbind(y, rowSums(n))
    names(result)[ncol(result)] <- newname
    class(result) <- c("veg", "data.frame")
    attr(result, "taxreflist") <- attr(x, "taxreflist")
  }
  else {
    cat("Nothing to combine for", sel, "\n")
    result <- x
  }
  return(result)
}
