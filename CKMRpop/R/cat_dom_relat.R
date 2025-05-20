
#' Categorize the dominant relationship from an ancestry match matrix
#'
#' This function takes an ancestry match matrix (AMM) and it returns what the
#' "dominant" relationship is.  It doesn't try to capture all the nuances
#' that might be present in the AMM.  For example, it does not note inbreeding
#' and other such things.  However this is useful for categorizing what type
#' of relationship is the "closest" relationship type amongst all the ways a
#' pair is related.
#' Currently for num_generations == 2, the function goes through in this order,
#' to identify relationships:
#' - `Se`: self.  This is as far as it goes with num_generations = 0
#' - `PO`: parent-offspring
#' - `Si`: sibling. This is as far as it goes with num_generations = 1
#' - `GP`: grandparental
#' - `A` : avuncular (aunt-niece)
#' - `FC`: first cousin.
#'
#' And so forth.  This has been implemented out to 3 generations
#' using the relationship zones in package data object
#' `relationship_zone_names`.
#' @param AMM The ancestry match matrix to categorize
#' @return This returns a list with two components:
#' - `type`: a string saying what type of relationship (i.e., "Si", or "A").
#' - `hits`: a two vector of the number of TRUEs in each "relationship zone" on the
#' upper or the lower diagonal of the AMM.
#' @export
#' @keywords internal
#' @examples
#' cat_dom_relat(half_first_cousin_amm)
cat_dom_relat <- function(AMM) {

  # Determine the number of generations from the dimensions of the matrix and
  # throw an error if it's not an integer
  L <- nrow(AMM)
  Ng <- log(L + 1, base = 2) - 1
  stopifnot(Ng %% 1 == 0)

  # cycle over the relationships in order until we get one that is non-zero
  Relats <- relationship_zone_names

  for(r in Relats) {
    masks <- anc_match_masks(num_generations = Ng, R = r)
    sums <- unlist(lapply(masks, function(x) sum(x & AMM)))
    if(any(sums > 0)) { return(list(type = r, hits = sums))}
  }

  # if we got to here, then there was no relationship, and we just return "U", c(0, 0).
  # Note that we typically should never get here if we are only dealing with pairs that
  # are related to some degree at n generations...
  return(list(type = "U", hits = c(0, 0)))
}
