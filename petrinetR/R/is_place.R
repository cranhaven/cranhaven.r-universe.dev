
#' @title Is place
#'
#' @description Check if a place is part of a petri net.
#'
#' @param place \code{\link{character}} of length one: the place id to check.
#' @inheritParams is_node
#' @return logical that indicates whether \code{place} is a place in \code{PN}
#' @export is_place


is_place <- function(place, PN) {
	if(place %in% places(PN)$id | place %in% places(PN)$label)
		return(T)
	else
		return(F)
}
