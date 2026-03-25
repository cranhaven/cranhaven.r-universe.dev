#' @title Create marked Petri Net
#'
#' @description Function to create a  \code{\link{marked_petrinet}}, consisting of a \code{\link{petrinet}}, an initial marking, and a final marking.
#'
#' @param PN \code{\link{petrinet}}: Object of class \code{\link{petrinet}}.
#' @param initial_marking \code{\link{character}}: A vector with place ids representing the initial marking.
#' @param final_marking \code{\link{character}}: A vector with place ids representing the final marking.
#'
#' @return A \code{\link{marked_petrinet}}
#' @export create_marked_PN

create_marked_PN <- function(PN, initial_marking, final_marking) {
	petrinet <- PN
	initial_marking <- as.character(initial_marking)
	final_marking <- as.character(final_marking)

	marked_PN <- list(petrinet = petrinet, initial_marking = initial_marking, final_marking = final_marking)

	class(marked_PN) <- c("marked_petrinet", "petrinet", "list")

	return(marked_PN)
}
