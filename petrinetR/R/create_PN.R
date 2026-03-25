#' @title Create Petri Net
#'
#' @description Function to create a \code{\link{petrinet}} by specifying places, transitions and flows.
#'
#' @param places \code{data.frame} or \code{tibble} of places, with columns id and label. Both columns should be characters.
#' @param transitions \code{data.frame} or \code{tibble} of transitions, with columns id and label. Both columns should be characters.
#' @param flows \code{data.frame} or \code{tibble} of flows, with columns named "from" and "to", referring to ids of places and transitions. Both columns should be characters.
#'
#'
#' @return A \code{\link{petrinet}}
#' @examples
#' library(dplyr)
#' create_PN(tibble(id = "p1", label = "place_1"),
#' 			tibble(id = "t1", label = "transition_1"),
#' 			tibble(from = "t1",to = "p1"))
#'
#' @export create_PN

create_PN <- function(places,transitions,flows) {
	PN <- list()
	PN$places <- places %>% mutate(across(c("id", "label"), as.character))
	PN$transitions <- transitions %>% mutate(across(c("id", "label"), as.character))
	PN$flows <- flows %>% mutate(across(c("from","to"), as.character))

	class(PN) <- c("petrinet", "list")

	return(PN)

}
