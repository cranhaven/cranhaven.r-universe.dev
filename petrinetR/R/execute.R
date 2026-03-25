#' @title Execute
#'
#' @description {Executes (fire) an enabled transition and returns the Petri Net with the New marking. If the transition is enabled
#'  via the firing of silent transition (i.e. starting with "inv_" of "tau"), it will fire these first. If the transition is not
#'  enabled, it will return FALSE.}
#'
#' @param PN A Petri Net
#' @param transition The transition to be fired
#'
#' @export execute


execute <- function(PN, transition){

	lifecycle::deprecate_warn(
		when = "0.3.0",
		what = "execute()")

	if(enabled_transition(PN, transition)){
		PN$marking <- union(setdiff(PN$marking, pre_set(PN, transition)), post_set(PN, transition))
		return(PN)
	}
	else if(transition %in% enabled(PN)$id) {
		PN %>% execute(enabled(PN)$by[transition == enabled(PN)$id][1] ) %>% execute(transition) %>% return()
	}
	else
		return(FALSE)
}
