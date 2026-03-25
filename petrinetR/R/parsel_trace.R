#' @title Parse (logical)
#'
#' @description Tests whether a sequence of transitions can be fired by a Petri Net. If so returns TRUE, otherwise FALSE.
#'
#' @param PN A Petri Net
#' @param trace A sequence of transitions, stored in a vector.
#'
#' @export parsel_trace

parsel_trace <- function(PN, trace) {

	lifecycle::deprecate_warn(
		when = "0.3.0",
		what = "enabled()")

	for(i in 1:length(trace)) {
		if(trace[i] %in% enabled(PN)$id){
			PN <- execute(PN, trace[i])
		}
		else{
			return(FALSE)
		}
	}
	return(TRUE)
}
