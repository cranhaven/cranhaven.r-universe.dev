
#' @title Parse
#'
#' @description Parses a sequence of transitions. If possible returns the Petri Net with the updated marking. Otherwise returns FALSE
#'
#' @param PN A Petri Net
#' @param trace A sequence of transitions, stored in a vector.
#'
#' @export parse_trace



parse_trace <- function(PN, trace) {

	lifecycle::deprecate_warn(
		when = "0.3.0",
		what = "parse_trace()")

	if(length(trace) == 0)
		return(PN)

	for(i in 1:length(trace)) {
		if(trace[i] %in% enabled(PN)$id){
			PN <- execute(PN, trace[i])
		}
		else{
			return(FALSE)
		}
	}
	return(PN)
}
