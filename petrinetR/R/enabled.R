
#' @title Enabled transitions
#'
#' @description List the enabled transitions in a marked Petri Net. Silent transitions, i.e. starting with "inv_" or "tau" are assumed to be able to fire silently, thereby possible enabling other transitions.
#'
#' @param PN A Petri Net
#'
#' @export enabled


enabled <- function(PN) {

	lifecycle::deprecate_warn(
		when = "0.3.0",
		what = "enabled()")

	PN %>%
		transitions %>%
		mutate(enabled = (id %>% sapply(enabled_transition, PN = PN))) %>%
		filter(enabled == TRUE) -> output

	output %>%
		filter(grepl("inv_", id) | grepl("tau", id)) -> enabled_inv

	if(nrow(enabled_inv) > 0) {
		for(i in 1:nrow(enabled_inv)){
			execute(PN, enabled_inv$id[i]) %>%
				enabled %>%
				mutate(by = enabled_inv$id[i]) %>%
				bind_rows(output, .) -> output
		}
	}

	output %>% unique %>% return()
}
