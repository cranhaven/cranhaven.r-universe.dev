#' @keywords internal
#' @noRd
has_cli <- function() {
  isTRUE(requireNamespace("cli", quietly = TRUE))
}


# Term conversion messages -----------------------------------------------------

#' @keywords internal
#' @noRd
message_interaction <- function(interactionTerm, exposureTerm) {
	msg <-
		paste0(
			"Interaction term `",
			interactionTerm,
			"` was applied to exposure term `",
			exposureTerm,
			"`"
		)

	message(msg)
}

#' @keywords internal
#' @noRd
warning_interaction_roles <- function(roleList) {

	interactionTerms <- names(roleList[roleList %in% .roles$interaction])

	msg <-
		paste(
			"The interaction term(s) `",
			paste0(interactionTerms, collapse = " + "),
			"` was/were specified but an exposure variable was not found.",
			"The result will treat the term(s) as regular predictor variables."
		)

	warning(msg)
}

#' @keywords internal
#' @noRd
warning_mediation_roles <- function(roleList) {

	mediatorTerms <- names(roleList[roleList %in% .roles$mediator])

	msg <-
		paste(
			"The mediator term(s) `",
			paste0(mediatorTerms, collapse = " + "),
			"` was/were specified but an exposure variable was not found.",
			"The result will treat these term(s) as regular predictor variables."
		)

	warning(msg)
}

# Formula simplification messages ----------------------------------------------

#' @keywords internal
#' @noRd
message_formula_to_fmls <- function() {
	message("Converting the supplied `formula` to a `fmls`")
}

#' @keywords internal
#' @noRd
message_fundamental_pattern <- function(mediationTerm,
																				strataTerm) {

	if (length(mediationTerm) == 0) mediationTerm <- NA
	if (length(strataTerm) == 0) strataTerm <- NA

	msg <-
		paste0(
			"Using `", .patterns[1], "` decomposition pattern: \n",
			"- Mediation term: ", mediationTerm, "\n",
			"- Stratifying term: ", strataTerm
		)

	message(msg)
}

# Model and model table messages -----------------------------------------------

#' @keywords internal
#' @noRd
message_empty_models <- function() {
	message("The model was either empty or not able to be parsed.")
}
