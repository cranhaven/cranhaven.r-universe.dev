


# Model Constructs -------------------------------------------------------------

#' Check of models can be combined into a suit, returns a compatability table
#' @noRd
validate_model_compatability <- function(x) {

	# Global variables

	# Should be a list of model_archetypes
	model_list <- list()

	for (i in seq_along(x)) {

		# Validate
		validate_class(x[[i]], "model_archetype")

		# Terms
		t <-
			attr(x[[i]], "terms") |>
			vec_data() |>
			dplyr::select(c(
				"term",
				"side",
				"role"
			))

		# Model information
		m <-
			vec_data(x[[i]]) |>
			dplyr::select(c(
				"tag",
				"type",
				"subtype"
			))

		# Combine into list of data.frames
		# Suppress warning about dropping row names
		model_list[[i]] <-
			cbind(m, t) |>
			suppressWarnings()
	}

	# Make a unique table
	tbl <-
		do.call(rbind, model_list) |>
		unique()

	# Validate model types
	if ( (length(unique(tbl$type)) != 1) | (length(unique(tbl$subtype)) != 1)) {
		stop(
			"The models need to have the same type [",
			paste(unique(tbl$type), collapse = ", "),
			"] and subtype [",
			paste(unique(tbl$subtype), collapse = ", "),
			"]",
			call. = FALSE
		)
	}

	# Validate outcome and exposure
	out <- unique(tbl$term[tbl$role == "outcome"])
	prd <- unique(tbl$term[tbl$role == "predictor"])
	exp <- unique(tbl$term[tbl$role == "exposure"])
	cov <- unique(tbl$term[tbl$role == "covariate"])
	med <- unique(tbl$term[tbl$role == "mediator"])
	unk <- unique(tbl$term[tbl$role == "unknown"])

	if (length(out) > 1 & length(exp) > 1) {
		stop(
			"If there are multiple outcomes [",
			paste(out, collapse = ", "),
			"] and multiple exposures [",
			paste(exp, collapse = ", "),
			"], the models are not related enough to group together",
			call. = FALSE
		)
	}

	# Else return invisibly true
	invisible(TRUE)

}


#' @keywords internal
#' @noRd
validate_class <- function(x, what) {
	if (!inherits(x, what)) {
		stop(
			deparse(substitute(x)),
			" needs to inherit from `",
			paste(what),
			#paste("c(", paste(what, collapse = ", "), ")", sep = ""),
			"`, but is of class `",
			paste(class(x), collapse = ', '),
			"`.",
			call. = FALSE
		)
	}
	invisible(TRUE)
}

#' Validate arguments for term creation
#' @keywords internal
#' @noRd
validate_classes <- function(x, what) {

	varnames <- names(x)

	lapply(
		varnames,
		FUN = function(.x) {
			if (!inherits(x[[.x]], what)) {
				stop(
					"`",
					.x,
					"` needs to inherit from `",
					paste("c(", paste(what, collapse = ", "),
								")",
								sep = ""
					),
					"`.",
					call. = FALSE
				)
			}
		}
	)

	invisible(TRUE)

}


#' Check objects for their class type If its incorrect based on the validator,
#' should message about the problem object. Returns TRUE invisibly if all
#' objects are appropriate.
#' @keywords internal
#' @noRd
check_classes <- function(x, fn) {

	stopifnot("Must check classes via logical determinant function `is_***()`"
						= inherits(fn, "function"))

	functionName <- as.character(match.call()[3])

	y <-
		sapply(x, function(.x) {
			.y <- fn(.x)
			if (!.y) {
				message("Element `", deparse1(.x),
								"` returns FALSE for `", functionName, "()`")
			}
			.y
		}, USE.NAMES = FALSE)


	invisible(all(y))

}
