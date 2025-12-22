### Formula Helpers -----------------------------------------------------------

#' Tools for working with formula-like objects
#' 
#' @return A `character` describing part of a `formula` or `fmls` object
#'
#' @param x A formula-like object
#'
#' @param ... Arguments to be passed to or from other methods
#'
#' @name formula_helpers
NULL

#' @rdname formula_helpers
#' @export
lhs <- function(x, ...) {
	UseMethod("lhs", object = x)
}

#' @rdname formula_helpers
#' @export
rhs <- function(x, ...) {
	UseMethod("rhs", object = x)
}

#' @rdname formula_helpers
#' @export
rhs.formula <- function(x, ...) {

	# Handles name, call, and character options
	# Does strip away parentheses
	if (inherits(x[[length(x)]], 'character')) {
		y <-
			x[[length(x)]] |>
			trimws()
	} else {
		y <-
			x[[length(x)]] |>
			deparse1() |>
			strsplit("\\+|-") |>
			unlist() |>
			trimws() |>
			{
				\(.x) gsub('"', "", .x)
			}()
	}

	# Handle special interaction terms in original formula
	# Will expand from `a * b` -> `a + b + a:b`
	pos <- grep("\\*", y)
	npos <- grep("\\*", y, invert = TRUE)

	ints <- character()
	if (length(pos) > 0) {
	  ints <-
	    y[pos] |>
	    strsplit("\\*") |>
	    unlist() |>
	    trimws() |>
	    {
	      \(.x) c(.x[1], .x[2], paste0(.x[1], ":", .x[2]))
	    }()
	}

	# Return
	c(ints, y[npos])

}

#' @rdname formula_helpers
#' @export
lhs.formula <- function(x, ...) {
	if (length(x) == 2) {
		y <- character()
	} else if (length(x) == 3) {
		y <-
			x[[2]] |>
			deparse1() |>
			strsplit("\\+|-") |>
			unlist() |>
			trimws() |>
			{
				\(.x) gsub('"', "", .x)
			}()
	}

	y
}


#' Convert labeling formulas to named lists
#'
#' @description
#' Take list of formulas, or a similar construct, and returns a named list. The
#' convention here is similar to reading from left to right, where the name or
#' position is the term is the on the *LHS* and the output label or target
#' instruction is on the *RHS*.
#'
#' If no label is desired, then the *LHS* can be left empty, such as `~ x`.
#'
#' @return A named list with the index as a `character` representing the term
#'   or variable of interest, and the value at that position as a `character`
#'   representing the label value.
#'
#' @param x An argument that may represent a formula to label variables, or can
#'   be converted to one. This includes, `list`, `formula`, or
#'   `character` objects. Other types will error.
#'
#' @export
labeled_formulas_to_named_list <- function(x) {

	# Check to see if its a single formula or a list of formulas
	stopifnot("Should be applied to individual or list of formulas" =
							inherits(x, c("list", "formula", "character")))

	# Empty, list, or formula management
	if (length(x) == 0) { # If an empty formula or list, return an empty list
		y <- list()
	} else if (inherits(x, "formula")) { # If a single formula
		nm <- lhs(x)
		val <- rhs(x)
		# If unnamed, then give it the same value as the name
		if (length(nm) == 0) {
			nm <- val
		}
		names(val) <- nm
		y <- as.list(val)
	} else if (inherits(x, "list")) { # If a list that contains formulas
		# Confirm each item is formula
		stopifnot("If a list is provided, each element must be a `formula`"
							= all(sapply(x, inherits, "formula")))

		y <- sapply(x, function(.x) {
			nm <- lhs(.x)
			val <- rhs(.x)
			# If unnamed, then give it the same value as the name
			if (length(nm) == 0) {
				nm <- val
			}
			if (grepl("^[[:digit:]]$", val)) {
				val <- as.integer(val)
			}
			names(val) <- nm
			.y <- as.list(val)
		})
	} else if (inherits(x, "character")) {
		nm <- x
		val <- x
		names(val) <- nm
		y <- as.list(val)
	}

	# Return
	y
}

