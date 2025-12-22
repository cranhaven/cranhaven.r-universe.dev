# Class definition -------------------------------------------------------------

#' Vectorized formulas
#'
#' @description
#'
#' This function defines a modified `formula` class that has been
#' vectorized. The `fmls` serves as a set of instructions or a _script_ for the
#' formula and its tm. It expands upon the functionality of formulas,
#' allowing for additional descriptions and relationships to exist between the
#' tm.
#'
#' @details
#'
#' This is not meant to supersede a [stats::formula()] object, but provide a
#' series of relationships that can be helpful in causal modeling. All `fmls`
#' can be converted to a traditional `formula` with ease. The base for this
#' object is built on the [tm()] object.
#'
#' # Patterns
#'
#' The expansion pattern allows for instructions on how the covariates should be
#' included in different formulas. Below, assuming that _x1_, _x2_, and _x3_ are
#' covariates...
#'
#' \deqn{y = x1 + x2 + x3}
#'
#' __Direct__:
#'
#' \deqn{y = x1 + x2 + x3}
#'
#' __Seqential__:
#'
#' \deqn{y = x1}
#' \deqn{y = x1 + x2}
#' \deqn{y = x1 + x2 + x3}
#'
#' __Parallel__:
#'
#' \deqn{y = x1}
#' \deqn{y = x2}
#' \deqn{y = x3}
#'
#' @inheritSection tm Roles
#' @inheritSection tm Pluralized Labeling Arguments
#'
#' @param x Objects of the following types can be used as inputs
#'
#'   - `tm`
#'
#'   - `formula`
#'
#' @param pattern A `character` from the following choices for pattern
#'   expansion. This is how the formula will be expanded, and decides how the
#'   covariates will incorporated. See the details for further explanation.
#'
#'   - direct: the covariates will all be included in each formula
#'
#'   - sequential: the covariates will be added sequentially, one by one, or by groups, as indicated
#'
#'   - parallel: the covariates or groups of covariates will be placed in parallel
#'
#'   - fundamental: every formula will be decomposed to a single outcome and predictor in an atomic fashion
#'
#' @param ... Arguments to be passed to or from other methods
#'
#' @return An object of class `fmls`
#' @name fmls
#' @importFrom rlang !!! :=
#' @export
fmls <- function(x = unspecified(),
								 pattern = c("direct",
								 						"sequential",
								 						"parallel",
								 						"fundamental"),
								 ...) {

	# Return early if nothing is given
	if (length(x) == 0) {
		return(new_fmls())
	}

	# Convert to term object if possible
	# Notably, if an interaction term is present, will be a separate variable
	validate_class(x, c("tm", "formula"))
	if (inherits(x, "formula")) {
		x <- tm(x)
	}

	# Check pattern
	if (length(pattern) > 1) {
		pattern <- pattern[1] # Direct
	}
	if (!pattern %in% .patterns) {
		stop("The pattern ",
				 deparse(pattern),
				 " is not yet supported.",
				 call. = FALSE)
	}

	## Patterns
	# If pattern is acceptable can send for pattern tracing
	# Shuttled through the parent function `trace_pattern()`
	tbl <- apply_pattern(x, pattern)

	## Mediation
	# If mediation is needed
	# Mediation should be done only if covariates are already added
	# Function `check_mediation()` is internal only
	med <- with(vec_proxy(x), term[role == "mediator"])
	if (length(med) > 0 & pattern != "fundamental") {
		tbl <- check_mediation(x, tbl)
	}

	## Groups
	# Will remove rows if they will cause an error
	tbl <- check_groups(x, tbl)

	# Create a list where each item is a vector of terms
	# These can be then turned into a term matrix
	fmMat <-
		lapply(as.list(as.data.frame(t(tbl))), function(.x) {
			table(.x) |>
				rbind() |>
				as.data.frame()
		}) |>
	  dplyr::bind_rows() |>
	  {
	    \(.x) replace(.x, is.na(.x), 0)
	  }()

	new_fmls(formulaMatrix = fmMat,
					 termTable = vec_proxy(x))

}

#' Initialize new formula-based data frame
#' @keywords internal
#' @noRd
new_fmls <- function(formulaMatrix = data.frame(),
										 termTable = data.frame()) {

	stopifnot(is.data.frame(formulaMatrix))
	stopifnot(is.data.frame(termTable))

	new_data_frame(
		x = formulaMatrix,
		termTable = termTable,
		class = "fmls"
	)

}

#' @rdname fmls
#' @export
is_fmls <- function(x) {
	inherits(x, "fmls")
}

#' @rdname fmls
#' @export
key_terms <- function(x) {

	# Global variables
	term <- NULL

	# If a formula object, must pull only terms that are available
	if (is_fmls(x)) {
		# Formula matrix
		fm <- vec_data(x)
		fm[is.na(fm)] <- 0
		tms <- names(fm)[colSums(fm) >= 1]

		# Term table.. add on extra "meta" terms PRN
		tmTab <- attr(x, 'termTable')
		tms <- c(tms, tmTab$term[tmTab$side == 'meta'])


		subset(tmTab, term %in% tms) |>
			vec_restore(to = tm())
	} else {
		NULL
	}
}

#' @export
format.fmls <- function(x, color = TRUE, ...) {

	# Break into matrix and key
	fmMat <- vec_data(x)
	tmTab <- attr(x, "termTable")

	fmt <-
		apply(
			fmMat,
			MARGIN = 1,
			FUN = function(.x) {
				.y <- tmTab[tmTab$term %in% names(.x[which(.x == 1)]),]

				if ("mediator" %in% .y$role & !("outcome" %in% .y$role)) {
					# Handle mediation formula
					.l <-
						vec_restore(.y[.y$role == "mediator", ], to = tm()) |>
						format() |>
						paste0(collapse = " + ")

					.r <-
						vec_restore(.y[.y$side == "right" &
													 	.y$role != "mediator", ], to = tm()) |>
						format() |>
						paste0(collapse = " + ")

					.f <- paste(.l, sep = " ~ ", .r)
				} else {
					.l <-
						vec_restore(.y[.y$side == "left", ], to = tm()) |>
						format() |>
						paste0(collapse = " + ")

					.r <-
						vec_restore(.y[.y$side == "right", ], to = tm()) |>
						format() |>
						paste0(collapse = " + ")

					.f <- paste(.l, sep = " ~ ", .r)

				.f
			}
		})

	# Return
	fmt
}

#' @export
print.fmls <- function(x, ...) {

	# Colorful printing
	if (length(x) > 1) {
		cat(format(x), sep = "\n")
	} else if (length(x) == 1) {
		cat(format(x))
	}
}

#' @export
vec_ptype_full.fmls <- function(x, ...) {
	"formulas"
}

#' @export
vec_ptype_abbr.fmls <- function(x, ...) {
	"fmls"
}

#' @export
methods::setOldClass(c("fmls", "vctrs_rcrd"))

# Coercion methods -----------------------------------------------------------

# SELF

#' @keywords internal
#' @noRd
fmls_ptype2 <- function(x, y, ..., x_arg = "", y_arg = "") {

	# Creates a "empty" data frame with appropraite fields
	newMatrix <- df_ptype2(x, y, ..., x_arg = x_arg, y_arg = y_arg)

	# Handle scalar term attribute
	xTm <- attr(x, "termTable")
	yTm <- attr(y, "termTable")

	tmTab <-
		rbind(xTm, yTm) |>
		unique()

	dups <- duplicated(tmTab$term)

	# New terms that will be the key scalar attribute
	newTmTab <- tmTab[!dups, ]

	new_fmls(newMatrix, termTable = newTmTab)
}

#' @keywords internal
#' @noRd
fmls_cast <- function(x, to, ..., x_arg = "", to_arg = "") {

	# New terms that will be the key scalar attribute

	# Handle terms first
	toTm <- attr(to, "termTable")
	xTm <- attr(x, "termTable")

	tmTab <-
		rbind(toTm, xTm) |>
		unique()

	dups <- duplicated(tmTab$term)

	newTmTab <- tmTab[!dups, ]

	# When casting, the matrices need to be similar in columns
	newMatrix <-
		df_cast(
			x,
			to,
			...,
			x_arg = x_arg,
			to_arg = to_arg
		)

	new_fmls(newMatrix, termTable = newTmTab)
}

#' @export
vec_ptype2.fmls.fmls <- function(x, y, ...) {
	fmls_ptype2(x, y, ...)
}

#' @export
vec_cast.fmls.fmls <- function(x, to, ...) {
	fmls_cast(x, to, ...)
}

# CHARACTER

#' @export
vec_ptype2.fmls.character <- function(x, y, ...) y # X = fmls

#' @export
vec_ptype2.character.fmls <- function(x, y, ...) x # X = character

#' @export
vec_cast.fmls.character <- function(x, to, ...) {
	# order is flipped, such that `x` is character
	# Cast from character into fmls
	x |>
		stats::as.formula(env = .GlobalEnv) |>
		fmls()
}

#' @export
vec_cast.character.fmls <- function(x, to, ...) {
	# Going from fmls to character
	# order is flipped, such that `x` is fmls
	as.character(x)
}

#' @export
as.character.fmls <- function(x, ...) {
	formulas_to_terms(x) |>
		lapply(stats::formula) |>
		sapply(deparse1)

}

# FORMULA

#' @export
formula.fmls <- function(x, ...) {
	x |>
		formulas_to_terms() |>
		lapply(stats::as.formula, env = .GlobalEnv)
}


#' @export
vec_ptype2.fmls.formula <- function(x, y, ...) {
	x
}

#' @export
vec_ptype2.formula.fmls <- function(x, y, ...) {
	y
}

#' @export
vec_cast.formula.fmls <- function(x, to, ...) {
	# Cast from `fmls` into `formula`
	# Returns a list of formulas
	stats::formula(x)

}

#' @export
vec_cast.fmls.formula <- function(x, to, ...) {
	# Cast from `formula` into `fmls`
	fmls(x)
}

# Formula internals ------------------------------------------------------------

#' @keywords internal
#' @noRd
check_mediation <- function(x, tbl) {

	# Roles
	tmTab <- vec_proxy(x)
	out <- tmTab$term[tmTab$role == "outcome"]
	exp <- tmTab$term[tmTab$role == "exposure"]
	prd <- tmTab$term[tmTab$role == "predictor"]
	con <- tmTab$term[tmTab$role == "confounder"]
	med <- tmTab$term[tmTab$role == "mediator"]
	int <- tmTab$term[tmTab$role == "interaction"]
	sta <- tmTab$term[tmTab$role == "strata"]

	# Requires a table from the `apply_*_pattern()` functions
	# Each row has been expanded for exposure and outcome
	# This will triple the number of rows subsequently
	validate_class(tbl, "tbl_df")

	# Mediation...
	# 	The combinations of mediation are based on causal reasoning
	# 	outcome ~ exposure + mediator + predictors
	#		mediator ~ exposure
	# 	outcome ~ mediator + exposure

	# 'outcome ~ exposure + mediator + predictors'
	# 	Covariates exists in each row already
	# 	Simply add mediator
	m1 <- tidyr::expand_grid(tbl, mediator = med)

	# 'mediator ~ exposure'
	# 	No other variables allowed
	# 	Add a new row of just this
	m2 <- tidyr::expand_grid(mediator = med, exposure = exp)

	# 'outcome ~ mediator + exposure'
	#		Only looking for effect of mediator on outcome WITH exposure
	m3 <- tidyr::expand_grid(outcome = out, mediator = med, exposure = exp)

	# Bind all the tables together
	tbl <-
		m1 |>
		dplyr::bind_rows(m2) |>
		dplyr::bind_rows(m3) |>
		unique()

	# Return
	tbl

}

#' @keywords internal
#' @noRd
check_groups <- function(x, tbl) {

	# Global variables
	group <- NULL

	# Roles
	tmTab <- vec_proxy(x)
	out <- tmTab$term[tmTab$role == "outcome"]
	exp <- tmTab$term[tmTab$role == "exposure"]
	prd <- tmTab$term[tmTab$role == "predictor"]
	con <- tmTab$term[tmTab$role == "confounder"]
	med <- tmTab$term[tmTab$role == "mediator"]
	int <- tmTab$term[tmTab$role == "interaction"]
	sta <- tmTab$term[tmTab$role == "strata"]

	# Requires a table from the `apply_*_pattern()` functions
	validate_class(tbl, "tbl_df")

	# Grouping variables now must be assessed
	# IF a group is present, the row must have its full group present OR ELSE
	# The term table from above serves as the reference
	# Grouping doesn't need to be checked IF no grouping variables in that row

	groupLevels <- with(tmTab, unique(group[!is.na(group)]))
	rowNums <- seq(nrow(tbl))
	badRows <- integer()

	for (g in groupLevels) {
		groupedPredictors <- subset(tmTab, group == g)$term
		for (r in rowNums) {
			# All predictors
			allPredictors <-
				tbl[r,] |>
				dplyr::select(-dplyr::any_of(c("outcome", "exposure"))) |>
				unlist() |>
				unname() |>
				stats::na.omit()

			# IF any grouping variables are present, then ALL must be present
			if (any(groupedPredictors %in% allPredictors)) {
				# THEN check if all are present
				if (!all(groupedPredictors %in% allPredictors)) {
					badRows <- c(badRows, r)
				}
			}

		}
	}

	# Now remove the "bad rows"
	ntbl <- tbl[badRows,]
	tbl <- suppressMessages(dplyr::anti_join(tbl, ntbl))
	stopifnot(
		"Based on restrictions from the chosen terms and pattern, no `fmls` can be generated."
		= nrow(tbl) > 0
	)

	# Return
	tbl

}

#' @keywords internal
#' @noRd
formulas_to_terms <- function(x) {

	validate_class(x, "fmls")

	fmMat <- vec_data(x)
	tmTab <- attr(x, "termTable")

	tms <-
		apply(
			fmMat,
			MARGIN = 1,
			FUN = function(.x) {
				.y <-
					tmTab[tmTab$term %in% names(.x[which(.x == 1)]) |
									tmTab$role == "strata", ]
				vec_restore(.y, to = tm())
		})

	# Return
	tms

}

