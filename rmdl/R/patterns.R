# Basic patterns ---------------------------------------------------------------

#' Apply patterns to formulas
#'
#' The family of `apply_*_pattern()` functions that are used to expand `fmls`
#' by specified patterns. These functions are not intended to be used directly
#' but as internal functions. They have been exposed to allow for potential
#' user-defined use cases.
#'
#' Currently supported patterns are: `r paste(.patterns)`.
#'
#' @return Returns a `tbl_df` object that has special column names and rows.
#'   Each row is essentially a precursor to a new formula.
#'
#'   These columns and rows must be present to be used with the `fmls()`
#'   function, and generally are the expected result of the specified pattern.
#'   They will undergo further internal modification prior to being turned into
#'   a `fmls` object, but this is an developer consideration. If developing a
#'   pattern, please use this guide to ensure that the output is compatible with
#'   the `fmls()` function.
#'
#'   - outcome: a single term that is the expected outcome variable
#'
#'   - exposure: a single term that is the expected exposure variable, which may not be present in every row
#'
#'   - covariate_*: the covariates expand based on the number that are present (e.g. "covariate_1", "covariate_2", etc)
#'
#' @param x A `tm` object
#' @param pattern A character string that specifies the pattern to use
#' @name patterns
#' @export
apply_pattern <- function(x, pattern) {

	# Only accept objects as `tm` objects
	validate_class(x, "tm")

	# Send to appropriate function
	if (pattern == "direct") {
		tbl <- apply_direct_pattern(x)
	} else if (pattern == "sequential") {
		tbl <- apply_sequential_pattern(x)
	} else if (pattern == "parallel") {
		tbl <- apply_parallel_pattern(x)
	} else if (pattern == "fundamental") {
		tbl <- apply_fundamental_pattern(x)
	} else if (pattern == "rolling_interaction") {
		tbl <- apply_rolling_interaction_pattern(x)
	} else {
		stop(
			"Pattern '",
			pattern,
			"' is not yet supported. If this was not a type, consider filing an issue or pull request."
		)
	}
}

#' @rdname patterns
#' @export
apply_fundamental_pattern <- function(x) {

	# Term table
	tmTab <- vec_proxy(x)

	# Handle roles
	out <- tmTab$term[tmTab$role == "outcome"]
	exp <- tmTab$term[tmTab$role == "exposure"]
	prd <- tmTab$term[tmTab$role == "predictor"]
	con <- tmTab$term[tmTab$role == "confounder"]
	med <- tmTab$term[tmTab$role == "mediator"]
	int <- tmTab$term[tmTab$role == "interaction"]
	sta <- tmTab$term[tmTab$role == "strata"]

	# Outcomes and exposures should be set as a "key pair"
	if (length(out) > 0 & length(exp) > 0) {
		tbl <- tidyr::expand_grid(outcome = out, exposure = exp)
	} else if (length(out) > 0 & length(exp) == 0) {
		tbl <- tidyr::expand_grid(outcome = out)
	} else if (length(out) == 0 & length(exp) > 0) {
		tbl <- tidyr::expand_grid(exposure = exp)
	} else if (length(out) == 0 & length(exp) == 0) {
		tbl <- tidyr::expand_grid()
	}

	# This forms the right hand side variables
	# However fundamental decomposition breaks the rules generally
	cov <- c(exp, prd, con, med, int, sta)
	tbl <- tidyr::expand_grid(left = out, right = cov)
	message_fundamental_pattern(med, sta)

	# Return
	tbl

}


#' @rdname patterns
#' @export
apply_direct_pattern <- function(x) {

	# Term table
	tmTab <- vec_proxy(x)

	# Handle roles
	out <- tmTab$term[tmTab$role == "outcome"]
	exp <- tmTab$term[tmTab$role == "exposure"]
	prd <- tmTab$term[tmTab$role == "predictor"]
	con <- tmTab$term[tmTab$role == "confounder"]
	med <- tmTab$term[tmTab$role == "mediator"]
	int <- tmTab$term[tmTab$role == "interaction"]
	sta <- tmTab$term[tmTab$role == "strata"]

	# Outcomes and exposures should be set as a "key pair"
	if (length(out) > 0 & length(exp) > 0) {
		tbl <- tidyr::expand_grid(outcome = out, exposure = exp)
	} else if (length(out) > 0 & length(exp) == 0) {
		tbl <- tidyr::expand_grid(outcome = out)
	} else if (length(out) == 0 & length(exp) > 0) {
		tbl <- tidyr::expand_grid(exposure = exp)
	} else if (length(out) == 0 & length(exp) == 0) {
		tbl <- tidyr::expand_grid()
	}

	# Covariates would be everything
	cov <- c(prd, con, int)

	if (length(cov) > 0) {
		for (i in seq_along(cov)) {
			tbl <-
				tidyr::expand_grid(tbl, "{paste0('covariate_', i)}" := cov[i])
		}
	}

	# Return
	tbl
}

#' @rdname patterns
#' @export
apply_sequential_pattern <- function(x) {

	# Term table
	tmTab <- vec_proxy(x)

	# Handle roles
	out <- tmTab$term[tmTab$role == "outcome"]
	exp <- tmTab$term[tmTab$role == "exposure"]
	prd <- tmTab$term[tmTab$role == "predictor"]
	con <- tmTab$term[tmTab$role == "confounder"]
	med <- tmTab$term[tmTab$role == "mediator"]
	int <- tmTab$term[tmTab$role == "interaction"]
	sta <- tmTab$term[tmTab$role == "strata"]

	# Outcomes and exposures should be set as a "key pair"
	if (length(out) > 0 & length(exp) > 0) {
		tbl <- tidyr::expand_grid(outcome = out, exposure = exp)
	} else if (length(out) > 0 & length(exp) == 0) {
		tbl <- tidyr::expand_grid(outcome = out)
	} else if (length(out) == 0 & length(exp) > 0) {
		tbl <- tidyr::expand_grid(exposure = exp)
	} else if (length(out) == 0 & length(exp) == 0) {
		tbl <- tidyr::expand_grid()
	}

	## Covariate order
	# Covariates are all predictors on RHS
	# They need to be the same order as the original terms however
	# Cool enough, 'x' as a `tm` works for matching as a `character`
	# Interaction terms should be placed next to each other

	cov <- c(prd, con, int)
	cov <- cov[order(match(cov, x))]


	# Expand based on number of covariates
	for (i in seq_along(cov)) {
		tbl <-
			tidyr::expand_grid(tbl, "{paste0('covariate_', i)}" := c(NA, cov[i]))
	}

	# Remove rows that are not appropriate...
	# 	e.g. no exposure or covariates
	# 	e.g. doesn't follow sequential rules
	n <- length(cov)

	if (n > 0 & !("exposure" %in% names(tbl))) {
		tbl <- tbl[which(!is.na(tbl[["covariate_1"]])), ]
	}

	ntbl <- list()

	for (i in seq_along(cov)) {
		# Potential columns, may not exist
		pc <- paste0("covariate_", i - 1)
		cc <- paste0("covariate_", i + 0)
		nc <- paste0("covariate_", i + 1)

		if (i == 1 & n == 1) {
			 # If there is only a single term overall, then it must be present?
			ntbl <- list()
		} else if (i == 1) {
			# First term
			# If missing, future terms cannot be present either
			ntbl[[i]] <-
				tbl |>
				dplyr::filter(is.na(!!rlang::sym(cc)) & !is.na(!!rlang::sym(nc)))

		} else if (i == n) {
			# Last term
			# If present, previous term must also be present
			ntbl[[i]] <-
				tbl |>
				dplyr::filter(!is.na(!!rlang::sym(cc)) & is.na(!!rlang::sym(pc)))

		} else {
			# All other rows
			# If variable i is empty, i...n must also be empty
			ntbl[[i]] <-
				tbl |>
				dplyr::filter(
					(!is.na(!!rlang::sym(cc)) & is.na(!!rlang::sym(pc))) |
						(is.na(!!rlang::sym(cc)) & !is.na(!!rlang::sym(nc)))
				)

		}
	}

	# Combine the bad tables together and cull them from original tables
	ntbl <- unique(dplyr::bind_rows(ntbl))
	if (nrow(ntbl) > 0) {
		tbl <- suppressMessages(dplyr::anti_join(tbl, ntbl))
	}

	# Return
	tbl
}

#' @rdname patterns
#' @export
apply_parallel_pattern <- function(x) {

	# Term table
	tmTab <- vec_proxy(x)

	# Handle roles
	out <- tmTab$term[tmTab$role == "outcome"]
	exp <- tmTab$term[tmTab$role == "exposure"]
	prd <- tmTab$term[tmTab$role == "predictor"]
	con <- tmTab$term[tmTab$role == "confounder"]
	med <- tmTab$term[tmTab$role == "mediator"]
	int <- tmTab$term[tmTab$role == "interaction"]
	sta <- tmTab$term[tmTab$role == "strata"]

	# Outcomes and exposures should be set as a "key pair"
	if (length(out) > 0 & length(exp) > 0) {
		tbl <- tidyr::expand_grid(outcome = out, exposure = exp)
	} else if (length(out) > 0 & length(exp) == 0) {
		tbl <- tidyr::expand_grid(outcome = out)
	} else if (length(out) == 0 & length(exp) > 0) {
		tbl <- tidyr::expand_grid(exposure = exp)
	} else if (length(out) == 0 & length(exp) == 0) {
		tbl <- tidyr::expand_grid()
	}

	# This needs to handle the issue of grouped variables
	# Group = NA generic variables that can be parallelized
	# Group = set(0, inf) integers that must be placed together
	#   Covariate columns = max(group != NA)
	groupLevels <- with(tmTab, unique(group[!is.na(group)]))
	groupedCov <- list()
	for (g in groupLevels) {
	  groupedCov[[as.character(g)]] <-
	    with(tmTab, term[group == 0L & role != "exposure" & !is.na(group)])
	}

	# Ungrouped variables
	ungroupedCov <-
	  with(tmTab, term[side == "right" & role != "exposure" & is.na(group)]) |>
	  as.list()

	# Covariates
	covList <- c(ungroupedCov, groupedCov)

	tabList <- list()
	for (i in seq_along(covList)) {
	  cov <- covList[[i]]
	  rowList <- list()
	  for (j in seq_along(cov)) {
	    rowList[[j]] <-
	      #tidyr::expand_grid(tbl, "{paste0('covariate_', j)}" := cov[[j]])
	      #tibble::add_column(tbl, "{paste0('covariate_', j)}" := cov[[j]])
	      tibble::tibble("{paste0('covariate_', j)}" := cov[[j]])
	  }
	  tabList[[i]] <- dplyr::bind_cols(rowList)
	}

	tbl <-
	  tidyr::expand_grid(tbl, dplyr::bind_rows(tabList))

	# Return
	tbl
}

# Complex patterns -------------------------------------------------------------


#' @rdname patterns
#' @export
apply_rolling_interaction_pattern <- function(x) {

	# Goal is to have a single formula and apply interaction through
	# This would create multiple formulas, one for each interaction term
	# Cannot know the type of variable for this to work
	# Must occur within fmls function itself

	# Term table
	tmTab <- vec_proxy(x)

	# Handle roles
	out <- tmTab$term[tmTab$role == "outcome"]
	exp <- tmTab$term[tmTab$role == "exposure"]
	prd <- tmTab$term[tmTab$role == "predictor"]
	con <- tmTab$term[tmTab$role == "confounder"]
	med <- tmTab$term[tmTab$role == "mediator"]
	int <- tmTab$term[tmTab$role == "interaction"]
	sta <- tmTab$term[tmTab$role == "strata"]

	# Outcomes and exposures should be set as a "key pair"
	if (length(out) > 0 & length(exp) > 0) {
		tbl <- tidyr::expand_grid(outcome = out, exposure = exp)
	} else if (length(out) > 0 & length(exp) == 0) {
		tbl <- tidyr::expand_grid(outcome = out)
	} else if (length(out) == 0 & length(exp) > 0) {
		tbl <- tidyr::expand_grid(exposure = exp)
	} else if (length(out) == 0 & length(exp) == 0) {
		tbl <- tidyr::expand_grid()
	}

	# Crucially, the interaction term marks WHEN in the formula to apply the roll
	# This means that every term BEFORE the interaction term will be maintained

}

