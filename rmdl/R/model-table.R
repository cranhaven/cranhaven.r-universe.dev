# Class ----------------------------------------------------------------------

#' Model tables
#'
#' @description
#'
#' `r lifecycle::badge('experimental')`
#'
#' The `model_table()` or `mdl_tbl()` function creates a `mdl_tbl` object that
#' is composed of either `fmls` objects or `mdl` objects, which are
#' thin/informative wrappers for generic formulas and hypothesis-based models.
#' The `mdl_tbl` is a data frame of model information, such as model fit,
#' parameter estimates, and summary statistics about a model, or a formula if it
#' has not yet been fit.
#'
#' @details
#' The table itself allows for ease of organization of model information and has
#' three additional, major components (stored as scalar attributes).
#'
#' 1. A formula matrix that describes the terms used in each model, and how they
#' are combined.
#'
#' 1. A term table that describes the terms and their properties and/or labels.
#'
#' 1. A list of datasets used for the analyses that can help support additional diagnostic testing.
#'
#' We go into further detail in the sections below.
#'
#' @section Data List
#'
#' @section Term Table
#'
#' @section Formula Matrix
#'
#' @param ... Named or unnamed `mdl` or `fmls` objects
#'
#' @param data A `data.frame` or `tbl_df` object, named correspondingly to
#'   the underlying data used in the models (to help match)
#'
#' @param x A `mdl_tbl` object
#' 
#' @return A `mdl_tbl` object, which is essentially a `data.frame` with
#'   additional information on the relevant data, terms, and formulas used to
#'   generate the models.
#'
#' @name mdl_tbl
#' @importFrom tibble tibble new_tibble
#' @export
mdl_tbl <- function(..., data = NULL) {

	# Steps:
	# 	Assess model...
	# 		Model fit/information
	# 		Parameter estimates
	#			Formula/terms
	#		Re-organize information into...
	#			Model data frame
	#			Formula "matrix"
	#			Terms (+/- labels and other meta info)

	# Call
	mc <- match.call()

	dots <- rlang::list2(...)
	if (length(dots) == 0) {
		return(new_model_table())
	}

	# Initially any number of objects could be given
	# These are all pushed into a list together
	# The object may or may not be named, and may or may not be plural
	if (length(dots) == 1) {

		# If unnamed, return to simplest form
		# If it is named, leave it alone (obviously a named list)
		if (is.null(names(dots))) {
			dots <- dots[[1]]
		}
	}

	# Model Table Lists...
	mtl <- vector("list", length(dots))

	for (i in seq_along(dots)) {
		if (inherits(dots[[i]], "mdl")) {
			mtl[[i]] <- construct_table_from_models(dots[i])
		}
		if (inherits(dots[[i]], "fmls")) {
			mtl[[i]] <- construct_table_from_formulas(dots[[i]])
		}
	}

	# Convert into a single table
	mdTab <- do.call(vec_rbind, mtl)

	# Once it comes back as a new class, we need to add data if its available
	if (!is.null(data)) {
		datLs <- attr(mdTab, "dataList")
		dataName <- as.character(mc$data)
		datLs[[dataName]] <- data
		attr(mdTab, "dataList") <- datLs
	}

	# Return new class
	mdTab
}

#' @rdname mdl_tbl
#' @export
model_table <- mdl_tbl

#' @rdname mdl_tbl
#' @export
is_model_table <- function(x) {
	inherits(x, "mdl_tbl")
}

#' @keywords internal
#' @noRd
methods::setOldClass(c("mdl_tbl", "vctrs_vctr"))

#' @export
print.mdl_tbl <- function(x, ...) {
	cat(sprintf("<%s>\n", class(x)[[1]]))
	cli::cat_line(format(x)[-1])
}

#' @export
vec_ptype_full.mdl_tbl <- function(x, ...) {
	"model_table"
}

#' @export
vec_ptype_abbr.mdl_tbl <- function(x, ...) {
	"mdl_tbl"
}

# Constructors -----------------------------------------------------------------

#' Restructure models to fit within a model table
#' Passes information to `new_model_table()` for initialization
#' @param x Vector of `mdl` objects
#' @keywords internal
construct_table_from_models <- function(x, ...) {

	# Global variables
	role <- NULL

	# Meta components of the models
	obj <- x[[1]] # Removes it from its list
	nm <- ifelse(is.null(names(x)), NA, ifelse(names(x) == "", NA, names(x)))
	n <- length(obj)
	rid <- sapply(obj, rlang::hash)
	fits <- rep(TRUE, n)

	# Components of the model fields
	mc <- unlist(field(obj, "modelCall"))
	ma <- field(obj, "modelArgs")
	pe <- field(obj, "parameterEstimates")
	si <- field(obj, "summaryInfo")

	# Get terms and formulas
	mf <- field(obj, "modelFormula")
	tl <- lapply(mf, key_terms)
	fl <- unname(sapply(mf, as.character, USE.NAMES = FALSE))

	# Formula IDs require checking against the formula matrix
	fid <- lapply(mf, unlist, recursive = FALSE)
	fmMat <-
		lapply(mf, vec_proxy) |>
		do.call(what = rbind, args = _) |>
		vec_data()

	# Terms must be combined into a term table for later look up
	tmTab <-
		lapply(tl, vec_data) |>
		do.call(what = rbind, args = _) |>
		unique()

	out <-
		sapply(tl, function(.x) {
			.y <- as.character(dplyr::filter(.x, role == "outcome"))
			if (length(.y) == 0) {
				.y <- NA_character_
			} else {
				.y
			}
		}) |>
		as.character()
	exp <-
		sapply(tl, function(.x) {
			.y <- as.character(dplyr::filter(.x, role == "exposure"))
			if (length(.y) == 0) {
				.y <- NA_character_
			} else {
				.y
			}
		}) |>
		as.character()
	med <-
		sapply(tl, function(.x) {
			.y <- as.character(dplyr::filter(.x, role == "mediator"))
			if (length(.y) == 0) {
				.y <- NA_character_
			} else {
				.y
			}
		}) |>
		as.character()
	int <-
		sapply(tl, function(.x) {
			.y <- as.character(dplyr::filter(.x, role == "interaction"))
			.z <- .y[!grepl(":", .y)]
			if (length(.z) == 0) {
				.z <- NA_character_
			} else {
				.z
			}
		}) |>
		as.character()

	# Get all data names and strata variables back
	da <- field(obj, "dataArgs")
	did <- sapply(da, function(.x) {
		.x$dataName
	})
	sta <- sapply(da, function(.x) {
		.x$strataVariable
	})
	slvl <- sapply(da, function(.x) {
		.x$strataLevel
	})

	# Initialize a new list
	res <- df_list(
		id = rid,
		formula_index = fid,
		data_id = did,
		name = nm,
		model_call = mc,
		formula_call = fl,
		outcome = out,
		exposure = exp,
		mediator = med,
		interaction = int,
		strata = sta,
		level = slvl,
		model_parameters = pe,
		model_summary = si,
		fit_status = fits
	)

	# Return
	new_model_table(res,
									formulaMatrix = fmMat,
									termTable = tmTab,
									dataList = list())

}

#' Restructure formulas to fit within a model table
#' @param x Vector of `fmls` objects
#' @keywords internal
construct_table_from_formulas <- function(x, ...) {

	# Global variables
	role <- NULL

	# Meta components of the models
	obj <- x[[1]] # Removes it from its list
	nm <- ifelse(is.null(names(x)), NA, ifelse(names(x) == "", NA, names(x)))
	n <- nrow(obj)
	rid <- apply(obj, MARGIN = 1, rlang::hash) # Since formulas are matrices
	fits <- rep(FALSE, n)

	# Get terms and formulas
	mf <- obj
	tl <- formulas_to_terms(mf)
	fl <- as.character(stats::formula(mf))

	# Formula IDs require checking against the formula matrix
	fid <- apply(mf, MARGIN = 1, list) |> unlist(recursive = FALSE)
	fmMat <- vec_data(mf)

	# Terms must be combined into a term table for later look up
	tmTab <-
		lapply(tl, vec_data) |>
		do.call(what = rbind, args = _) |>
		unique()

	out <- sapply(tl, function(.x) {
		.y <- as.character(filter(.x, role == "outcome"))
		if (length(.y) == 0) {
			.y <- NA_character_
		} else {
			.y
		}
	})
	exp <- sapply(tl, function(.x) {
		.y <- as.character(filter(.x, role == "exposure"))
		if (length(.y) == 0) {
			.y <- NA_character_
		} else {
			.y
		}
	})
	med <- sapply(tl, function(.x) {
		.y <- as.character(filter(.x, role == "mediator"))
		if (length(.y) == 0) {
			.y <- NA_character_
		} else {
			.y
		}
	})
	int <- sapply(tl, function(.x) {
		.y <- as.character(filter(.x, role == "interaction"))
		.z <- .y[!grepl(":", .y)]
		if (length(.z) == 0) {
			.z <- NA_character_
		} else {
			.z
		}
	})

	# Data will be empty because this is just a formula
	# Strata may be present, but levels cannot be
	sta <- sapply(tl, function(.x) {
		.y <- as.character(filter(.x, role == "strata"))
		if (length(.y) == 0) {
			.y <- NA_character_
		} else {
			.y
		}
	})

	# Initialize a new list
	res <- df_list(
		id = rid,
		formula_index = fid,
		data_id = NA,
		name = nm,
		model_call = NA,
		formula_call = fl,
		outcome = out,
		exposure = exp,
		mediator = med,
		interaction = int,
		strata = sta,
		level = NA,
		model_parameters = NA,
		model_summary = NA,
		fit_status = fits
	)

	# Data list (empty by default)
	datLs <- list()

	# Return
	new_model_table(res,
									formulaMatrix = fmMat,
									termTable = tmTab,
									dataList = datLs)
}

#' @keywords internal
new_model_table <- function(x = list(),
														formulaMatrix = data.frame(),
														termTable = data.frame(),
														dataList = list(),
														...) {

	# Invariant rules:
	#		Can add and remove rows (each row is essentially a model)
	#		Rows can be re-ordered
	#		Columns cannot be re-ordered
	#
	#	Invariant columns:
	#		Key Relationship: outcome and exposure, roles, etc
	#		Context: Formula (giving information on covariates) and Model Type
	#		Fit: Individual parameters and model level estimates/statistics

	if (length(x) == 0) {
		stop("No data was available to be coerced to a `mdl_tbl` object.")
	}

	validate_class(formulaMatrix, "data.frame")
	validate_class(termTable, "data.frame")
	validate_class(dataList, "list")

	tibble::new_tibble(
		x,
		formulaMatrix = formulaMatrix,
		termTable = termTable,
		dataList = dataList,
		class = "mdl_tbl"
	)
}

#' @importFrom dplyr dplyr_reconstruct
#' @export
dplyr_reconstruct.mdl_tbl <- function(data, template) {
  model_table_reconstruct(x = data, to = template)
}

#' @importFrom dplyr dplyr_row_slice
#' @export
dplyr_row_slice.mdl_tbl <- function(data, i, ...) {
	model_table_reconstruct(vec_slice(data, i), data)
}

#' @keywords internal
model_table_reconstruct <- function(x, to) {
	if (model_table_reconstructable(x, to)) {
		df_reconstruct(x, to)
	} else {
		# Return without reconstructing...
		x <- as.data.frame(x)
		message("Removing invariant columns in `mdl_tbl` returns `data.frame` object")
		x
	}

}

#' If objects are model tables, attributes are carried over to subset object
#' @keywords internal
df_reconstruct <- function(x, to) {

	# Remove global variables
	term <- role <- NULL

	validate_model_table(to)

	# Formula matrix
	# Used hash-ids to get the right formulas
	fmMat <- attr(to, "formulaMatrix")
	newMat <-
		fmMat[which(to$id %in% x$id), ] |>
		{
			\(.x) {
				.y <- colSums(.x)
				.y[is.na(.y)] <- 0
				.x[which(.y > 0)]
			}
		}()

	# Special terms and the rest of terms in formulas
	out <- unique(to$outcome)
	exp <- unique(to$exposure)
	med <- unique(to$mediator)
	int <- unique(to$interaction)
	sta <- unique(to$strata)
	special <- stats::na.omit(c(out, exp, med, int, sta))
	others <- setdiff(names(newMat), special)

	# Terms and term tables
	tmTab <- attr(to, "termTable")
	newTab <- dplyr::bind_rows(
		filter(tmTab, term %in% out & role == "outcome"),
		filter(tmTab, term %in% exp & role == "exposure"),
		filter(tmTab, term %in% med & role == "mediator"),
		filter(tmTab, term %in% int & role == "interaction"),
		filter(tmTab, term %in% sta & role == "strata"),
		filter(tmTab, term %in% others)
	)

	# Datasets
	# Combine the datasets and make sure they are unique
	datLs <- attr(to, "dataList")
	dataNames <- unique(to$data_id)
	newDat <- datLs[dataNames]

	# Update attributes of template/target
	attr(to, "termTable") <- newTab
	attr(to, "formulaMatrix") <- newMat
	attr(to, "dataList") <- newDat

	# Incorporate new attributes and return
	attrs <- attributes(to)
	attrs$names <- names(x)
	attrs$row.names <- .row_names_info(x, type = 0L)
	attributes(x) <- attrs

	# Return the original table (= x) with updated attributes
	# This will then be "dplyr"-ed into the new table (= to)
	x

}

#' Can `mdl_tbl` be reconstructed based on invariants?
#' @keywords internal
#' @param x data frame that will have invariants checked
#' @param to the tibble subclass of `mdl_tbl` that would be reconstructed
model_table_reconstructable <- function(x, to) {

	# Invariant columns must be present
	XCols <- names(x)
	ToCols <- names(to)
	if (all(is.element(XCols, ToCols))) {
		return(TRUE)
	} else {
		return(FALSE)
	}

}

#' Model table object validation
#' @keywords internal
#' @param x data frame that will have invariants checked
validate_model_table <- function(x) {

	# Invariant columns must be present
	cols <- names(x)

	modelCols <-
		c("model_call",
			"formula_call",
			"outcome",
			"model_parameters",
			"model_summary")

	formulaCols <-
		c("formula_call", "outcome")

	if (all(modelCols %in% cols)) {
		colStatus <- TRUE
	} else if (all(formulaCols %in% cols)) {
		colStatus <- TRUE
	} else {
		colStatus <- FALSE
	}

	stopifnot(
		"Model table does not contain invariant columns" = isTRUE(colStatus)
	)

	invisible(x)

}

# Casting and coercion ---------------------------------------------------------

# SELF

#' @keywords internal
mdl_tbl_ptype2 <- function(x, y, ..., x_arg = "", y_arg = "") {

  # Create a temporary/new structure of the table
	mdTab <- tib_ptype2(x, y, ..., x_arg = x_arg, y_arg = y_arg)

	# Terms must be coalesced together
	xTm <- attr(x, "termTable")
	yTm <- attr(y, "termTable")
	tmTab <-
		rbind(xTm, yTm) |>
		unique()

	# Formula matrices must bound together
	xFm <- attr(x, "formulaMatrix")
	yFm <- attr(y, "formulaMatrix")
  fmMat <-
    dplyr::bind_rows(xFm, yFm) |>
	  {
	    \(.x) replace(.x, is.na(.x), 0)
	  }()

  # Datasets must be pulled together
  xDat <- attr(x, "dataList")
  yDat <- attr(y, "dataList")
  datLs <-
    list(xDat, yDat) |>
    purrr::list_flatten()

  # Output with correct scalar attributes
	new_model_table(x = as.list(mdTab),
	                formulaMatrix = fmMat,
	                termTable = tmTab,
									dataList = datLs)

}

#' @keywords internal
mdl_tbl_cast <- function(x, to, ..., x_arg = "", to_arg = "") {

	# Terms must be coalesced together
	toTm <- attr(to, "termTable")
	xTm <- attr(x, "termTable")
	tmTab <-
		rbind(toTm, xTm) |>
		unique()

	# Formula matrices must bound together
	toFm <- attr(to, "formulaMatrix")
	xFm <- attr(x, "formulaMatrix")
  fmMat <-
    dplyr::bind_rows(toFm, xFm) |>
	  {
	     \(.x) replace(.x, is.na(.x), 0)
	  }()

  # Create a temporary/new structure of the table
  mdTab <- tib_cast(x, to, ..., x_arg = x_arg, to_arg = to_arg)

  # Output with correct scalar attributes
	new_model_table(x = as.list(mdTab),
	                formulaMatrix = fmMat,
	                termTable = tmTab)

}

#' @export
vec_ptype2.mdl_tbl.mdl_tbl <- function(x, y, ...) {
	mdl_tbl_ptype2(x, y, ...)
}

#' @export
vec_cast.mdl_tbl.mdl_tbl <- function(x, to, ...) {
	mdl_tbl_cast(x, to, ...)
}

# Model Table Helper Functions ------------------------------------------------

#' Model table helper functions
#'
#' @description
#'
#' `r lifecycle::badge('experimental')`
#' 
#' These functions are used to help manage the `mdl_tbl` object. They allow
#' for specific manipulation of the internal components, and are intended to
#' generally extend the functionality of the object.
#'
#' - `attach_data()`: Attaches a dataset to a `mdl_tbl` object
#' - `flatten_models()`: Flattens a `mdl_tbl` object down to its specific parameters
#'
#' # Attaching Data
#'
#' When models are built, oftentimes the included matrix of data is available
#' within the raw model, however when handling many models, this can be
#' expensive in terms of memory and space. By attaching datasets independently
#' that persist regardless of the underlying models, and by knowing which models
#' used which datasets, it can be ease to back-transform information.
#'
#' # Flattening Models
#'
#' A `mdl_tbl` object can be flattened to its specific parameters, their
#' estimates, and model-level summary statistics. This function additionally
#' helps by allowing for exponentiation of estimates when deemed appropriate.
#' The user can specify which models to exponentiate by name. This heavily
#' relies on the [broom::tidy()] functionality.
#'
#' @param x A `mdl_tbl` object
#'
#' @param data A `data.frame` object that has been used by models
#'
#' @param exponentiate A `logical` value that determines whether to exponentiate
#' the estimates of the models. Default is `FALSE`. If `TRUE`, the user can specify
#' which models to exponentiate by name using the __which__ argument.
#'
#' @param which A `character` vector of model names to exponentiate. Default is `NULL`. If __exponentiate__ is set to `TRUE` and __which__ is set to `NULL`, then all estimates will be exponentiated, which is often a *bad idea*.
#'
#' @param ... Arguments to be passed to or from other methods
#'
#' @return When using `attach_data()`, this returns a modified version of the
#'   `mdl_tbl` object however with the dataset attached. When using the
#'   `flatten_models()` function, this returns a simplified `data.frame` of the
#'   original model table that contains the model-level and parameter-level
#'   statistics. 
#'   
#' @name model_table_helpers
NULL

#' @rdname model_table_helpers
#' @export
attach_data <- function(x, data, ...) {

  validate_class(x, "mdl_tbl")
  validate_class(data, "data.frame")

	# Get name of object that will be the dataset
	mc <- match.call()
	datLs <- attr(x, 'dataList')

	# Add to data list
	dataName <- as.character(mc$data)
	datLs[[dataName]] <- data

	# Update attributes
	attr(x, 'dataList') <- datLs

	# Return
	x
}

#' @rdname model_table_helpers
#' @export
flatten_models <- function(x, exponentiate = FALSE, which = NULL, ...) {

	# Remove global variables
	model_statistic <- model_p_value <- model_parameters <- model_summary <-
		fit_status <- formula_call <- estimate <- conf_low <- conf_high <- NULL

	validate_class(x, "mdl_tbl")

	y <-
	  x |>
		tibble::tibble() |>
		dplyr::filter(fit_status == TRUE) |>
		dplyr::mutate(number = sapply(formula_call, function(.x) {
			.x |>
				stats::formula() |>
				stats::terms() |>
				labels() |>
				length()
		}, USE.NAMES = FALSE)) |>
		dplyr::select(dplyr::any_of(c(
			"formula_call",
			"model_call",
			"data_id",
			"name",
			"number",
			"outcome",
			"exposure",
			"mediator",
			"interaction",
			"strata",
			"level",
			"model_parameters",
			"model_summary"
		))) |>
		tidyr::unnest_wider(model_summary) |>
		dplyr::rename(dplyr::any_of(c(
			model_statistic = 'statistic',
			model_p_value = 'p_value'
		))) |>
		tidyr::unnest(model_parameters)

	# Exponentiate estimates based on user input
	if (exponentiate) {
		if (is.null(which)) {
		  y <-
		    y |>
		    dplyr::mutate(dplyr::across(c(estimate, conf_low, conf_high), ~ exp(.x)))
		} else {
		  y <-
		    y |>
		    dplyr::mutate(dplyr::across(c(estimate, conf_low, conf_high), ~ dplyr::if_else(name %in% which, exp(.x), .x)))
		}
	}

	# Return
	y

}
