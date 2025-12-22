#' @importFrom generics fit
#' @export
generics::fit

#' @export
fit.fmls <- function(object,
										 .fn,
										 ...,
										 data,
										 raw = TRUE) {

	# Global variables
	role <- NULL

	cl <- match.call()
	dots <- list(...)

	# Validate functions
	.fn <- as.character(cl[[3]])
	stopifnot("The .fn argument supplied is not yet supported" =
							.fn %in% .models)

	# Check data
	stopifnot(is.data.frame(data))
	dataName <- deparse1(cl[["data"]])

	# Models to be returned
	if (raw) {
		ml <- list()
	} else {
		ml <- mdl()
	}

	for (i in 1:nrow(object)) {
		t <- formulas_to_terms(object)[[i]]

		f <- stats::as.formula(t)
		sta <- filter(t, role == "strata")

		# If no strata, can model simply
		if (length(sta) == 0) {
			dots$data <- quote(data)
			x <- do.call(.fn, args = c(formula = f, dots))
			x$call[["formula"]] <- str2lang(deparse1(x$call[["formula"]]))
			x$call[["data"]] <- as.name(dataName)

			# Handle model list based on how output (list or `mdl`)
			if (raw) {
				y <- list(x)
				ml <- append(ml, y)
			} else {
				y <- mdl(x, formulas = object[i, ], data_name = dataName)
				ml <- c(ml, y)
			}
		} else {
			# Must now model along the levels of the strata terms
			for (j in seq_along(sta)) {

				# Ignores NA values
				strata <- as.character(sta[j])
				strataLevels <- unique(stats::na.omit(data[[strata]]))

				for (k in seq_along(strataLevels)) {

					# Organize and get the individual strata term and level
					strataData <- data[data[[strata]] == strataLevels[k],]
					dots$data <- quote(strataData)
					x <- do.call(.fn, args = c(formula = f, dots))
    			x$call[["formula"]] <- str2lang(deparse1(x$call[["formula"]]))
    			x$call[["data"]] <- as.name(dataName)

					# Handle model list based on how output (list or `mdl`)
					if (raw) {
						y <- list(x)
						ml <- append(ml, y)
					} else {
						y <-
							mdl(
								x,
								formulas = object[i,],
								data_name = dataName,
								strata_variable = strata,
								strata_level = strataLevels[k]
							)

						ml <- c(ml, y)
					}
				}
			}
		}

	}

	# Return the models in either list form or modified as `mdl`
	ml

}

#' @importFrom generics tidy
#' @export
generics::tidy

#' Create a "fail-safe" of tidying fits
#' @noRd
my_tidy <- function(x,
										conf.int = TRUE,
										conf.level = 0.95,
										exponentiate = FALSE,
										...) {
	broom::tidy(x,
							conf.int = conf.int,
							conf.level = conf.level,
							exponentiate = FALSE) |>
		janitor::clean_names()
}

#' Local load of it if not when package starts
#' @noRd
possible_tidy <-
	purrr::possibly(my_tidy, otherwise = NA, quiet = FALSE)

#' @importFrom generics glance
#' @export
generics::glance

#' Create a "fail-safe" of glance at fits
#' @noRd
my_glance <- function(x, ...) {
	broom::glance(x) |>
		janitor::clean_names()
}

#' Local load of it if not when package starts
#' @noRd
possible_glance <-
	purrr::possibly(my_glance, otherwise = NA, quiet = FALSE)
