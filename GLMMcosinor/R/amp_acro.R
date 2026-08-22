#' Used to specify a cosinor component in the model formula.
#'
#' Checks the validity of user inputs before creating an updated formula
#' and associated modifications to the \code{data.frame}.
#'
#' @param time_col A  \code{numeric} column within the \code{data.frame()}
#' passed by via the \code{data} arg containing the time values.
#' @param n_components The Number of cosinor components in the model.
#' @param group A vector of the names for the group factors (column names
#' within the \code{data.frame()} passed by via the \code{data} arg).
#' @param period A \code{numeric} value or vector containing the period.
#' The number of values should be equal to \code{n_components}.
#' @param ... Extra arguments for use within \code{GLMMcosinor}.
#'
#' @srrstats {G1.4a}
#' @srrstats {G2.0}
#' @srrstats {G2.1}
#' @srrstats {G2.2}
#' @srrstats {G2.4}
#' @srrstats {G2.4a}
#' @srrstats {G2.4b}
#' @srrstats {G2.4c}
#' @srrstats {G2.4d}
#' @srrstats {G2.5}
#' @srrstats {G2.9}
#' @srrstats {G2.6}
#' @srrstats {G5.0}
#' @srrstats {RE1.2}
#'
#' @return A \code{data.frame} and \code{formula} appropriate for use by
#' \code{data_processor()}.
#' @export
#' @examples
#' # Single component cosinor model
#' cglmm(
#'   vit_d ~ amp_acro(time_col = time, group = "X", period = 12),
#'   data = vitamind
#' )
#'
#' # 2-component cosinor model with simulated data
#' sim_data <- simulate_cosinor(
#'   n = 500,
#'   mesor = 5,
#'   amp = c(2, 1),
#'   acro = c(1, 1.5),
#'   beta.mesor = 2,
#'   beta.amp = c(2, 1),
#'   beta.acro = c(1, 1.5),
#'   family = "gaussian",
#'   period = c(12, 6),
#'   n_components = 2,
#'   beta.group = TRUE,
#' )
#'
#' cglmm(
#'   Y ~ group + amp_acro(times,
#'     n_components = 2,
#'     group = "group",
#'     period = c(12, 6)
#'   ),
#'   data = sim_data,
#'   family = gaussian
#' )
amp_acro <- function(time_col,
                     n_components = 1,
                     group,
                     period,
                     ...) {
  .amp_acro(time_col, n_components, group, period, .env = environment(), ...)
}

#' Checks the validity of user inputs and creates formula and modifies
#' \code{data.frame}.
#' @param time_col A  \code{numeric} column within the \code{data.frame()}
#' passed by via the \code{data} arg containing the time values.
#' @param n_components The Number of cosinor components in the model.
#' @param group A vector of the names for the group factors (column names
#' within the \code{data.frame()} passed by via the \code{data} arg).
#' @param period A \code{numeric} value or vector containing the period.
#' The number of values should be equal to \code{n_components}.
#' @param .data The dataframe from the original \code{cglmm()} call.
#' @param .formula The formula from the original \code{cglmm()} call.
#' @param .quietly controls whether messages from \code{amp_acro()} are
#' displayed. Defaults to \code{TRUE}.
#' @param .amp_acro_ind The index of the portion of the formula containing
#' amp_acro. -1 for main formula (default), 0 for \code{zi} or \code{disp}
#' formulae.
#' @param .data_prefix Prefix for columns to be added in the new
#' \code{data.frame}. Defaults to \code{"main_"}.
#' @param .env The environment in which to evaluate column names on the data
#' being passed.
#'
#' @srrstats {G1.4a}
#' @srrstats {G2.0}
#' @srrstats {G2.1}
#' @srrstats {G2.2}
#' @srrstats {G2.4}
#' @srrstats {G2.4a}
#' @srrstats {G2.4b}
#' @srrstats {G2.4c}
#' @srrstats {G2.4d}
#' @srrstats {G2.5}
#' @srrstats {G2.9}
#' @srrstats {G2.6}
#' @srrstats {G5.0}
#' @srrstats {RE1.2}
#'
#' @noRd
#' @return A \code{data.frame} and \code{formula} appropriate for use by
#' \code{data_processor()}.
.amp_acro <- function(time_col,
                      n_components,
                      group,
                      period,
                      no_amp_acro = FALSE,
                      no_amp_acro_vector,
                      cond_period,
                      .data,
                      .formula,
                      .quietly = TRUE,
                      .amp_acro_ind = -1,
                      .data_prefix = "main_",
                      .env) {
  if (missing(no_amp_acro_vector)) {
    no_amp_acro_vector <- NULL
  }
  no_amp_acro_vector <- no_amp_acro_vector

  # ensure .data argument is a dataframe, matrix, or tibble (tested)
  assertthat::assert_that(
    inherits(.data, "data.frame") | inherits(.data, "matrix") | inherits(
      .data,
      "tbl"
    ),
    msg = "'data' must be of class 'data.frame', 'matrix', or 'tibble'"
  )

  # Formatting .data argument as dataframe if matrix or tibble (tested)

  if (inherits(.data, "matrix") | inherits(.data, "tbl")) {
    .data <- as.data.frame(.data)
    if (!.quietly) {
      message("Data has been reformatted as dataframe") # (tested)
    }
  }

  amp_acro_iteration <- function(time_col,
                                 n_components,
                                 group,
                                 .formula,
                                 period,
                                 .quietly = TRUE,
                                 .data,
                                 .amp_acro_ind = -1,
                                 no_amp_acro,
                                 no_amp_acro_vector,
                                 cond_period) {
    if (n_components == 0) {
      no_amp_acro <- TRUE
    }

    # assess the quality of the inputs


    # Function to check if 'amp_acro' is in the formula
    check_amp_acro <- function(.formula) {
      # Convert formula to character and check for presence of 'amp_acro'
      formula_str <- as.character(.formula)
      if (any(grepl("amp_acro", formula_str))) {
        return(TRUE)
      } else {
        return(FALSE)
      }
    }

    # Check if 'amp_acro' is in the formula
    has_amp_acro <- check_amp_acro(.formula)
    if (!has_amp_acro) {
      no_amp_acro_vector[[.data_prefix]] <- TRUE
    } else {
      no_amp_acro_vector[[.data_prefix]] <- FALSE
    }

    if (!no_amp_acro) {
      # Ensure n_components is an integer > 0
      stopifnot(assertthat::is.count(n_components))
      # ensure period is numeric
      lapply(period, function(period) stopifnot(assertthat::is.number(period)))
      # ensure all periods are greater than 0
      stopifnot(all(period > 0))
      # check that .formula is of class 'formula'
      stopifnot(inherits(.formula, "formula"))
    }

    if (!no_amp_acro) {
      assertthat::assert_that(
        (paste(substitute(time_col, .env)) %in% colnames(.data)),
        msg = "time_col must be the name of a column in dataframe"
      )

      # the time_vector ttt is extracted based on the class of time_col argument
      if (is.character(substitute(time_col, .env))) {
        time_col <- noquote(substitute(time_col, .env))

        # extract the time vector
        ttt <- .data[[time_col]]
      } else {
        # ensure time_col is within the dataframe
        if (!inherits(substitute(time_col, .env), "name")) {
          stop("time_col must be name of column in data.")
        }

        # extract the time vector
        ttt <- eval(substitute(time_col, .env), envir = .data)
      }


      # ensure ttt contains numeric values only (tested)
      if (!assertthat::assert_that(is.numeric(ttt))) {
        stop("time column in dataframe must contain numeric values")
      }

      # ensure time_col is univariate (tested)
      assertthat::assert_that(is.vector(ttt),
        msg = "time_col must be univariate"
      )

      # Check if 'group' is a non-string and convert it to a string if necessary
      if (all(!is.character(substitute(group, .env))) & !missing(group)) {
        group_change <- as.character(substitute(group, .env))
        if (length(group_change) != 1) {
          group <- group
        } else {
          group <- group_change
        }
      }
    }

    # allow the user to not have any grouping structure
    if (missing(group)) {
      group <- 0
      group_check <- FALSE
    } else {
      if (all(is.na(group)) | all(is.null(group))) {
        group <- 0
        group_check <- FALSE
      } else {
        group_check <- TRUE
        check_group_var(.data = .data, group = group)
      }
    }

    # "group_check" variable is passed to cglmm to indicate if there is a
    # group argument present in amp_acro()

    # ensure the length of the grouping variable matches the value of
    # n_components. (tested) if one grouping variable is supplied but
    # n_components > 1, then the one grouping variable is repeated to match the
    # value of n_components
    if (!no_amp_acro) {
      if (length(group) != n_components) {
        if (length(group) == 1) {
          group <- rep(group, n_components)
        } else {
          stop(paste(
            "Grouping variable in amp_acro() must be of length 1 or",
            "the same as n_components"
          ))
        }
      }
    }
    group_original <- group
    # show error message if user uses 'rrr' or 'sss' in their grouping variable
    # name (tested)
    if (any(grepl("rrr", group) == TRUE) | any(grepl("sss", group) == TRUE)) {
      stop("Group variable names cannot contain 'rrr' or 'sss'")
    }

    # ensure the length of the period matches the value of n_components (tested)
    # if one period is supplied but n_components > 1, then the period is
    # repeated to match the value of n_components

    if (!no_amp_acro) {
      if (length(period) != n_components) {
        if (length(period) == 1) {
          period <- rep(period, n_components)
        } else {
          stop(paste(
            "period value(s) in amp_acro() must be of length 1 or",
            "the same as n_components"
          ))
        }
      }
    }

    # check for NA group values supplied by the user and replaces with zeroes.
    # this is important when creating the formula: 'newformula'.
    for (i in seq_along(group)) {
      if (is.na(group[i]) == TRUE) {
        group[i] <- 0
      }
    }

    # create a vector with just the named groups, disregarding 'zero'/NA
    # elements
    group_names <- group[group != 0]

    # Formatting the group columns in .data as factors
    for (i in group_names) {
      .data[[i]] <- factor(.data[[i]])
    }
    # get the terms and variable names from the amp_acro call
    Terms <- stats::terms(.formula, specials = "amp_acro")

    non_group_factors <- stats::terms(.formula, specials = "amp_acro") |>
      attr("factors") |>
      colnames() |>
      (\(x) {
        grep("amp_acro", x, invert = TRUE, value = TRUE)
      })()

    Terms$factors <- unique(c(non_group_factors, group_names))
    varnames <- get_varnames(Terms)
    # create the initial formula string


    spec_dex <- unlist(attr(Terms, "special")$amp_acro) + .amp_acro_ind

    if (!no_amp_acro) {
      non_acro_formula <- attr(Terms, "term.labels")[-spec_dex]
    } else {
      non_acro_formula <- attr(Terms, "term.labels")
    }

    if (!no_amp_acro) {
      # generate 'n_components' number of rrr and sss vectors
      n_count <- seq_len(n_components)
      vec_rrr <- (paste0(.data_prefix, "rrr", n_count)) # vector of rrr names
      vec_sss <- (paste0(.data_prefix, "sss", n_count)) # vector of sss names
      formula_expr <- NULL
      # adding the rrr and sss columns to the dataframe
      for (i in seq_len(n_components)) {
        #
        rrr_names <- eval(vec_rrr[i])
        sss_names <- eval(vec_sss[i])
        .data[[rrr_names]] <- cos(2 * pi * ttt / period[i])
        .data[[sss_names]] <- sin(2 * pi * ttt / period[i])

        # add a warning message that columns have been added to the dataframe
        if (!.quietly) {
          message(paste(
            rrr_names, "and", sss_names, "have been added to dataframe"
          ))
        }

        # if grouping variable is not 0 (NA), create interaction terms in the
        # formula
        if (group[i] != 0) {
          acpart <- paste((rep(group[i], 2)), c(rrr_names, sss_names), sep = ":")
          acpart_combined <- paste(acpart[1], acpart[2], sep = " + ")
          formula_expr <- paste(formula_expr, "+", acpart_combined)
        }

        # if grouping variable is 0 (or NA), do not create interaction terms
        # in the formula
        if (group[i] == 0) {
          acpart_combined <- NULL
          formula_expr <- paste(formula_expr, "+", rrr_names, "+", sss_names)
        }
      }
    }

    if (.amp_acro_ind == -1) {
      left_part <- all.vars(.formula, max.names = 1)
    } else {
      left_part <- NULL
    }

    if (no_amp_acro) {
      formula_expr <- NULL
      vec_rrr <- NULL
      vec_sss <- NULL
    }

    newformula <- stats::as.formula(
      paste(left_part,
        paste(
          c(
            attr(
              stats::terms(.formula),
              "intercept"
            ),
            non_acro_formula,
            formula_expr
          ),
          collapse = " + "
        ),
        sep = " ~ "
      )
    )
    newformula <- stats::update.formula(newformula, ~.)

    # storing the covariates. If none, then 'covariates' stored as 'NULL'
    if (is.character(non_acro_formula) && length(non_acro_formula) == 0) {
      covariates <- NULL
    } else {
      covariates <- non_acro_formula
    }

    # update the formula
    time_name <- paste(substitute(time_col, .env))
    # create NULL vectors for group metrics. These will be updated if there is
    # a group argument
    group_stats <- NULL
    if (group_check == TRUE) {
      for (i in group_names) {
        single_group_level <- levels(as.factor(.data[[i]]))
        group_stats[[i]] <- as.array(single_group_level)
      }
    }
    return(list(
      newdata = .data,
      newformula = newformula,
      vec_rrr = vec_rrr,
      vec_sss = vec_sss,
      n_components = n_components,
      period = period,
      group_stats = group_stats,
      group = group,
      group_check = group_check,
      time_name = time_name,
      response_var = left_part,
      group_original = group,
      covariates = covariates,
      no_amp_acro_vector = no_amp_acro_vector
    ))
  }
  if (no_amp_acro) {
    n_components <- 0
    period <- NULL
  }

  res <- amp_acro_iteration(
    time_col = time_col,
    n_components = n_components,
    group = group,
    .formula = lme4::nobars(.formula),
    period = period,
    .quietly = .quietly,
    .data = .data,
    .amp_acro_ind = .amp_acro_ind,
    no_amp_acro = no_amp_acro,
    no_amp_acro_vector = no_amp_acro_vector
  )

  # if a mixed model is specified, handle formula accordingly
  if (!is.null(lme4::findbars(.formula))) {
    ranef_part <- lapply(lme4::findbars(.formula), deparse1)
    ranef_parts_replaced <- lapply(ranef_part, function(x) {
      component_num <- regmatches(
        x, gregexpr("(?<=amp_acro)\\d+", x, perl = TRUE)
      )[[1]]
      if (length(component_num) == 0) {
        return(x)
      } else {
        for (i in seq_len(length(component_num))) {
          string_match <- paste0(
            ".*amp_acro", component_num[i], "\\s([^+|]*).*"
          )
          ranef_part_addition <- gsub(string_match, "\\1", x)
          ranef_part_group <- gsub(".*\\|\\s*(.*)", "\\1", x)

          rrr_part <- paste0("main_rrr", component_num[i], ranef_part_addition)
          sss_part <- paste0("main_sss", component_num[i], ranef_part_addition)

          x <- gsub(
            paste0(
              "amp_acro",
              component_num[i],
              " ",
              ranef_part_addition
            ),
            paste0(
              rrr_part,
              "+",
              sss_part
            ),
            x,
            fixed = TRUE
          )
          # x$group <- ranef_part_group
        }
        return(x)
      }
    })

    # ranef_groups is stored, and will be part of the eventual output. This is
    # a vector containing the names of the variables with mixed effects.

    ranef_groups <- unique(gsub(".*\\|\\s*", "", ranef_parts_replaced))

    ranef_part_updated <- paste(
      sprintf(
        "(%s)",
        ranef_parts_replaced
      ),
      collapse = "+"
    )

    main_part <- paste(paste(deparse(res$newformula), collapse = ""),
      ranef_part_updated,
      collapse = "", sep = "+"
    )
    res$newformula <- stats::as.formula(main_part)
    res$ranef_groups <- ranef_groups
    #

    # res$mixedef <- append(res$mixdef, )
    #
  } else {
    res$ranef_groups <- NA
  }

  res
}
