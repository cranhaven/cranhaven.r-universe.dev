#' Test for differences in a cosinor model between components.
#'
#' Given a time variable and optional covariates, generate inference a cosinor
#' fit. For the covariate named (or vector of covariates), this function
#' performs a Wald test comparing the group with covariates equal to 1 to the
#' group with covariates equal to 0. This may not be the desired result for
#' continuous covariates.
#'
#'
#' @param x An \code{cglmm} object.
#' @param x_str A \code{character}. The name of the grouping variable within
#' which differences in the selected cosinor characteristic (amplitude or
#' acrophase) will be tested. If there is no grouping variable in the model,
#' then this can be left as NULL (default).
#' @param param A \code{character}. Either \code{"amp"} or \code{"acr"} for
#' testing differences in amplitude or acrophase, respectively.
#' @param comparison_A An \code{integer}. Refers to the component number that is
#'  to act as the reference group.
#' for the comparison.
#' @param comparison_B An \code{integer}. Refers to the component number that is
#' to act as the comparator group
#' @param level_index An \code{integer}. If
#' \code{comparison_type = "components"}, \code{level_index} indicates which
#' level of the grouping variable is being used for the comparison between
#' components.
#' @param ci_level The level for calculated confidence intervals. Defaults to
#' \code{0.95}.
#'
#' @return Returns a \code{test_cosinor} object.
#' @export
#'
#' @examples
#' data_2_component <- simulate_cosinor(
#'   n = 10000,
#'   mesor = 5,
#'   amp = c(2, 5),
#'   acro = c(0, pi),
#'   beta.mesor = 4,
#'   beta.amp = c(3, 4),
#'   beta.acro = c(0, pi / 2),
#'   family = "gaussian",
#'   n_components = 2,
#'   period = c(10, 12),
#'   beta.group = TRUE
#' )
#' mod_2_component <- cglmm(
#'   Y ~ group + amp_acro(times,
#'     n_components = 2, group = "group",
#'     period = c(10, 12)
#'   ),
#'   data = data_2_component
#' )
#' test_cosinor_components(mod_2_component, param = "amp", x_str = "group")
test_cosinor_components <- function(x,
                                    x_str = NULL,
                                    param = "amp",
                                    comparison_A = 1,
                                    comparison_B = 2,
                                    level_index = 0,
                                    ci_level = 0.95) {
  # Validating the inputs


  if (param == "amp" | param == "acr") {
  } else {
    # Display an error message if the parameter is invalid
    stop("Invalid parameter. Expected 'amp' or 'acro'.")
  }

  validate_ci_level(ci_level)

  assertthat::assert_that(
    inherits(x, "cglmm"),
    msg = "'x' must be of class 'cglmm'"
  )


  if (!is.null(x_str)) {
    stopifnot(is.character(x_str))
  }

  # assertthat::assert_that(
  #   length(grep(x_str, names(x$coefficients))) > 0,
  #   msg = "x_str must be the name of a group in object"
  # )


  assertthat::assert_that(
    comparison_A %in% seq_len(x$n_components) & comparison_B %in% seq_len(x$n_components),
    msg = paste(
      "'comparison_A' and 'comparison_B' must be numbers",
      "corresponding to a component in the model"
    )
  )
  if (!is.null(x_str)) {
    assertthat::assert_that(
      level_index %in% x$group_stats[[x$group_original[comparison_A]]] &
        level_index %in% x$group_stats[[x$group_original[comparison_B]]],
      msg = paste(
        "'level_index' must be supplied and it must be a number",
        "corresponding to a level in the model"
      )
    )
  }

  # passing these inputs into the internal function

  test_obj <- .test_cosinor(
    x = x,
    x_str = x_str,
    param = param,
    comparison_A = comparison_A,
    comparison_B = comparison_B,
    level_index = level_index,
    ci_level = ci_level,
    comparison_type = "components"
  )
  test_obj
}


#' Test for differences in a cosinor model between levels of the grouping
#' variable.
#'
#' Given a time variable and optional covariates, generate inference a cosinor
#' fit. For the covariate named (or vector of covariates), this function
#' performs a Wald test comparing the group with covariates equal to 1 to the
#' group with covariates equal to 0. This may not be the desired result for
#' continuous covariates.
#'
#' @param x An \code{cglmm} object.
#' @param x_str A \code{character}. The name of the grouping variable within
#' which differences in the selected cosinor characteristic (amplitude or
#' acrophase) will be tested.
#' @param param A \code{character}. Either \code{"amp"} or \code{"acr"} for
#' testing differences in amplitude or acrophase, respectively.
#' @param comparison_A An \code{integer, or string}. Refers to the first level
#' within the grouping variable \code{x_str} that is to act as the reference
#' group in the comparison. Ensure that it corresponds to the name of the level
#' in the original dataset.
#' @param comparison_B An \code{integer, or string}. Refers to the second level
#' within the grouping variable \code{x_str} that is to act as the comparator
#' group in the comparison. Ensure that it corresponds to the name of the level
#' in the original dataset.
#' @param component_index An \code{integer}. If
#' \code{comparison_type = "levels"}, \code{component_index} indicates which
#' component is being compared between the levels of the grouping variable.
#' @param ci_level The level for calculated confidence intervals. Defaults to
#' \code{0.95}.
#'
#'
#' @return Returns a \code{test_cosinor} object.
#' @export
#'
#' @examples
#' data_2_component <- simulate_cosinor(
#'   n = 10000,
#'   mesor = 5,
#'   amp = c(2, 5),
#'   acro = c(0, pi),
#'   beta.mesor = 4,
#'   beta.amp = c(3, 4),
#'   beta.acro = c(0, pi / 2),
#'   family = "gaussian",
#'   n_components = 2,
#'   period = c(10, 12),
#'   beta.group = TRUE
#' )
#' mod_2_component <- cglmm(
#'   Y ~ group + amp_acro(times,
#'     n_components = 2, group = "group",
#'     period = c(10, 12)
#'   ),
#'   data = data_2_component
#' )
#' test_cosinor_levels(mod_2_component, param = "amp", x_str = "group")
test_cosinor_levels <- function(x,
                                x_str,
                                param = "amp",
                                comparison_A,
                                comparison_B,
                                component_index = 1,
                                ci_level = 0.95) {
  # Validating the inputs

  if (param == "amp" | param == "acr") {
  } else {
    # Display an error message if the parameter is invalid
    stop("Invalid parameter. Expected 'amp' or 'acr'.")
  }

  validate_ci_level(ci_level)

  assertthat::assert_that(
    inherits(x, "cglmm"),
    msg = "'x' must be of class 'cglmm'"
  )

  stopifnot(is.character(x_str))

  assertthat::assert_that(
    length(grep(x_str, names(x$coefficients))) > 0,
    msg = "x_str must be the name of a group in object"
  )



  # If there are no levels supplied, the first two levels will be compared
  if (missing(comparison_A) & missing(comparison_B)) {
    comparison_A <- x$group_stats[[x_str]][1]
    comparison_B <- x$group_stats[[x_str]][2]
  }

  assertthat::assert_that(
    comparison_A %in% x$group_stats[[x_str]],
    msg = "'comparison_A' must correspond to a level within the group
    specified by 'x_str'"
  )

  assertthat::assert_that(
    comparison_B %in% x$group_stats[[x_str]],
    msg = paste(
      "'comparison_B' must correspond to a level within the",
      "group specified by 'x_str'"
    )
  )


  assertthat::assert_that(
    component_index %in% 1:x$n_components,
    msg = paste(
      "'component_index' must be supplied and it must be a",
      "number corresponding to a component in the model"
    )
  )


  # passing these inputs into the internal function
  test_obj <- .test_cosinor(
    x = x,
    x_str = x_str,
    param = param,
    comparison_A = comparison_A,
    comparison_B = comparison_B,
    component_index = component_index,
    ci_level = ci_level,
    comparison_type = "levels"
  )
  test_obj
}



#' Test for differences in a cosinor model. This function has been replaced
#' with a more user-friendly and intuitive way of specifying comparisons:
#' \code{test_cosinor_components} and \code{test_cosinor_levels}.
#' These external functions use this internal function.
#'
#' Given a time variable and optional covariates, generate inference a cosinor
#' fit. For the covariate named (or vector of covariates), this function
#' performs a Wald test comparing the group with covariates equal to 1 to the
#' group with covariates equal to 0. This may not be the desired result for
#' continuous covariates.
#'
#'
#' @param x An \code{cglmm} object.
#' @param x_str A \code{character}. The name of the grouping variable within
#' which differences in the selected cosinor characteristic (amplitude or
#' acrophase) will be tested.
#' @param param A \code{character}. Either \code{"amp"} or \code{"acr"} for
#' testing differences in amplitude or acrophase, respectively.
#' @param comparison_A An \code{integer}. Refers to the level (within the
#' grouping variable) or component number that is to act as the reference group
#' for the comparison.
#' @param comparison_B An \code{integer}. Refers to the level (within the
#' grouping variable) or component number that is to act as the comparator group
#' for the comparison.
#' @param comparison_type A \code{character}. Indicates whether the comparison
#' to be performed is to be between levels of a grouping variable
#' (\code{"levels"}) or indices of components in a multiple-component cosinor
#' model (\code{"components"}).
#' @param component_index An \code{integer}. If
#' \code{comparison_type = "levels"}, \code{component_index} indicates which
#' component is being compared between the levels of the grouping variable.
#' @param level_index An \code{integer}. If
#' \code{comparison_type = "components"}, \code{level_index} indicates which
#' level of the grouping variable is being used for the comparison between
#' components.
#' @param ci_level The level for calculated confidence intervals. Defaults to
#' \code{0.95}.
#'
#' @return Returns a \code{test_cosinor} object.
#' @examples
#' mod_grouped <- cglmm(vit_d ~ X + amp_acro(time,
#'   group = "X",
#'   n_components = 1,
#'   period = 12
#' ), data = vitamind)
#'
#' test_cosinor(mod_grouped, "X", "amp")
#'
#' data_2_component <- simulate_cosinor(
#'   n = 10000,
#'   mesor = 5,
#'   amp = c(2, 5),
#'   acro = c(0, pi),
#'   beta.mesor = 4,
#'   beta.amp = c(3, 4),
#'   beta.acro = c(0, pi / 2),
#'   family = "gaussian",
#'   n_components = 2,
#'   period = c(10, 12)
#' )
#'
#' mod_2_component <- cglmm(
#'   Y ~ group + amp_acro(times,
#'     n_components = 2, group = "group",
#'     period = c(10, 12)
#'   ),
#'   data = data_2_component
#' )
#'
#' test_cosinor(mod_2_component, param = "amp", comparison_type = "components")
#'
#' @noRd
.test_cosinor <- function(x,
                          x_str,
                          param,
                          comparison_A,
                          comparison_B,
                          comparison_type,
                          component_index,
                          level_index,
                          ci_level) {
  # TODO handle the text labels for the factor variable used for grouping - i.e
  # using data made with : sim_data %>% mutate(group = ifelse(group == 1,
  # "control", "treatment"))
  summary.fit <- summary(x)

  if (is.null(x_str)) {
    x_str_length <- 1
  } else {
    x_str_length <- length(x_str)
  }

  index <- matrix(0, ncol = length(x$coefficients), nrow = x_str_length)
  colnames(index) <- names(x$coefficients)

  if (comparison_type == "components") {
    if (is.null(x_str)) {
      index[1, paste0(param, comparison_A)] <- -1
      index[1, paste0(param, comparison_B)] <- 1
    } else {
      for (i in seq_along(x_str)) {
        index[i, paste0(x_str[i], level_index, ":", param, comparison_A)] <- -1
        index[i, paste0(x_str[i], level_index, ":", param, comparison_B)] <- 1
      }
    }
  }

  if (comparison_type == "levels") {
    if (x$n_components == 1) {
      component_index <- ""
    }

    for (i in seq_along(x_str)) {
      index[i, paste0(
        x_str[i],
        comparison_A, ":", param, component_index
      )] <- -1
      index[i, paste0(
        x_str[i],
        comparison_B, ":", param, component_index
      )] <- 1
    }
  }

  if (param == "acr") {
    # get the smallest valid difference between estimates for acrophase
    # (ie, -pi, and pi should have a difference of 0, not 2*pi)
    diff_raw <- index %*% x$coefficients
    diff.est_c <- c(abs(diff_raw), abs(diff_raw + 2 * pi), abs(diff_raw - 2 * pi))
    min_index <- which.min(diff.est_c)
    diff.est <- as.matrix(diff.est_c[min_index])
  } else {
    diff.est <- index %*% x$coefficients
  }

  diff.var <- index[
    , grep("(amp|acr)", names(x$coefficients)),
    drop = FALSE
  ] %*%
    summary.fit$transformed.covariance %*%
    t(index[, grep("(amp|acr)", names(x$coefficients)), drop = FALSE])

  glob.chi <- (diff.est %*% solve(diff.var) %*% t(diff.est))[1, 1]
  ind.Z <- diff.est / sqrt(diag(diff.var))

  # get the quantile corresponding to ci_level
  zt <- stats::qnorm((1 - ci_level) / 2, lower.tail = F)

  interval <- cbind(
    diff.est,
    diff.est - zt * sqrt(diag(diff.var)),
    diff.est + zt * sqrt(diag(diff.var))
  )

  global.test <- structure(list(
    statistic = glob.chi,
    df = dim(diff.var)[1],
    conf.int = NULL,
    p.value = stats::pchisq(glob.chi, df = dim(diff.var)[1], lower.tail = FALSE)
  ), class = "cglmmSubTest")

  ind.test <- structure(list(
    statistic = ind.Z[, ],
    df = NULL,
    conf.int = interval,
    ci_level = ci_level,
    p.value = 2 * stats::pnorm(-abs(ind.Z))[, ],
    names = x_str
  ), class = "cglmmSubTest")

  if (comparison_type == "components") {
    test_details <- list(
      x = x,
      x_str = x_str,
      param = param,
      comparison_A = comparison_A,
      comparison_B = comparison_B,
      comparison_type = comparison_type,
      level_index = level_index,
      ci_level = ci_level
    )
  }

  if (comparison_type == "levels") {
    test_details <- list(
      x = x,
      x_str = x_str,
      param = param,
      comparison_A = comparison_A,
      comparison_B = comparison_B,
      comparison_type = comparison_type,
      component_index = component_index,
      ci_level = ci_level
    )
  }

  structure(
    list(
      global.test = global.test,
      ind.test = ind.test,
      test_details = test_details
    ),
    class = "cglmmTest"
  )
}


#' Print results of test of cosinor model
#'
#' @param x A \code{test_cosinor} object.
#' @param ... Arguments passed to \code{print}
#'
#' @return \code{print(x)} returns \code{x} invisibly.
#' @examples
#' data_2_component <- simulate_cosinor(
#'   n = 10000,
#'   mesor = 5,
#'   amp = c(2, 5),
#'   acro = c(0, pi),
#'   beta.mesor = 4,
#'   beta.amp = c(3, 4),
#'   beta.acro = c(0, pi / 2),
#'   family = "gaussian",
#'   n_components = 2,
#'   period = c(10, 12),
#'   beta.group = TRUE
#' )
#' mod_2_component <- cglmm(
#'   Y ~ group + amp_acro(times,
#'     n_components = 2, group = "group",
#'     period = c(10, 12)
#'   ),
#'   data = data_2_component
#' )
#' test_cosinor_levels(
#'   mod_2_component,
#'   param = "amp",
#'   x_str = "group"
#' )
#' @export
print.cglmmTest <- function(x, ...) {
  cat("Test Details: \n")
  .print_details(x$test_details)

  cat("\n\n\nGlobal test: \n")
  print(x$global.test, ...)
  cat("\n\n\nIndividual tests:\n")
  print(x$ind.test, ...)

  invisible(x)
}

#' Print test of model
#'
#' @param x A \code{sub_test_cosinor} object.
#' @param ... Additional, ignored arguments.
#'
#' @return \code{print(x)} returns \code{x} invisibly.
#'
#' @examples
#' data_2_component <- simulate_cosinor(
#'   n = 10000,
#'   mesor = 5,
#'   amp = c(2, 5),
#'   acro = c(0, pi),
#'   beta.mesor = 4,
#'   beta.amp = c(3, 4),
#'   beta.acro = c(0, pi / 2),
#'   family = "gaussian",
#'   n_components = 2,
#'   period = c(10, 12),
#'   beta.group = TRUE
#' )
#' mod_2_component <- cglmm(
#'   Y ~ group + amp_acro(times,
#'     n_components = 2, group = "group",
#'     period = c(10, 12)
#'   ),
#'   data = data_2_component
#' )
#' test_output <- test_cosinor_levels(
#'   mod_2_component,
#'   param = "amp",
#'   x_str = "group"
#' )
#' print(test_output$global.test)
#' @export
print.cglmmSubTest <- function(x, ...) {
  if (length(x$statistic) == 1) {
    cat("Statistic: \n")
    cat(round(x$statistic, 2))
    cat("\n\nP-value: \n")
    cat(round(x$p.value, 4))

    if (!is.null(x$conf.int)) {
      ci <- round(x$conf.int, 2)
      cat(paste0(
        "\n\nEstimate and ", x$ci_level * 100, "% confidence interval:\n"
      ))
      cat(paste0(ci[1], " (", ci[2], " to ", ci[3], ")"))
    }
  } else {
    ci <- round(x$conf.int, 2)

    msat <- data.frame(
      statistic = round(x$statistic),
      estimate = paste0(ci[, 1], " (", ci[, 2], " to ", ci[, 3], ")"),
      p.value = round(x$p.value, 4)
    )

    rownames(msat) <- x$names
  }
  invisible(x)
}

#' Print extra details about \code{test_cosinor} comparison.
#'
#' @param test_details A \code{list} of details from \code{test_cosinor}.
#'
#' @return \code{.print_details(x)} returns \code{x} invisibly.
#' @noRd
.print_details <- function(test_details) {
  # Print parameter being assessed
  cat(paste0(
    "Parameter being tested:\n",
    switch(test_details$param,
      amp = "Amplitude",
      acr = "Acrophase"
    )
  ))

  # Print type of comparison
  cat("\n\nComparison type:\n")
  cat(test_details$comparison_type)

  # If comparison is on levels of a factor...
  #     - specify factor variable
  #     - levels being compared
  #     - the component being held constant
  if (test_details$comparison_type == "levels") {
    cat(paste0(
      "\n\nGrouping variable used for comparison between groups: ",
      test_details$x_str,
      "\nReference group: ", test_details$comparison_A,
      "\nComparator group: ", test_details$comparison_B
    ))

    if (test_details$component_index == "") {
      cat("\n\ncglmm model only has a single component and to compare
          between groups.\n")
    } else {
      cat(paste0(
        "\n\ncglmm model has", test_details$x$n_components,
        " components. Component ", test_details$component_index,
        " is being used for comparison between groups.\n"
      ))
    }
  }

  # If comparison is on components of the model...
  #     - specify the component indices being compared
  #     - the grouping variable (if there is one) being held constant
  #       and its value
  if (test_details$comparison_type == "components") {
    cat(paste0(
      "\n\nComponent indices used for comparison between groups: ",
      test_details$x_str,
      "\nReference component: ", test_details$comparison_A,
      "\nComparator component: ", test_details$comparison_B
    ))
  }

  invisible(test_details)
}
