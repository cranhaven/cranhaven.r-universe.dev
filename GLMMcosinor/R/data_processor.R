#' Fit the cosinor GLMM model using the output from
#' \code{update_formula_and_data()} and a new formula
#'
#' @param obj Output from `update_formula_and_data()`.
#' @param formula A (optionally) new formula to use when fitting the cosinor
#' model (maybe with random effects) or other covariates found in the data.
#' @param ... Optional additional arguments passed to \code{glmmTMB::glmmTMB()}.
#'
#' @return Returns a fitted cosinor model as a \code{cglmm} object.
#' @srrstats {G1.4}
#' @export
#'
#' @examples
#' # Use vitamind data but add a "patient" identifier used as a random effect
#' vitamind2 <- vitamind
#' vitamind2$patient <- sample(
#'   LETTERS[1:5],
#'   size = nrow(vitamind2), replace = TRUE
#' )
#'
#' # Use update_formula_and_data() to perform wrangling steps of cglmm()
#' # without yet fitting the model
#' data_and_formula <- update_formula_and_data(
#'   data = vitamind2,
#'   formula = vit_d ~ X + amp_acro(time,
#'     group = "X",
#'     period = 12
#'   )
#' )
#'
#' # print formula from above
#' data_and_formula$newformula
#'
#' # fit model while adding random effect to cosinor model formula.
#' mod <- fit_model_and_process(
#'   obj = data_and_formula,
#'   formula = update.formula(
#'     data_and_formula$newformula, . ~ . + (1 | patient)
#'   )
#' )
#'
#' mod
#' mod$fit # printing the `glmmTMB` model within shows Std.Dev. of random effect
fit_model_and_process <- function(obj, formula, ...) {
  if (!missing(formula)) {
    obj$newformula <- formula
  }
  do.call(data_processor, obj)
}


#' Process and fit the data using glmmTMB after initial processing
#' by data_utils.R.
#'
#' @param newdata A processed \code{data.frame} with rrr and sss columns added.
#' @param newformula A processed \code{formula} with rrr and sss components.
#' @param vec_sss A vector of sss for each component.
#' (eg, \code{c("sss1, sss2")}).
#' @param vec_rrr A vector of sss for each component.
#' (eg, \code{c("sss1, sss2")}).
#' @param n_components The number of components specified in the model formula.
#' @param group_stats A vector containing the number of levels per grouping
#' variable.
#' @param group The original \code{group} argument.
#' @param group_check A \code{logical}. Whether a grouping argument is present.
#' @param period A vector of values for the period of each component.
#' @param family The \code{family} for fitting the model.
#' @param Terms A \code{terms} object from the original \code{cglmm()}
#' call.
#' @param ... Optional additional arguments passed to \code{glmmTMB::glmmTMB()}.
#'
#' @srrstats {RE1.0}
#' @srrstats {RE1.3}
#' @srrstats {RE1.3a}
#' @srrstats {RE1.4}
#' @srrstats {RE2.0}
#' @srrstats {RE2.1}
#' @srrstats {RE4.0}
#' @srrstats {RE4.2}
#' @srrstats {RE4.4}
#' @srrstats {G1.4}
#' @srrstats {G2.13}
#'
#' The following standards are covered in the glmmTMB package
#' @srrstats {RE2.2}
#' @srrstats {RE3.0}
#' @srrstats {RE3.1}
#' @srrstats {RE4.8}
#' @srrstats {RE4.10}
#' @srrstats {RE4.11}
#' @srrstats {RE4.12}
#' @srrstats {RE4.13}
#' @srrstats {G1.4a}
#' @srrstats {G2.3}
#' @srrstats {G2.3a}
#' @srrstats {G2.3b}
#' @srrstats {G2.14}
#' @srrstats {G2.14a}
#' @srrstats {G2.14b}
#' @srrstats {G2.14c}
#' @srrstats {G2.15}
#' @srrstats {G2.16}
#'
#' @return A \code{cglmm} model.
#' @noRd
data_processor <- function(newdata,
                           newformula,
                           vec_sss,
                           vec_rrr,
                           n_components,
                           group_stats,
                           group,
                           group_check,
                           period,
                           time_name,
                           family,
                           Terms,
                           cglmm.calls,
                           dispformula,
                           dispformula_check,
                           dispformula_used,
                           ziformula,
                           ziformula_check,
                           ziformula_used,
                           response_var,
                           group_original,
                           ranef_groups,
                           covariates,
                           no_amp_acro_vector,
                           ...) {
  group_names <- names(group_stats)

  dispformula_val <- dispformula$formula
  ziformula_val <- ziformula$formula

  # Fit the data and formula to a model
  fit <- glmmTMB::glmmTMB(
    formula = newformula,
    data = newdata,
    family = family,
    dispformula = dispformula_val,
    ziformula = ziformula_val,
    ...
  )
  # Retrieve the fit, coefficients from the model and priming vectors
  # in preparation for transforming the raw coefficients
  mf <- fit

  main_coefs <- glmmTMB::fixef(mf)$cond
  if (no_amp_acro_vector[["main_"]]) {
    conditional_model <- main_coefs
  } else {
    conditional_model <- get_new_coefs(
      main_coefs,
      vec_rrr,
      vec_sss,
      n_components,
      period
    )
    items_keep <- c(
      "formula",
      "vec_rrr",
      "vec_sss",
      "n_components",
      "group_stats",
      "group_check"
    )
  }

  if (dispformula_used) {
    disp_coefs <- glmmTMB::fixef(mf)$disp
    if (no_amp_acro_vector[["disp_"]]) {
      dispersion_model <- disp_coefs
    } else {
      dispersion_model <- get_new_coefs(
        disp_coefs,
        dispformula$vec_rrr,
        dispformula$vec_sss,
        dispformula$n_components,
        period
      )
    }
    disp_list <- c(
      dispformula[items_keep],
      list(
        coefficients = dispersion_model,
        raw_coefficients = disp_coefs,
        group = dispformula$group_disp # currently not being used
      )
    )

    names(disp_list) <- paste0(names(disp_list), "_disp")
  } else {
    disp_list <- NULL
  }

  if (ziformula_used) {
    zi_coefs <- glmmTMB::fixef(mf)$zi
    if (no_amp_acro_vector[["zi_"]]) {
      zi_model <- zi_coefs
      ziformula[items_keep] <- NULL
      zi_list <- c(
        list(
          coefficients = zi_model,
          raw_coefficients = zi_coefs,
          group = ziformula$group_zi # currently not being used
        )
      )
    } else {
      zi_model <- get_new_coefs(
        zi_coefs,
        ziformula$vec_rrr,
        ziformula$vec_sss,
        ziformula$n_components,
        period
      )
      zi_list <- c(
        ziformula[items_keep],
        list(
          coefficients = zi_model,
          raw_coefficients = zi_coefs,
          group = ziformula$group_zi # currently not being used
        )
      )
    }

    names(zi_list) <- paste0(names(zi_list), "_zi")
  } else {
    zi_list <- NULL
  }

  # update calls
  if (missing(cglmm.calls)) {
    cglmm.calls <- list()
  }
  cglmm.calls$data_processor <- match.call()

  # Arrange the output
  structure(
    list(
      formula = newformula,
      fit = fit,
      cglmm.calls = cglmm.calls,
      Terms = Terms,
      coefficients = conditional_model,
      raw_coefficients = main_coefs,
      vec_sss = vec_sss,
      vec_rrr = vec_rrr,
      period = period,
      time_name = time_name,
      n_components = n_components,
      group_stats = group_stats,
      group = group,
      group_check = group_check,
      dispformula_check = dispformula_check,
      dispformula_used = dispformula_used,
      ziformula_check = ziformula_check,
      ziformula_used = ziformula_used,
      disp_list = disp_list,
      zi_list = zi_list,
      response_var = response_var,
      newdata = newdata,
      group_original = group_original,
      ranef_groups = ranef_groups,
      covariates = covariates
    ),
    class = "cglmm"
  )
}
