#' Summarize a cosinor model
#' Given a time variable and optional covariates, generate inference a cosinor
#' fit. Gives estimates, confidence intervals, and tests for the raw parameters,
#' and for the mean, amplitude, and acrophase parameters. If the model includes
#' covariates, the function returns the estimates of the mean, amplitude, and
#' acrophase for the group with covariates equal to 1 and equal to 0. This may
#' not be the desired result for continuous covariates.
#'
#' @param object An object of class \code{cglmm}
#' @param ci_level The level for calculated confidence intervals. Defaults to
#' 0.95.
#' @param ... Currently unused
#'
#' @srrstats {G1.4}
#' @srrstats {RE4.18}
#'
#' @return Returns a summary of the `cglmm` model as
#' a `cglmmSummary` object.
#' @examples
#'
#'
#' fit <- cglmm(vit_d ~ X + amp_acro(time,
#'   group = "X",
#'   n_components = 1,
#'   period = 12
#' ), data = vitamind)
#' summary(fit)
#'
#' @export

summary.cglmm <- function(object, ci_level = 0.95, ...) {
  # get the fitted model from the cglmm() output, along with
  # n_components, vec_rrr, and vec_sss
  mf <- object$fit
  n_components <- object$n_components
  vec_rrr <- object$vec_rrr
  vec_sss <- object$vec_sss

  validate_ci_level(ci_level)


  # this function can be looped if there is disp or zi formula present.
  # note that 'model_index' is a string: 'cond', 'disp', or 'zi'
  cglmmSubSummary <- function(model_index) {
    if (model_index == "disp") {
      n_components <- object$disp_list$n_components_disp
    }
    if (model_index == "zi") {
      n_components <- object$zi_list$n_components_zi
    }

    # get the arguments from the function wrapping this function
    args <- match.call()[-1]
    coefs <- glmmTMB::fixef(mf)[[model_index]]

    # reassign vec_rrr and vec_sss to those in the disp or zi model, if
    # necessary
    if (model_index == "disp") {
      vec_rrr <- object$disp_list$vec_rrr_disp
      vec_sss <- object$disp_list$vec_sss_disp
    }

    if (model_index == "zi") {
      vec_rrr <- object$zi_list$vec_rrr_zi
      vec_sss <- object$zi_list$vec_sss_zi
    }

    # create objects r.coef, s.coef, and mu.coef. This will be Boolean vectors
    # that indicate the position of particular coefficients in coefs.
    r.coef <- NULL
    s.coef <- NULL
    mu.coef <- NULL
    mu_inv <- rep(0, length(names(coefs)))

    # put a '|' between adjecent elements of vec_rrr and vec_sss, used for
    # indexing.
    if (length(vec_rrr) > 1) {
      vec_rrr_spec <- vec_rrr[1]
      vec_sss_spec <- vec_sss[1]
      for (i in 2:length(vec_rrr)) {
        vec_rrr_spec <- paste0(vec_rrr_spec, "|", vec_rrr[i])
        vec_sss_spec <- paste0(vec_sss_spec, "|", vec_sss[i])
      }
    } else {
      vec_rrr_spec <- vec_rrr
      vec_sss_spec <- vec_sss
    }

    # Get a Boolean vector for rrr, sss, and mu. This will be used to extract
    # the relevant raw parameters from the raw coefficient model output
    r.coef <- grepl(vec_rrr_spec, names(coefs))
    s.coef <- grepl(vec_sss_spec, names(coefs))

    # Keep track of non-mesor terms
    mu_inv_carry <- r.coef + s.coef
    # Ultimately, every non-mesor term will be true
    mu_inv <- mu_inv_carry + mu_inv

    # invert 'mu_inv' to get a Boolean vector for mesor terms a matrix of rrr
    # coefficients
    mu.coef <- c(!mu_inv)
    r.coef <- (t(matrix(unlist(r.coef), ncol = length(r.coef))))
    # a matrix of sss coefficients
    s.coef <- (t(matrix(unlist(s.coef), ncol = length(s.coef))))

    # generate coefs containing sss, and rrr, respectively
    beta.s <- coefs[s.coef]
    beta.r <- coefs[r.coef]


    # convert beta.s and beta.r to groups
    groups.r <- c(beta.r, beta.r[which(names(beta.r) != names(beta.r))])
    groups.s <- c(beta.s, beta.s[which(names(beta.s) != names(beta.s))])

    # calculate parameters amp and acr
    amp <- sqrt(groups.r^2 + groups.s^2)
    # acr <- -atan2(groups.s, groups.r)
    acr <- atan2(groups.s, groups.r)


    # rename the vectors amp and acr
    for (i in seq_len(n_components)) {
      names(amp) <- gsub(vec_rrr[i], paste0("amp", i), names(amp))
      names(acr) <- gsub(vec_sss[i], paste0("acr", i), names(acr))
    }

    # calculate the variance-covariance matrix
    vmat <- stats::vcov(mf)[[model_index]][
      c(which(r.coef), which(s.coef)),
      c(which(r.coef), which(s.coef))
    ]

    # if n_components = 1, then print "amp" and "acr" rather than "amp1", "acr1"
    if (n_components == 1) {
      names(amp) <- gsub(vec_rrr, "amp", names(amp))
      names(acr) <- gsub(vec_sss, "acr", names(acr))
      new_coefs <- c(coefs[mu.coef], unlist(amp), unlist(acr))
    }

    # determine the partial derivatives of amplitude and acrophase

    # a_r is the partial derivative of amp with respect to r.
    # hence, a_r = d(amp)/d(groups.r),
    # where amp = sqrt(groups.r^2 + groups.s^2). Likewise for a_s

    a_r <- (groups.r^2 + groups.s^2)^(-0.5) * groups.r
    a_s <- (groups.r^2 + groups.s^2)^(-0.5) * groups.s

    # b_r is the partial derivative of acr with respect to r.
    # hence, b_r = d(acr)/d(groups.r),
    # where acr = arctan(s/r). Likewise for b_s
    b_r <- (1 / (1 + (groups.s^2 / groups.r^2))) * (-groups.s / groups.r^2)
    b_s <- (1 / (1 + (groups.s^2 / groups.r^2))) * (1 / groups.r)

    # jac is the jacobian matrix, a matrix of partial derivatives
    if (length(groups.r) == 1) {
      jac <- matrix(c(a_r, a_s, b_r, b_s), byrow = TRUE, nrow = 2)
    } else {
      jac <- rbind(
        cbind(diag(a_r), diag(a_s)),
        cbind(diag(b_r), diag(b_s))
      )
    }

    # apply the delta method
    cov.trans <- (jac) %*% vmat %*% t(jac)
    se.trans <- sqrt(diag(cov.trans))

    # assemble summary matrix
    coef <- c(coefs[mu.coef], unlist(amp), unlist(acr))
    se <- c(sqrt(diag(stats::vcov(mf)[[model_index]]))[mu.coef], se.trans)


    zt <- stats::qnorm((1 - ci_level) / 2, lower.tail = F)
    raw.se <- sqrt(diag(stats::vcov(mf)[[model_index]]))

    rawmat <- cbind(
      estimate = coefs,
      standard.error = raw.se,
      lower.CI = coefs - zt * raw.se,
      upper.CI = coefs + zt * raw.se,
      p.value = 2 * stats::pnorm(-abs(coefs / raw.se))
    )

    smat <- cbind(
      estimate = coef,
      standard.error = se,
      lower.CI = coef - zt * se,
      upper.CI = coef + zt * se,
      p.value = 2 * stats::pnorm(-abs(coef / se))
    )

    if (object$group_check) {
      rownames(smat) <- update_covnames(rownames(smat), object$group_stats)
    }

    structure(
      list(
        transformed.table = as.data.frame(smat),
        raw.table = as.data.frame(rawmat),
        transformed.covariance = cov.trans,
        raw.covariance = vmat
      ),
      class = "cglmmSubSummary"
    )
  }

  # store the output from the conditional model
  main_output <- cglmmSubSummary(model_index = "cond")

  # store the output from the dispersion model (if present)
  if (object$dispformula_check) {
    output_disp <- cglmmSubSummary(model_index = "disp")
  } else {
    output_disp <- NULL
  }

  # store the output from the zero-inflation model (if present)
  if (object$ziformula_check) {
    output_zi <- cglmmSubSummary(model_index = "zi")
  } else {
    output_zi <- NULL
  }

  # here, the outputs from main_output are renamed to remove the main_output tag
  # this was done to remain cohesive with other parts of the package.
  structure(list(
    transformed.table = main_output$transformed.table,
    raw.table = main_output$raw.table,
    transformed.covariance = main_output$transformed.covariance,
    raw.covariance = main_output$raw.covariance,
    main_output = main_output,
    output_disp = output_disp,
    output_zi = output_zi,
    object = object
  ), class = "cglmmSummary")
}


#' Print the summary of a cosinor model
#'
#' @param x An object of class \code{cglmmSummary}
#' @param digits Controls the number of digits displayed in the summary output
#' @param ... Currently unused
#'
#' @srrstats {G1.4}
#' @return `print` returns `x` invisibly.
#' @examples
#'
#' fit <- cglmm(vit_d ~ X + amp_acro(time,
#'   group = "X",
#'   n_components = 1,
#'   period = 12
#' ), data = vitamind)
#' summary(fit)
#'
#' @export
#'

# check if there is dispersion or zi (as opposed to default) then print
print.cglmmSummary <- function(x,
                               digits = getOption("digits"), ...) {
  cat("\n Conditional Model \n")
  cat("Raw model coefficients:\n")
  stats::printCoefmat(x$main_output$raw.table,
    digits = digits,
    has.Pvalue = TRUE
  )
  cat("\n")
  cat("Transformed coefficients:\n")
  stats::printCoefmat(x$main_output$transformed.table,
    digits = digits,
    has.Pvalue = TRUE
  )

  # display the output from the dispersion model (if present)

  if (!is.null(x$output_disp)) {
    cat("\n***********************\n")
    cat("\n Dispersion Model \n")
    cat("Raw model coefficients:\n")
    stats::printCoefmat(x$output_disp$raw.table,
      digits = digits,
      has.Pvalue = TRUE
    )
    cat("\n")
    cat("Transformed coefficients:\n")
    stats::printCoefmat(x$output_disp$transformed.table,
      digits = digits,
      has.Pvalue = TRUE
    )
  }

  if (!is.null(x$output_zi)) {
    cat("\n***********************\n")
    cat("\n Zero-Inflation Model \n")
    cat("Raw model coefficients:\n")
    stats::printCoefmat(x$output_zi$raw.table,
      digits = digits,
      has.Pvalue = TRUE
    )
    cat("\n")
    cat("Transformed coefficients:\n")
    stats::printCoefmat(x$output_zi$transformed.table,
      digits = digits,
      has.Pvalue = TRUE
    )
  }
  invisible(x)
}
