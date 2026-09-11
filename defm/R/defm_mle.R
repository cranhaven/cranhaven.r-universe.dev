#' Maximum Likelihood Estimation of DEFM
#'
#' Fits a Discrete Exponential-Family Model using Maximum Likelihood.
#'
#' @param object An object of class [DEFM].
#' @param start Double vector. Starting point for the MLE.
#' @param lower,upper Lower and upper limits for the optimization (passed to
#' [stats4::mle].)
#' @param ... Further arguments passed to [stats4::mle].
#' @export
#' @import stats4
#' @details 
#' The likelihood function of the DEFM is closely-related to the 
#' Exponential-Family Random Graph Model \[ERGM\]. Furthermore, the DEFM can 
#' be treated as a generalization of the ERGM. The model implemented here
#' can be viewed as an ERGM for a bipartite network, where the actors are
#' individuals and the events are the binary outputs.
#' 
#' If the model features no markov terms, i.e., terms that depend on more than
#' one output, then the model is equivalent to a logistic regression. The 
#' example below shows this equivalence.
#' 
#'  
#' @return An object of class [stats4::mle].
#' @examples
#' #' Using Valente's SNS data
#' data(valentesnsList)
#' 
#' # Creating the DEFM object
#' logit_0 <- new_defm(
#'   id = valentesnsList$id,
#'   X = valentesnsList$X,
#'   Y = valentesnsList$Y[,1,drop=FALSE],
#'   order = 0
#' )
#' 
#' # Building the model
#' td_logit_intercept(logit_0)
#' td_logit_intercept(logit_0, covar = "Hispanic")
#' td_logit_intercept(
#'   logit_0,
#'   covar = "exposure_smoke"
#' )
#' td_logit_intercept(logit_0, covar = "Grades")
#' init_defm(logit_0) # Needs to be initialized
#' 
#' # Fitting the model
#' res_0 <- defm_mle(logit_0)
#' 
#' # Refitting the model using GLM
#' res_glm <- with(
#'   valentesnsList,
#'   glm(Y[,1] ~ X[,1] + X[,3] + X[,7], family = binomial())
#'   )
#' 
#' # Comparing results
#' summary_table(res_0)
#' summary(res_glm)
#' 
#' # Comparing the logodds
#' head(logodds(logit_0, par = coef(res_0), i = 0, j = 0))
#' 
#' @seealso [DEFM] for objects of class DEFM and [loglike_defm()] for the
#' log-likelihood function of DEFMs.
#' @references 
#' Vega Yon, G. G., Pugh, M. J., & Valente, T. W. (2022). Discrete Exponential-Family Models for Multivariate Binary Outcomes (arXiv:2211.00627). arXiv. \url{https://arxiv.org/abs/2211.00627}
defm_mle <- function(
  object,
  start,
  lower,
  upper,
  ...
  ) {

  if (missing(start))
    start <- as.list(structure(rep(0, nterms_defm(object)), names = names(object) ))

  if (missing(lower))
    lower <- rep(-20, length(nterms_defm(object)))

  if (missing(upper))
    upper <- rep(20, length(nterms_defm(object)))

  if (!inherits(object, "DEFM"))
    stop("-object- must be an object of class \"DEFM\"")

  minuslog <- sprintf(
    "function(%s) {
      par <- c(%1$s)
      -loglike_defm(object, par, as_log = TRUE)
    }", paste0("`", names(start), "`", collapse = ", ")
  )

  minuslog <- gsub("\\\\", "\\\\\\", minuslog)

  minuslog <- eval(parse(text = minuslog))

  stats4::mle(
    minuslogl = minuslog,
    start     = start,
    lower     = lower,
    upper     = upper,
    method    = "L-BFGS-B",
    nobs      = nrow_defm(object) + ifelse(
      morder_defm(object) > 0, -nobs_defm(object), 0L
      ),
    ...
  )

}

#' @importFrom stats pnorm
pval_calc <- function(obj) {
  stats::pnorm(
    -abs(stats4::coef(obj)/sqrt(diag(stats4::vcov(obj))))
    ) * 2
}

#' @export
#' @param as_texreg When `TRUE`, wraps the result in a texreg object
#' @details The function `summary_table` computes pvalues and returns a table
#' with the estimates, se, and pvalues. If `as_texreg = TRUE`, then it will
#' return a texreg object.
#' @rdname defm_mle
summary_table <- function(object, as_texreg = FALSE, ...) {

  # Generating the output table
  tab <- list(
    coef.names  = names(stats4::coef(object)),
    coef        = stats4::coef(object),
    se          = sqrt(diag(stats4::vcov(object))),
    gof.names   = c("AIC", "BIC", "N"),
    gof         = c(stats4::AIC(object), stats4::BIC(object), stats4::nobs(object)),
    gof.decimal = c(T, T, F),
    pvalues     = pval_calc(object)
  )

  if (as_texreg) {

    if (!length(find.package("texreg", quiet = TRUE)))
      stop("To use texreg you need to have the package installed in your system. Currently it seems to be not available.")

    do.call(texreg::createTexreg, tab)

  } else {

    do.call(
      data.frame,
      tab[c("coef", "se", "pvalues")]
      )

  }

}

#' Takes a list of a single fitted model and groups the terms into
#' interpretable groups.
#' @param fits A fitted model or a list of fitted models.
#' @param skip_intercept TRUE/FALSE. Whether to skip the intercept terms
#' from the output.
#' @return A list with three elements:
#' * `custom.coef.map`: A named list that maps the original coefficient names
#' to a cleaner version.
#' * `reorder.coef`: An integer vector that indicates how to reorder the
#' coefficients.
#' * `groups`: A named list that groups the coefficients into interpretable
#' groups.
#' @noRd
group_terms <- function(
  fits,
  skip_intercept = FALSE
  ) {

  # We want to treat all as a list
  if (!inherits(fits, "list")) {
    fits <- list(fits)
  }

  replacement <- c(
    "Logit intercept\\s+(.+)" = "\\1",
    "(Motif\\s+)?\\{([^,]+),\\s*([^}]+)\\}" = "{\\2, \\3}",
    "(Motif\\s+)?\\{([^,]+),\\s*([^}]+)\\}(.+)" = "{\\2, \\3}\\4"
  )

  vnames <- lapply(fits, coef) |>
    unname() |>
    unlist() |>
    names() |>
    unique()

  vnames_new <- gsub(
    x = vnames,
    replacement = replacement[1],
    pattern = names(replacement)[1]
  ) |>
    gsub(
      replacement = replacement[2],
      pattern = names(replacement)[2]
    )

  # Identifying the order
  term_patterns <- names(replacement)
  terms_intercept <- which(grepl(pattern = term_patterns[1], x = vnames))
  terms_motifs <- which(grepl(pattern = term_patterns[2], x = vnames))
  terms_transitions <- which(grepl(pattern = term_patterns[3], x = vnames))

  # Removing the motifs from the transitions
  terms_motifs <- setdiff(terms_motifs, terms_transitions)

  # Reorganizing terms in this sort
  terms_order <- c(terms_intercept, terms_motifs, terms_transitions)

  terms_groups <- NULL
  if (length(terms_intercept) > 0) {
    terms_groups <- c(terms_groups, list(
      "Logit intercept" = seq_along(terms_intercept)
    ))
  }

  nterms <- length(unlist(terms_groups))
  if (length(terms_motifs) > 0) {
    terms_groups <- c(terms_groups, list(
      "Co-ocurrence" = seq_along(terms_motifs) + nterms
    ))
  }

  nterms <- length(unlist(terms_groups))
  if (length(terms_transitions) > 0) {
    terms_groups <- c(terms_groups, list(
      "Transition" = seq_along(terms_transitions) + nterms
    ))
  }

  coefmap <- structure(
    vnames_new,
    names = vnames
  ) |> as.list()

  if (skip_intercept) {
    coefmap <- coefmap[!grepl("^Logit intercept nsi_[a-zA-Z_]+$", names(coefmap))]
  }

  list(
    custom.coef.map = coefmap,
    reorder.coef = terms_order,
    groups = terms_groups
  )

}

#' @param fits Either a single or a list of `defm` fit objects.
#' @param fun Function to be called from the `texreg` package, e.g., `texreg::screenreg`.
#' @param ... Further arguments passed to `summary_table` (with the exception of `as_texreg`, which is set to `TRUE`).
#' @param skip_intercept Whether or not to skip the intercept (logit) terms when printing the table
#' @rdname defm_mle
#' @returns
#' An object of class texreg with additional attributes: `custom.coef.map`,
#' `reorder.coef`, and `groups`.
#' @export
texreg_fancy <- function(
  fits,
  fun,
  skip_intercept = FALSE,
  ...
) {

  extra_info <- group_terms(fits, skip_intercept = skip_intercept)

  if (inherits(fits, "mle")) {

    fits <- list(fits)

  } else if (inherits(fits, "list")) {

    if (!all(sapply(fits, inherits(), what = "mle"))) {

      stop("When `fits` is a list, all its elements should be of class 'mle'.")

    }

  }

  do.call(
    fun,
    c(
      list(lapply(fits, summary_table, as_texreg = TRUE, ...)),
      extra_info
      )
  )

}