get_z <- function(object, data) {
  mf <- if (missing(data)) model.frame(object) else model.frame(object, data = data)
  z_name <- attr(stats::terms(object$formula), "term.labels")[1]
  mf[, z_name]
}

get_y <- function(object, data) {
  mf <- if (missing(data)) model.frame(object) else model.frame(object, data = data)
  y_name <- attr(stats::terms(object$formula), "term.labels")[2]
  mf[, y_name]
}

censor <- function(x, lower, upper) {
  pmin(pmax(x, lower), upper)
}

#' @export
nobs.opsr <- function(object, ...) {
  object$nObs[["Total"]]
}

#' @export
fitted.opsr <- function(object, ...) {
  p <- lapply(seq_len(object$nReg), function(j) {
    ## this is the conditional expectation
    predict(object, group = j, type = "response")
  })
  p_df <- Reduce(cbind, p)
  fitted <- rowSums(p_df, na.rm = TRUE)
  fitted
}

#' @export
residuals.opsr <- function(object, ...) {
  y <- get_y(object)
  y - fitted(object)
}

#' @export
update.opsr <- function(object, ...) {
  NextMethod("update", object)
}

## returns loglik of ordered probit model
ll_probit <- function(object) {
  probs <- lapply(seq_len(object$nReg), function(i) {
    predict(object, group = i, type = "prob")
  })
  probs <- rowSums(Reduce(cbind, probs), na.rm = TRUE)
  sum(log(probs))
}

r2 <- function(object) {
  z <- get_z(object)
  y <- get_y(object)
  RS <- residuals(object)^2
  TS <- (y - mean(y))^2
  R2o <- unlist(lapply(seq_len(object$nReg), function(i) {
    RSS <- sum(RS[z == i])
    yo <- y[z == i]
    TSS <- sum((yo - mean(yo))^2)
    1 - RSS/TSS
  }))
  R2total <- 1 - sum(RS)/sum(TS)
  R2 <- c(R2total, R2o)
  names(R2) <- c("Total", paste0("o", 1:object$nReg))
  R2
}

#' @export
coef.opsr <- function(object, component = c("all", "structural", "selection", "outcome"), ...) {
  component <- match.arg(component)
  all_coefs <- object$estimate
  nm <- names(all_coefs)
  out <-
    switch(component,
           "all" = all_coefs,
           "structural" = all_coefs[grepl("^kappa|^sigma|^rho", nm)],
           "selection" = all_coefs[grepl("^s_", nm)],
           "outcome" = all_coefs[grepl("^o[0-9]_", nm)])

  out
}

is_opsr_null <- function(object) {
  methods::is(object, "opsr.null")
}

## is a recent feature...
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

## creds to stats::printCoefmat
to.signif.codes <- function(pv) {
  Signif <- stats::symnum(pv, corr = FALSE, na = FALSE,
                          cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                          symbols = c("***", "**", "*", ".", " "))
  Signif
}

pleaseCite <- function(pkgname) {
  pc <- utils::capture.output(print(utils::citation(pkgname)))
  pc <- paste(pc, collapse = "\n")
  pc
}

is_tobit_5 <- function(object) {
  methods::is(object, "tobit.5") || object$nReg == 2
}


