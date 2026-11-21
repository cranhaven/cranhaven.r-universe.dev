#' Summarizing OPSR TE Objects
#'
#' This function computes weighted treatment effects and corresponding weighted
#' paired t-tests.
#'
#' @param object an object of class `"opsr.te"`.
#' @param ... further arguments passed to or from other methods.
#'
#' @return An object of class `"summary.opsr.te"` containing among others:
#' * `ate`: An object of class `"ate"`.
#' * `te`: An object of class `"te"`.
#'
#' The p-values of the weighted paired t-test are attached as attributes.
#'
#' @method summary opsr.te
#' @export
summary.opsr.te <- function(object, ...) {
  make.dim.names <- function(object) {
    nm <- paste0("T", 1:object$nReg)
    list(nm, nm)
  }

  wtd.paired.t.test <- function(x, y, weights = NULL) {
    diff <- y - x
    fit <- stats::lm(diff ~ 1, weights = weights)
    s <- summary(fit)
    t.test <- s$coefficients[1, c("t value", "Pr(>|t|)")]
    class(t.test) <- c("wtd.paired.t.test", class(t.test))
    t.test
  }

  papply <- function(mat, FUN, ...) {
    cbn <- utils::combn(ncol(mat), 2)
    out <- lapply(1:ncol(cbn), function(i) {
      idx <- cbn[, i]
      x <- mat[, idx[1]]
      y <- mat[, idx[2]]
      res <- FUN(x, y, ...)
      attr(res, "idx.papply") <- idx
      res
    })
    out
  }

  ## te
  ## for each group pairwise diff when switching regime
  te.group <- function(object, group) {
    dat <- object$ce.by.groups[[group]]
    w <- object$weights
    t.test <- papply(dat, wtd.paired.t.test, weights = w)
    te <- apply(dat, 2, function(x) {
      stats::weighted.mean(x, w = w, na.rm = TRUE)
    })
    te <- outer(te, te, "-")
    dimnames(te) <- make.dim.names(object)
    attr(te, "group") <- group
    attr(te, "t.test") <- t.test
    class(te) <- c("te.group", class(te))
    te
  }

  te.mat <- function(object) {
    te <- lapply(seq_len(object$nReg), function(i) {
      te.group(object, group = i)
    })
    class(te) <- c("te.mat", class(te))
    te
  }

  vec.from.mat <- function(x) {
    idx <- which(lower.tri(x), arr.ind = TRUE)
    df <- data.frame(
      from = rownames(x)[idx[, 2]],
      to = rownames(x)[idx[, 1]],
      te = x[idx]
    )
    nm <- paste(df$from, df$to, sep = "->")
    if (is_tobit_5(object)) {
      nm <- ""
    }
    stats::setNames(df$te, nm)
  }

  t.test.from.mat <- function(x) {
    t.test <- attr(x, "t.test")
    df <- as.data.frame(Reduce(rbind, t.test))
    if (is_tobit_5(object)) {
      df <- t(df)
    }
    from_to <- sapply(t.test, function(x) attr(x, "idx.papply"))
    nm <- paste0("T", from_to[1, ], "->T", from_to[2, ])
    stats::setNames(df[, "Pr(>|t|)"], nm)
  }

  te <- function(object) {
    x <- te.mat(object)
    te <- sapply(x, vec.from.mat)
    p.vals <- sapply(x, t.test.from.mat)
    if (is_tobit_5(object)) {
      te <- as.matrix(t(te))
      rownames(te) <- "T"
      p.vals <- as.matrix(t(p.vals))
    }
    cn <- paste0("G", 1:object$nReg)
    colnames(te) <- cn
    colnames(p.vals) <- cn
    attr(te, "p.vals") <- p.vals
    class(te) <- c("te", class(te))
    te
  }

  ## ate = E[Y(1) - Y(0)]
  ate.mat <- function(object) {
    dat <- Reduce(rbind, object$ce.by.groups)
    w <- rep(object$weights, object$nReg)
    t.test <- papply(dat, wtd.paired.t.test, weights = w)
    ate <- apply(dat, 2, function(x) {
      x. <- stats::na.omit(x)
      w. <- w[-attr(x., "na.action")]
      stats::weighted.mean(x., w = w.)
    })
    ate <- outer(ate, ate, "-")
    dimnames(ate) <- make.dim.names(object)
    attr(ate, "t.test") <- t.test
    class(ate) <- c("ate.mat", class(ate))
    ate
  }

  ate <- function(object) {
    x <- ate.mat(object)
    ate <- vec.from.mat(x)
    p.vals <- t.test.from.mat(x)
    attr(ate, "p.vals") <- p.vals
    class(ate) <- c("ate", class(ate))
    ate
  }

  ## opsr.ate
  out <- list()
  out$call <- match.call()
  out$fit <- object$fit
  out$te.mat <- te.mat(object)
  out$te <- te(object)
  out$ate.mat <- ate.mat(object)
  out$ate <- ate(object)
  class(out) <- c("summary.opsr.te", class(out))
  out
}
