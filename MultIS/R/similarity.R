#' Generate a similarity matrix
#'
#' @export
#' @param readouts The readouts that are used to generate the similarity matrix
#' @param self Values to set on the diagonal of the matrix. If NULL, the values
#'             that are returned by the method are used.
#' @param upper Only used with "rsquared". If TRUE, generates the upper
#'              triangle.
#' @param method The method to use as a string. Possible values for the string
#'               are "rsquared" and any method that is accepted by stats::dist.
#'               In case of stats::dist we are using the change in the values
#'               over time / compartments (columns).
#' @param strategy Defines the strategy how to treat 0 / NA values. Considering
#'                 a pair (two lines), **atLeastOne** ignores all columns, where
#'                 both are 0. **all** takes all measures into account,
#'                 independent whether they are 0 or not.
#' @param min_measures Minimum number of measures to compare two integration
#'                    sites (rows). If there are less measures, the similarity
#'                    entry is NA.
#' @param post_norm Normalize the similarity matrix to [0,1] scale.
#' @param parallel Whether parallelism should be used. Number of cores is set
#'                 by option mc.cores. If unset, parallel::detectCores is used.
#' @return A similarity matrix.
get_similarity_matrix <- function(readouts,
                                  self = NULL,
                                  upper = TRUE,
                                  method = "rsquared",
                                  strategy = "atLeastOne",
                                  min_measures = 3L,
                                  post_norm = TRUE,
                                  parallel = FALSE) {
  if (!is.matrix(readouts)) {
    stop("readouts need to be a matrix")
  }
  if (ncol(readouts) > 0 && is.null(colnames(readouts))) {
    stop("columns need to have a name")
  }
  if (nrow(readouts) > 0 && is.null(rownames(readouts))) {
    stop("rows need to have a name")
  }

  stopifnot(strategy %in% c("atLeastOne", "all"))
  stopifnot(is.integer(min_measures))
  stopifnot(is.character(method))

  readouts <- readouts[rowSums(readouts, na.rm = TRUE) != 0, , drop = FALSE]
  if (nrow(readouts) == 0) {
    warning("There is no row with reads > 0.")
    m <- matrix(
      0,
      nrow = nrow(readouts),
      ncol = nrow(readouts),
      dimnames = list(NULL, NULL))
    class(m) <- c(class(m), "ISSimilarity")

    return(m)
  }

  m <- parallel::mclapply(seq_len(nrow(readouts)), function(is1) {
    ret <- sapply(1:is1, function(is2) {
      dat <- readouts[c(is1, is2), , drop = FALSE]
      if (strategy == "atLeastOne")
        dat <- dat[, dat[1, ] != 0 | dat[2, ] != 0, drop = FALSE]
      if (ncol(dat) < min_measures) {
        return(NA)
      }

      if (method == "rsquared") {
        fit <- stats::lm(y ~ 0 + x,
                         data = data.frame(x = dat[1, ], y = dat[2, ]))
        return(ifelse(is.na(fit$coefficients[["x"]]),
                      NA,
                      suppressWarnings(summary(fit)$r.squared)))
      } else {
        return(stats::dist(x = dat / apply(dat, MARGIN = 1, max),
                           method = method))
      }
    })
    return(c(ret, rep(NA, nrow(readouts) - length(ret))))
  }, mc.cores = ifelse(parallel,
                       getOption("mc.cores", parallel::detectCores()),
                       1))

  m <- do.call("rbind", m)
  if (upper) {
    m[upper.tri(m, diag = FALSE)] <- t(m)[upper.tri(m, diag = FALSE)]
  }
  dimnames(m) <- list(rownames(readouts), rownames(readouts))

  if (length(stats::na.omit(diag(m))) != 0 &&
      unique(stats::na.omit(diag(m))) == 0) {
    m <- 1 - m
  }

  if (post_norm && !all(is.na(m))) {
    m <- m - min(m, na.rm = TRUE)
    m <- m / max(m, na.rm = TRUE)
  }

  if (!is.null(self)) {
    diag(m) <- self
  }

  class(m) <- c(class(m), "ISSimilarity")

  return(m)
}
