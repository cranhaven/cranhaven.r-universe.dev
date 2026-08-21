#' @describeIn englegranger Executes Johansen Test.
#' @export
johansen <- function(formula, data, type = "eigen", lags = 1, trend = "const"){

  # ---- Check Syntax ----

  mf <- match.call()
  m <- match(c("formula", "data"), names(mf), 0L)
  if (is.null(data))
    stop()
  mf <- mf[c(1L, m)]
  mf[[1L]] <- quote(stats::model.frame)
  mf <- eval(mf, parent.frame())
  mt <- attr(mf, "terms")
  mf <- stats::na.omit(mf)
  y <- stats::model.response(mf, "numeric")
  x <- stats::model.matrix(mt, mf)[, -1]
  x <- cbind(y, x)
  trend <- match.arg(trend,
                     choices = c("none", "const", "trend"))
  if (nrow(x) == 0)
    stop("0 (non-NA) cases")
  if (NROW(y) != nrow(x))
    stop("Incompatible dimensions")
  lag <- lags
  if (lag < 0)
    stop("Lags must be set to a non negative value.")


  # ---- Johansen Test ----
  if (identical(trend, "trend"))
    trend = "both"

  jo_vec <- tsDyn::VECM(x, lag = lags,
                        include = trend, # Bezeichnung Trend anpassen
                        estim = "ML")
  jo_vec_sum <- summary(tsDyn::rank.test(jo_vec, type = type))
  test.stat <- as.numeric(jo_vec_sum$eigen[1])

  out <- list(test.stat = test.stat,
              lags = lags,
              trend = trend,
              type = type,
              trace = jo_vec_sum[, 1:4],
              eigen = jo_vec_sum[, c(1, 5:6)],
              test = "Johansen",
              formula = formula)
  return(out)
}
