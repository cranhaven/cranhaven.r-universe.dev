#' Error-correction screening with MARS forecasts under rolling-origin validation
#'
#' Evaluates every directed pair between a set of circulation variables and a set of production
#' variables, in both directions, and measures how often a non-linear error-correction forecast
#' is both statistically admissible and predictive across successive temporal windows. Each
#' window first screens the pair for integration, cointegration and a significant
#' error-correction term; only windows that pass that screen fit a Multivariate Adaptive
#' Regression Splines (MARS) forecaster. Use it to rank directed relationships by the temporal
#' stability of their error-correction structure rather than by a single full-sample fit.
#'
#' @param data_path Character scalar. Path to an Excel file with exactly 14 columns in this
#'   order: a date column followed by the 13 series `ER.SPOT.CAN.US`, `ER.SPOT.US.CAN`,
#'   `ER.SPOT.US.REMB`, `CPI`, `TreasuryBonds10y`, `FedDiscountRate`, `Exports`,
#'   `RealNetProfit`, `RealSocialConsumptionPerWorker2017`, `RealWagePPP2017`,
#'   `CapitalStockPPP2017`, `LaborProductivityPPP2017`, `InvestmentPerWorkerPPP2017`. The
#'   columns are renamed to these names by position, whatever their names in the file.
#' @param circ_vars Character vector of circulation variable names, a subset of the 13 names
#'   above. Names not present are dropped.
#' @param prod_vars Character vector of production variable names, a subset of the 13 names
#'   above. Names not present are dropped.
#' @param cointeg_rule Character scalar, `"either"` (default) or `"both"`: whether
#'   cointegration requires the Engle-Granger/Phillips-Ouliaris screen or the Johansen screen,
#'   or both of them.
#' @param eg_p_cutoff Numeric scalar in (0, 1), default `0.05`. Significance level of the
#'   residual unit-root test and of the Phillips-Ouliaris test.
#' @param ecm_p_cutoff Numeric scalar in (0, 1), default `0.05`. One-sided significance level
#'   for the error-correction coefficient being negative.
#' @param lag_max_ecm Integer scalar >= 1, default `4`. Largest lag order of the differenced
#'   series considered in the linear error-correction model.
#' @param min_tr Integer scalar >= 1, default `20`. Minimum number of complete training rows
#'   required to fit MARS in a window.
#' @param min_te Integer scalar >= 1, default `8`. Minimum number of complete test rows required
#'   to evaluate a window.
#' @param rolling_cv_enable Logical scalar, default `TRUE`. If `FALSE`, a single split with the
#'   first 75 percent of observations for training is used instead of rolling windows.
#' @param rolling_cv_window Character scalar, `"sliding"` (default) or `"expanding"`. Training
#'   window type of the outer validation.
#' @param rolling_cv_initial_frac Numeric scalar in (0, 1), default `0.8`. Fraction of the
#'   sample in the first training window.
#' @param rolling_cv_initial_min Integer scalar >= 1, default `40`. Minimum size of the first
#'   training window.
#' @param rolling_cv_test Integer scalar >= 1, default `12`. Test horizon of each outer window.
#' @param rolling_cv_step Integer scalar >= 1, default `12`. Advance between outer windows.
#' @param nested_tune Logical scalar, default `TRUE`. If `TRUE`, MARS degree and number of terms
#'   are chosen in each outer training window by an inner validation; if `FALSE`, degree 2 and
#'   25 terms are used.
#' @param nested_initial_f Numeric scalar in (0, 1), default `0.6`. Fraction of the outer
#'   training window in the first inner training window (at least 30 rows).
#' @param nested_test Integer scalar >= 1, default `6`. Test horizon of each inner window.
#' @param nested_step Integer scalar >= 1, default `3`. Advance between inner windows.
#' @param mars_grid Data frame with numeric columns `degree` and `nk`, default
#'   `expand.grid(degree = c(1, 2), nk = c(15, 25, 35, 50, 65))`. Candidate MARS settings.
#' @param support_min Numeric scalar in (0, 1], default `0.75`. Minimum proportion of outer
#'   windows that must pass the screen for `pass_support`.
#' @param folds_min_abs Integer scalar >= 0, default `5`. Minimum absolute number of outer
#'   windows that must pass the screen for `pass_support`.
#' @param parallel_enable Logical scalar, default `TRUE`. Evaluate directions in parallel R
#'   sessions.
#' @param parallel_workers Integer scalar >= 1, default `max(1, parallel::detectCores() - 1)`.
#'   Number of parallel sessions when `parallel_enable = TRUE`.
#'
#' @return A `data.frame` with one row per direction and columns `pair` (`"X -> Y"`), `Y`, `X`,
#'   `folds` (outer windows), `folds_proceed` (windows that passed the screen), `RMSE`, `MAE`,
#'   `MAPE`, `sMAPE`, `R2`, `TheilU`, `bias_prop`, `var_prop`, `cov_prop` (forecast metrics
#'   averaged over the windows that passed), `support` (`folds_proceed / folds`),
#'   `pass_support` (logical), `R2_stab` (`R2 * support`) and `U_stab` (`TheilU / support`),
#'   sorted by decreasing `R2` and then by `pair`.
#'
#' @details
#' In every training window the response `Y` and the predictor `X` must both be integrated of
#' order one: augmented Dickey-Fuller tests with lag order chosen by AIC fail to reject a unit
#' root in levels (with drift or with trend, 10 percent) and reject it in first differences.
#' The VAR lag order is chosen by the Schwarz criterion. Cointegration is assessed with the
#' Johansen trace test (constant, then trend, 5 percent) and with the Engle-Granger residual
#' test or the Phillips-Ouliaris test at `eg_p_cutoff`, combined by `cointeg_rule`. A linear
#' error-correction model is then fitted with the lag order that minimises BIC, preferring the
#' first order whose residuals pass a Ljung-Box test at lag 12, and the error-correction
#' coefficient must be negative with a one-sided p-value below `ecm_p_cutoff` using Newey-West
#' standard errors. The screen is deliberately strict because MARS is flexible enough to fit
#' spurious non-linear structure between non-cointegrated series.
#'
#' Windows that pass fit MARS to the first difference of `Y` on the lagged error-correction
#' term, the first difference of `X`, the first lags of both differences and the second lag of
#' the difference of `Y`, and forecast the level as the
#' previous level plus the forecast difference. With `nested_tune = TRUE`, the MARS setting with
#' the lowest mean RMSE across inner expanding windows is used; the inner validation never sees
#' the outer test window.
#'
#' `pass_support` requires `folds_proceed >= max(folds_min_abs, ceiling(support_min * folds))`,
#' so both a relative and an absolute number of admissible windows are needed.
#'
#' @section Methodological notes:
#' `support` measures temporal stability of the screen, not forecast accuracy: a direction can
#' forecast well in the few windows where it passes and still have low support. `R2` and the
#' other metrics are averaged only over windows that passed, so they must be read together with
#' `folds_proceed`. The error-correction term uses the Engle-Granger long-run coefficients
#' estimated in the training window, so no test-window information enters the forecaster.
#' Parallel evaluation uses reproducible random streams (`future.seed = TRUE`). The function
#' restores the caller's `future` plan and the BLAS/OpenMP thread settings of the calling R
#' session on exit, also when it stops with an error. The thread limit applies to the calling
#' session only: the parallel worker sessions are separate R processes and keep their own BLAS
#' and OpenMP settings. A thread count that cannot be read (for example OpenMP when
#' `RhpcBLASctl` was built without it) is left untouched.
#'
#' @section Dependencies:
#' `readxl` reads the data; `dplyr` orders observations and builds lags; `urca` provides the
#' augmented Dickey-Fuller and Johansen tests; `vars` selects the VAR lag order; `tseries`
#' provides the Phillips-Ouliaris test; `lmtest` and `sandwich` give the Newey-West test of the
#' error-correction coefficient; `earth` fits MARS; `future` and `future.apply` evaluate
#' directions in parallel; `progressr` reports progress; `RhpcBLASctl`, if installed, keeps
#' BLAS and OpenMP single-threaded in the calling R session during the evaluation.
#'
#' @references
#' Engle, R. F., & Granger, C. W. J. (1987). Co-integration and error correction:
#' Representation, estimation, and testing. *Econometrica, 55*(2), 251\enc{–}{-}276.
#' \doi{10.2307/1913236}
#'
#' Friedman, J. H. (1991). Multivariate adaptive regression splines. *The Annals of
#' Statistics, 19*(1), 1\enc{–}{-}67. \doi{10.1214/aos/1176347963}
#'
#' Johansen, S. (1988). Statistical analysis of cointegration vectors. *Journal of Economic
#' Dynamics and Control, 12*(2\enc{–}{-}3), 231\enc{–}{-}254. \doi{10.1016/0165-1889(88)90041-3}
#'
#' Newey, W. K., & West, K. D. (1987). A simple, positive semi-definite, heteroskedasticity and
#' autocorrelation consistent covariance matrix. *Econometrica, 55*(3), 703\enc{–}{-}708.
#' \doi{10.2307/1913610}
#'
#' Phillips, P. C. B., & Ouliaris, S. (1990). Asymptotic properties of residual based tests for
#' cointegration. *Econometrica, 58*(1), 165\enc{–}{-}193. \doi{10.2307/2938339}
#'
#' @seealso [bsts_model()], [bglmar1()]; the vignettes `ecm-mars-eng` and `ecm-mars-esp`.
#'
#' @examples
#' \dontrun{
#' result <- ecm_mars(
#'   data_path = file.path(tempdir(), "data.xlsx"),
#'   circ_vars = c("ER.SPOT.CAN.US", "ER.SPOT.US.CAN", "ER.SPOT.US.REMB",
#'                 "CPI", "TreasuryBonds10y", "FedDiscountRate"),
#'   prod_vars = c("Exports", "RealNetProfit", "RealSocialConsumptionPerWorker2017",
#'                 "RealWagePPP2017", "CapitalStockPPP2017",
#'                 "LaborProductivityPPP2017", "InvestmentPerWorkerPPP2017")
#' )
#' }
#'
#' @export
ecm_mars <- function(data_path, circ_vars, prod_vars, cointeg_rule = "either", 
                     eg_p_cutoff = 0.05, ecm_p_cutoff = 0.05, lag_max_ecm = 4,
                     min_tr = 20, min_te = 8, rolling_cv_enable = TRUE,
                     rolling_cv_window = "sliding", rolling_cv_initial_frac = 0.8,
                     rolling_cv_initial_min = 40, rolling_cv_test = 12, rolling_cv_step = 12,
                     nested_tune = TRUE, nested_initial_f = 0.6, nested_test = 6, nested_step = 3,
                     mars_grid = expand.grid(degree = c(1, 2), nk = c(15, 25, 35, 50, 65)),
                     support_min = 0.75, folds_min_abs = 5, parallel_enable = TRUE,
                     parallel_workers = max(1, parallel::detectCores() - 1)) {
  
  oplan <- if (parallel_enable) {
    future::plan(future::multisession, workers = parallel_workers)
  } else {
    future::plan(future::sequential)
  }
  on.exit(future::plan(oplan), add = TRUE)

  restore_threads <- limit_threads()
  on.exit(restore_threads(), add = TRUE)

  raw <- readxl::read_excel(data_path)

  workbook_names <- c(
    "Month",
    "ER.SPOT.CAN.US", "ER.SPOT.US.CAN", "ER.SPOT.US.REMB",
    "CPI", "TreasuryBonds10y", "FedDiscountRate",
    "Exports", "RealNetProfit", "RealSocialConsumptionPerWorker2017",
    "RealWagePPP2017", "CapitalStockPPP2017",
    "LaborProductivityPPP2017", "InvestmentPerWorkerPPP2017"
  )
  check_workbook_width(raw, workbook_names)
  colnames(raw) <- workbook_names

  circ_vars <- intersect(circ_vars, setdiff(colnames(raw), "Month"))
  prod_vars <- intersect(prod_vars, setdiff(colnames(raw), "Month"))
  
  df <- raw %>% dplyr::arrange(.data$Month)
  vars_all <- unique(c(circ_vars, prod_vars))
  df <- df[, c("Month", vars_all)]
  n_total <- nrow(df)
  idx_tr <- 1:floor(0.75 * n_total)
  idx_te <- (max(idx_tr) + 1):n_total
  
  ur_reject_unitroot <- function(x, type = c("none", "drift", "trend"), level = 0.05) {
    type <- match.arg(type)
    x <- as.numeric(stats::na.omit(x))
    if (length(x) < 15) return(NA)
    obj <- tryCatch(urca::ur.df(x, type = type, selectlags = "AIC"), error = function(e) NULL)
    if (is.null(obj)) return(NA)
    stat <- tryCatch(obj@teststat[1], error = function(e) NA_real_)
    cns  <- colnames(obj@cval)
    cn_num <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", cns)))
    idx <- which.min(abs(cn_num - (level * 100)))
    crit <- tryCatch(obj@cval[1, idx], error = function(e) NA_real_)
    if (!is.finite(stat) || !is.finite(crit)) return(NA)
    stat < crit
  }
  
  is_I1 <- function(x) {
    x <- as.numeric(x)
    rej_lvl_drift <- ur_reject_unitroot(x, type = "drift", level = 0.10)
    rej_lvl_trend <- ur_reject_unitroot(x, type = "trend", level = 0.10)
    lvl_nonstat   <- (isFALSE(rej_lvl_drift) || isFALSE(rej_lvl_trend))
    rej_d1_none   <- ur_reject_unitroot(diff(x), type = "none", level = 0.10)
    d1_stat       <- isTRUE(rej_d1_none)
    isTRUE(lvl_nonstat) && isTRUE(d1_stat)
  }
  
  select_K <- function(y, x, max_lag = 8L) {
    Z <- cbind(Y = as.numeric(y), X = as.numeric(x))
    Z <- Z[stats::complete.cases(Z), , drop = FALSE]
    if (nrow(Z) < (max_lag + 10)) max_lag <- max(2, floor(nrow(Z) / 8))
    sel <- suppressWarnings(vars::VARselect(Z, lag.max = max_lag, type = "const"))
    if (!is.null(sel$selection["SC(n)"])) return(as.integer(sel$selection["SC(n)"]))
    as.integer(sel$selection["AIC(n)"])
  }
  
  johansen_cointegration <- function(y, x, K, ecdets = c("const", "trend")) {
    K <- max(2L, K)
    df <- data.frame(y = as.numeric(y), x = as.numeric(x))
    df <- df[stats::complete.cases(df), , drop = FALSE]
    if (nrow(df) < (K + 5)) return(list(cointeg = FALSE, stat = NA_real_, crit5 = NA_real_, ecdet = NA_character_))
    pick <- list(cointeg = FALSE, stat = NA_real_, crit5 = NA_real_, ecdet = NA_character_)
    for (e in ecdets) {
      jo <- tryCatch(urca::ca.jo(df, type = "trace", K = K, ecdet = e, spec = "longrun"), error = function(z) NULL)
      if (is.null(jo)) next
      stat <- tryCatch(jo@teststat[1], error = function(z) NA_real_)
      cns  <- colnames(jo@cval)
      cn_num <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", cns)))
      idx5 <- tryCatch(which.min(abs(cn_num - 5)), error = function(z) integer(0))
      if (length(idx5) == 0 || is.na(idx5)) idx5 <- if ("5pct" %in% cns) which(cns == "5pct") else which(cns == "5%")
      crit5 <- tryCatch(jo@cval[1, idx5, drop = TRUE], error = function(z) NA_real_)
      if (is.finite(stat) && is.finite(crit5) && stat > crit5) {
        pick <- list(cointeg = TRUE, stat = stat, crit5 = crit5, ecdet = e)
        break
      }
    }
    pick
  }
  
  engle_granger <- function(y, x) {
    df <- data.frame(y = as.numeric(y), x = as.numeric(x))
    df <- df[stats::complete.cases(df), , drop = FALSE]
    if (nrow(df) < 20) return(list(p = NA, cointeg = FALSE, alpha = NA, beta = NA, res = NULL))
    reg <- stats::lm(y ~ x, data = df)
    res <- stats::residuals(reg)
    rej_adf <- ur_reject_unitroot(res, type = "none", level = eg_p_cutoff)
    eg_ok   <- isTRUE(rej_adf)
    po_p <- suppressWarnings(tryCatch(tseries::po.test(df)$p.value, error = function(e) NA_real_))
    po_ok <- (!is.na(po_p) && po_p < eg_p_cutoff)
    list(
      p = if (!is.na(po_p)) po_p else NA_real_,
      cointeg = (eg_ok || po_ok),
      alpha = unname(stats::coef(reg)[1]),
      beta  = unname(stats::coef(reg)[2]),
      res   = res
    )
  }
  
  .build_ecm_df <- function(y, x, alpha, beta, L_max) {
    y <- as.numeric(y); x <- as.numeric(x)
    dY <- c(NA, diff(y)); dX <- c(NA, diff(x))
    yL1 <- dplyr::lag(y); xL1 <- dplyr::lag(x)
    ECM1 <- yL1 - alpha - beta * xL1
    df_ecm <- data.frame(dY = dY, dX = dX, ECM1 = ECM1)
    for (i in 1:L_max) {
      df_ecm[[paste0("dY_lag", i)]] <- dplyr::lag(df_ecm$dY, i)
      df_ecm[[paste0("dX_lag", i)]] <- dplyr::lag(df_ecm$dX, i)
    }
    df_ecm
  }

  .ecm_formula_for_L <- function(L) {
    rhs <- c("ECM1",
             if (L >= 1) paste0("dY_lag", 1:L) else character(0),
             if (L >= 1) paste0("dX_lag", 1:L) else character(0))
    stats::as.formula(paste("dY ~", paste(rhs, collapse = " + ")))
  }

  choose_ecm_L <- function(df_full, L_max = 4, lb_lag = 12, ic = c("BIC", "AIC")) {
    ic <- match.arg(ic); cand <- list()
    for (L in 1:L_max) {
      keep <- c("dY", "ECM1", paste0("dY_lag", 1:L), paste0("dX_lag", 1:L))
      d_L <- df_full[, keep]; d_L <- d_L[stats::complete.cases(d_L), , drop = FALSE]
      if (nrow(d_L) < 20) next
      fit <- stats::lm(.ecm_formula_for_L(L), data = d_L)
      ic_val <- if (ic == "BIC") stats::BIC(fit) else stats::AIC(fit)
      lb_p <- suppressWarnings(tryCatch(
        stats::Box.test(stats::residuals(fit), lag = lb_lag, type = "Ljung-Box")$p.value,
        error = function(e) NA_real_
      ))
      cand[[length(cand) + 1L]] <- list(L = L, fit = fit, ic = ic_val, lb_p = lb_p, n = nrow(d_L))
    }
    if (length(cand) == 0L) return(NULL)
    ord <- order(vapply(cand, `[[`, numeric(1), "ic"),
                 vapply(cand, `[[`, numeric(1), "L"))
    cand <- cand[ord]; best <- cand[[1L]]
    if (!is.na(best$lb_p) && best$lb_p <= 0.05) {
      for (cc in cand) {
        if (!is.na(cc$lb_p) && cc$lb_p > 0.05) { best <- cc; break }
      }
    }
    best
  }

  ecm_linear_significance <- function(y, x, alpha, beta,
                                      L_max = lag_max_ecm, K_sel = NULL, lb_lag = 12,
                                      use_HAC = TRUE, p_cut = ecm_p_cutoff) {
    if (any(!is.finite(c(alpha, beta)))) return(list(p_alpha = NA_real_, signif = FALSE, L = NA_integer_))
    df_full <- .build_ecm_df(y, x, alpha, beta, L_max = L_max)
    best <- choose_ecm_L(df_full[, c("dY", "ECM1", paste0("dY_lag", 1:L_max), paste0("dX_lag", 1:L_max))],
                         L_max = L_max, lb_lag = lb_lag, ic = "BIC")
    if (is.null(best)) return(list(p_alpha = NA_real_, signif = FALSE, L = NA_integer_))
    fit <- best$fit; L <- best$L
    if (use_HAC) {
      ct  <- lmtest::coeftest(fit, vcov. = sandwich::NeweyWest(fit, prewhite = FALSE, adjust = TRUE))
      est <- as.numeric(ct["ECM1", "Estimate"])
      se  <- as.numeric(ct["ECM1", "Std. Error"])
    } else {
      sm  <- summary(fit)$coefficients
      est <- as.numeric(sm["ECM1", "Estimate"])
      se  <- as.numeric(sm["ECM1", "Std. Error"])
    }
    tval <- est / se
    p_one <- tryCatch(stats::pt(tval, df = fit$df.residual), error = function(e) NA_real_)
    sig   <- (!is.na(p_one) && p_one < p_cut && est < 0)
    list(p_alpha = p_one, signif = sig, L = L, lb_p = best$lb_p, n = best$n, lambda = est, t = tval)
  }

  safe_mape <- function(pred, obs) {
    ok <- is.finite(pred) & is.finite(obs) & (abs(obs) > .Machine$double.eps)
    if (!any(ok)) return(NA_real_)
    mean(abs((pred[ok] - obs[ok]) / obs[ok])) * 100
  }

  smape <- function(pred, obs) {
    ok <- is.finite(pred) & is.finite(obs) & (abs(pred) + abs(obs) > .Machine$double.eps)
    if (!any(ok)) return(NA_real_)
    mean(2 * abs(pred[ok] - obs[ok]) / (abs(pred[ok]) + abs(obs[ok]))) * 100
  }

  theil_u <- function(pred, obs) {
    ok <- is.finite(pred) & is.finite(obs)
    if (!any(ok)) return(NA_real_)
    sqrt(mean((pred[ok] - obs[ok])^2)) / sqrt(mean(obs[ok]^2))
  }

  build_pair_frame <- function(y, x, alpha, beta) {
    y <- as.numeric(y); x <- as.numeric(x)
    dY <- c(NA, diff(y)); dX <- c(NA, diff(x))
    yLag1 <- dplyr::lag(y); XLag1 <- dplyr::lag(x)
    ECM1 <- (yLag1 - alpha - beta * XLag1)
    data.frame(Y = y, X = x, dY = dY, dX = dX, yLag1 = yLag1, XLag1 = XLag1, ECM1 = ECM1)
  }

  make_rolling_splits <- function(n, initial, test, step, window = c("expanding", "sliding")) {
    window <- match.arg(window)
    splits <- list()
    start_train <- 1L; end_train <- initial
    while ((end_train + 1) <= n - 1) {
      start_test <- end_train + 1L
      end_test   <- min(end_train + test, n)
      if (start_test > end_test) break
      if (window == "expanding") {
        tr <- start_train:end_train
      } else {
        sz  <- end_train - start_train + 1L
        win <- min(sz, initial)
        tr  <- (end_train - win + 1L):end_train
      }
      te <- start_test:end_test
      splits[[length(splits) + 1L]] <- list(train = tr, test = te)
      end_train <- min(end_train + step, n - 1L)
    }
    splits
  }

  prepare_frames <- function(y_tr, x_tr, y_te, x_te, alpha, beta) {
    tr <- build_pair_frame(y_tr, x_tr, alpha, beta)
    te <- build_pair_frame(y_te, x_te, alpha, beta)
    tr <- tr[stats::complete.cases(tr[, c("dY", "dX", "yLag1", "XLag1", "ECM1")]), , drop = FALSE]
    te <- te[stats::complete.cases(te[, c("dY", "dX", "yLag1", "XLag1", "ECM1")]), , drop = FALSE]
    if (nrow(tr) == 0L || nrow(te) == 0L) return(NULL)
    tr$dY_lag1 <- dplyr::lag(tr$dY, 1); tr$dY_lag2 <- dplyr::lag(tr$dY, 2)
    tr$dX_lag1 <- dplyr::lag(tr$dX, 1); tr <- tr[stats::complete.cases(tr), , drop = FALSE]
    te$dY_lag1 <- dplyr::lag(te$dY, 1); te$dY_lag2 <- dplyr::lag(te$dY, 2)
    te$dX_lag1 <- dplyr::lag(te$dX, 1); te <- te[stats::complete.cases(te), , drop = FALSE]
    if (nrow(tr) < min_tr || nrow(te) < min_te) return(NULL)
    list(tr = tr, te = te)
  }

  compute_metrics <- function(Y_hat, Y_obs) {
    err <- Y_hat - Y_obs
    mse <- mean(err^2); rmse <- sqrt(mse); mae <- mean(abs(err))
    mape <- safe_mape(Y_hat, Y_obs); smp <- smape(Y_hat, Y_obs)
    SST <- sum((Y_obs - mean(Y_obs))^2)
    r2  <- if (is.finite(SST) && SST > .Machine$double.eps) 1 - sum(err^2) / SST else NA_real_
    u   <- theil_u(Y_hat, Y_obs)
    if (is.na(mse) || mse <= .Machine$double.eps) {
      bias_prop <- NA_real_; var_prop <- NA_real_; cov_prop <- NA_real_
    } else {
      bias_prop <- (mean(Y_hat) - mean(Y_obs))^2 / mse
      var_prop  <- (stats::sd(Y_hat) - stats::sd(Y_obs))^2 / mse
      cov_prop  <- 1 - bias_prop - var_prop
    }
    c(RMSE = rmse, MAE = mae, MAPE = mape, sMAPE = smp, R2 = r2, TheilU = u,
      bias_prop = bias_prop, var_prop = var_prop, cov_prop = cov_prop)
  }

  evaluate_fold_once <- function(y, x, tr_idx, te_idx, mars_degree = 2, mars_nk = 25) {
    y_tr <- y[tr_idx]; x_tr <- x[tr_idx]
    y_te <- y[te_idx]; x_te <- x[te_idx]
    I1_y <- is_I1(y_tr); I1_x <- is_I1(x_tr)
    K    <- select_K(y_tr, x_tr)
    jo   <- johansen_cointegration(y_tr, x_tr, K = K, ecdets = c("const", "trend"))
    eg   <- engle_granger(y_tr, x_tr)
    ecm  <- ecm_linear_significance(y_tr, x_tr,
                                    alpha = eg$alpha, beta = eg$beta,
                                    L_max = lag_max_ecm, K_sel = K,
                                    lb_lag = 12, use_HAC = TRUE, p_cut = ecm_p_cutoff)
    cointeg_pass <- switch(cointeg_rule,
                           "both"   = (isTRUE(eg$cointeg) && isTRUE(jo$cointeg)),
                           "either" = (isTRUE(eg$cointeg) || isTRUE(jo$cointeg)))
    proceed <- (I1_y && I1_x && cointeg_pass && isTRUE(ecm$signif))
    if (!proceed) {
      return(list(proceed = FALSE,
                  diag = c(I1_Y = I1_y, I1_X = I1_x, EG = eg$cointeg, JO = jo$cointeg, ECMsig = ecm$signif),
                  metrics = rep(NA_real_, 9)))
    }
    frames <- prepare_frames(y_tr, x_tr, y_te, x_te, alpha = eg$alpha, beta = eg$beta)
    if (is.null(frames)) {
      return(list(proceed = FALSE,
                  diag = c(I1_Y = I1_y, I1_X = I1_x, EG = eg$cointeg, JO = jo$cointeg, ECMsig = ecm$signif),
                  metrics = rep(NA_real_, 9)))
    }
    tr_frame <- frames$tr; te_frame <- frames$te
    mars_fit <- earth::earth(dY ~ ECM1 + dX + dY_lag1 + dX_lag1 + dY_lag2,
                             data = tr_frame, degree = mars_degree, nk = mars_nk, trace = 0)
    dY_hat <- as.numeric(stats::predict(mars_fit, newdata = te_frame))
    Y_hat  <- te_frame$yLag1 + dY_hat
    Y_obs  <- te_frame$Y
    list(proceed = TRUE,
         diag = c(I1_Y = I1_y, I1_X = I1_x, EG = eg$cointeg, JO = jo$cointeg, ECMsig = ecm$signif),
         metrics = compute_metrics(Y_hat, Y_obs))
  }

  tune_mars_inner <- function(y, x, tr_idx_outer) {
    n_tr <- length(tr_idx_outer)
    initial <- max(30, floor(nested_initial_f * n_tr))
    splits_in <- make_rolling_splits(n_tr, initial, nested_test, nested_step, window = "expanding")
    if (length(splits_in) == 0L) return(list(degree = 2, nk = 25))
    best <- NULL; best_rmse <- Inf
    for (k in seq_len(nrow(mars_grid))) {
      deg <- mars_grid$degree[k]; nk <- mars_grid$nk[k]
      rmses <- c()
      for (sp in splits_in) {
        tr_abs <- tr_idx_outer[sp$train]; te_abs <- tr_idx_outer[sp$test]
        res <- evaluate_fold_once(y, x, tr_abs, te_abs, mars_degree = deg, mars_nk = nk)
        if (isTRUE(res$proceed) && is.finite(res$metrics["RMSE"])) rmses <- c(rmses, res$metrics["RMSE"])
      }
      if (length(rmses) > 0L) {
        mrmse <- mean(rmses)
        if (mrmse < best_rmse) { best_rmse <- mrmse; best <- list(degree = deg, nk = nk) }
      }
    }
    if (is.null(best)) best <- list(degree = 2, nk = 25)
    best
  }

  metric_names <- c("RMSE", "MAE", "MAPE", "sMAPE", "R2", "TheilU", "bias_prop", "var_prop", "cov_prop")

  evaluate_direction_cv <- function(Y_name, X_name) {
    y <- df[[Y_name]]; x <- df[[X_name]]; n <- length(y)
    if (!rolling_cv_enable) {
      tr_idx <- idx_tr; te_idx <- idx_te
      if (nested_tune) {
        best <- tune_mars_inner(y, x, tr_idx)
        res  <- evaluate_fold_once(y, x, tr_idx, te_idx, mars_degree = best$degree, mars_nk = best$nk)
      } else {
        res  <- evaluate_fold_once(y, x, tr_idx, te_idx, mars_degree = 2, mars_nk = 25)
      }
      met <- as.numeric(res$metrics); names(met) <- metric_names
      return(data.frame(pair = paste0(X_name, " -> ", Y_name),
                        Y = Y_name, X = X_name,
                        folds = 1L, folds_proceed = as.integer(res$proceed),
                        RMSE = met["RMSE"], MAE = met["MAE"], MAPE = met["MAPE"], sMAPE = met["sMAPE"],
                        R2 = met["R2"], TheilU = met["TheilU"], bias_prop = met["bias_prop"],
                        var_prop = met["var_prop"], cov_prop = met["cov_prop"],
                        stringsAsFactors = FALSE))
    }
    initial <- max(rolling_cv_initial_min, floor(rolling_cv_initial_frac * n))
    splits <- make_rolling_splits(n, initial, rolling_cv_test, rolling_cv_step, window = rolling_cv_window)
    if (length(splits) == 0L) {
      return(data.frame(pair = paste0(X_name, " -> ", Y_name), Y = Y_name, X = X_name,
                        folds = 0, folds_proceed = 0,
                        RMSE = NA, MAE = NA, MAPE = NA, sMAPE = NA, R2 = NA, TheilU = NA,
                        bias_prop = NA, var_prop = NA, cov_prop = NA, stringsAsFactors = FALSE))
    }
    metrics_mat <- NULL; proceed_vec <- logical(0)
    for (sp in splits) {
      tr_idx <- sp$train; te_idx <- sp$test
      if (nested_tune) {
        best <- tune_mars_inner(y, x, tr_idx)
        res  <- evaluate_fold_once(y, x, tr_idx, te_idx, mars_degree = best$degree, mars_nk = best$nk)
      } else {
        res  <- evaluate_fold_once(y, x, tr_idx, te_idx, mars_degree = 2, mars_nk = 25)
      }
      proceed_vec <- c(proceed_vec, isTRUE(res$proceed))
      metrics_mat <- rbind(metrics_mat, as.numeric(res$metrics))
    }
    colnames(metrics_mat) <- metric_names
    avg <- apply(metrics_mat, 2, function(v) if (all(is.na(v))) NA_real_ else mean(v, na.rm = TRUE))
    data.frame(pair = paste0(X_name, " -> ", Y_name),
               Y = Y_name, X = X_name,
               folds = length(splits),
               folds_proceed = sum(proceed_vec, na.rm = TRUE),
               RMSE = avg["RMSE"], MAE = avg["MAE"], MAPE = avg["MAPE"], sMAPE = avg["sMAPE"],
               R2 = avg["R2"], TheilU = avg["TheilU"], bias_prop = avg["bias_prop"],
               var_prop = avg["var_prop"], cov_prop = avg["cov_prop"],
               stringsAsFactors = FALSE)
  }
  
  tasks <- rbind(
    data.frame(Y = rep(prod_vars, each = length(circ_vars)), X = rep(circ_vars, times = length(prod_vars))),
    data.frame(Y = rep(circ_vars, each = length(prod_vars)), X = rep(prod_vars, times = length(circ_vars)))
  )
  
  results <- NULL
  progressr::with_progress({
    p <- progressr::progressor(steps = nrow(tasks))
    results <- future.apply::future_lapply(seq_len(nrow(tasks)), function(i) {
      p(sprintf("Evaluating: %s -> %s", tasks$X[i], tasks$Y[i]))
      evaluate_direction_cv(Y_name = tasks$Y[i], X_name = tasks$X[i])
    }, future.seed = TRUE)
  }, handlers = progressr::handler_progress())
  
  bench <- dplyr::bind_rows(results) %>%
    dplyr::mutate(
      support      = .data$folds_proceed / pmax(.data$folds, 1),
      pass_support = .data$folds_proceed >= pmax(folds_min_abs, ceiling(support_min * .data$folds)),
      R2_stab      = .data$R2 * .data$support,
      U_stab       = .data$TheilU / pmax(.data$support, .Machine$double.eps)
    ) %>%
    dplyr::arrange(dplyr::desc(.data$R2), .data$pair)
  
  return(bench)
}

# Stops unless the workbook read by ecm_mars() has exactly one column per name the function
# assigns. Assigning more names than a tibble has columns keeps the first ones without an
# error, so a workbook of the wrong width would otherwise run to the end and return a table
# shaped like a valid result with every metric missing.
check_workbook_width <- function(raw, workbook_names) {
  if (ncol(raw) != length(workbook_names)) {
    stop(sprintf("`data_path` must have exactly %d columns, as documented in ?ecm_mars; it has %d.",
                 length(workbook_names), ncol(raw)),
         call. = FALSE)
  }
  invisible(TRUE)
}

# Returns the BLAS and OpenMP thread controls as a list of four functions, or NULL when
# RhpcBLASctl is not installed. It is the only place that touches RhpcBLASctl, so the tests
# replace this binding instead of altering the namespace of another package.
thread_controller <- function() {
  if (!requireNamespace("RhpcBLASctl", quietly = TRUE)) {
    return(NULL)
  }
  list(
    get_blas = RhpcBLASctl::blas_get_num_procs,
    set_blas = RhpcBLASctl::blas_set_num_threads,
    get_omp  = RhpcBLASctl::omp_get_max_threads,
    set_omp  = RhpcBLASctl::omp_set_num_threads
  )
}

# TRUE for a single positive whole number, the only kind of thread count that can be restored.
is_thread_count <- function(x) {
  is.numeric(x) && length(x) == 1L && !is.na(x) && x >= 1 && x == round(x)
}

# Sets BLAS and OpenMP to one thread in the calling R session and returns a function that
# restores the previous values. A channel whose previous value cannot be read as a thread count
# (RhpcBLASctl reports NA for OpenMP when it was built without it) is neither changed nor
# restored. The restorer of a channel is recorded before the channel is changed, so an error
# while limiting restores what was already changed, and each channel is restored on its own,
# so a failure in one does not prevent the other. Worker sessions started by `future` are
# separate processes and keep their own settings.
limit_threads <- function(controller = thread_controller()) {
  restorers <- list()
  restore <- function() {
    for (channel in rev(names(restorers))) {
      tryCatch(
        restorers[[channel]](),
        error = function(e) {
          warning(sprintf("Could not restore the %s thread count: %s", channel, conditionMessage(e)),
                  call. = FALSE)
        }
      )
    }
    invisible(NULL)
  }
  if (is.null(controller)) {
    return(restore)
  }
  channels <- list(BLAS = c("get_blas", "set_blas"), OpenMP = c("get_omp", "set_omp"))
  withCallingHandlers({
    for (channel in names(channels)) {
      previous <- controller[[channels[[channel]][1]]]()
      if (!is_thread_count(previous)) {
        next
      }
      restorers[[channel]] <- local({
        setter <- controller[[channels[[channel]][2]]]
        value <- previous
        function() setter(value)
      })
      controller[[channels[[channel]][2]]](1L)
    }
  }, error = function(e) restore())
  restore
}
