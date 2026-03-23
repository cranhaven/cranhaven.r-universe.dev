ACF <- NULL

#' Adaptive Trend and Cycle Analysis for Time Series (universal version)
#'
#' @description
#' Performs adaptive trend estimation, cycle detection, Fourier harmonic
#' selection, bootstrap confidence intervals, change points detection, and
#' rolling-origin forecasting. Supports LOESS, GAM, and GAMM models, and
#' handles irregular sampling using the Lomb-Scargle periodogram.
#'
#' Works with:
#' - dates_type = "date"   : Date (daily/weekly/monthly data)
#' - dates_type = "posix"  : POSIXct (sub-daily, wearable, EEG, sensors)
#' - dates_type = "numeric": numeric time (spike trains, simulations)
#'
#' @param signal Numeric vector of observed values.
#' @param dates Vector of time indices (Date, POSIXct, or numeric).
#' @param dates_type "date", "posix", or "numeric".
#' @param normalize Logical; if TRUE, Z score normalization is applied.
#' @param trendmethod "loess" or "gam".
#' @param usefourier Logical; whether to include Fourier harmonics.
#' @param fourierK Integer; fixed number of harmonics if auto selection disabled.
#' @param auto_fourier_select Logical; if TRUE, selects K via AICc/BIC.
#' @param fourier_selection_criterion "AICc" or "BIC".
#' @param fourierK_max Maximum K to consider during selection.
#' @param cimethod "model", "bootstrapiid", or "bootstrapmbb".
#' @param nboot Number of bootstrap samples.
#' @param blocksize Block size for MBB bootstrap.
#' @param seasonalfrequency Seasonal frequency for STL (only for dates_type="date").
#' @param stlrobust Logical; robust STL decomposition.
#' @param specspans Smoothing spans for spectral estimation.
#' @param auto_seasonality Logical; if TRUE, uses dominant period.
#' @param lagmax Maximum lag for ACF and Ljung Box tests.
#' @param loess_span_mode "auto_aicc", "auto_gcv", "cv", "fixed".
#' @param loess_span_fixed Numeric; fixed LOESS span.
#' @param loess_span_grid Grid of spans for CV.
#' @param loess_cv_k Number of folds for blocked CV.
#' @param blocklength_mode "auto_pwsd", "heuristic", "fixed".
#' @param blocklength_fixed Fixed block length.
#' @param robust Logical; robust LOESS or robust GAM family.
#' @param use_gamm Logical; fit GAMM instead of GAM.
#' @param group_var Character; grouping variable for random intercepts.
#' @param group_values Optional vector to attach as grouping variable.
#' @param random_effect Optional random effects list for mgcv::gamm.
#' @param cor_struct "none", "ar1", "arma".
#' @param arma_p,arma_q ARMA orders.
#' @param forecast_holdout_h Holdout horizon for forecasting.
#' @param forecast_origin_mode "expanding" or "sliding".
#' @param train_window Training window for sliding origin.
#' @param forecast_lock_K Logical; lock Fourier K across origins.
#'
#' @return A list with:
#' - Data (with PlotDate, timenum, Trend, CI, Outlier)
#' - Trend
#' - CI (lower, upper)
#' - Residuals
#' - Fourier (K)
#' - ChangePoints (in PlotDate scale)
#' - Spectrum
#' - Plot (Trend, Spectrum)
#'
#' @importFrom stats Box.test acf as.formula coef gaussian lm loess median predict quantile setNames spec.pgram
#' @importFrom lomb lsp
#'
#' @export
adaptive_cycle_trend_analysis <- function(
    signal,
    dates,
    dates_type = c("date", "posix", "numeric"),
    normalize = FALSE,
    trendmethod = c("loess", "gam"),
    usefourier = FALSE,
    fourierK = 2,
    auto_fourier_select = TRUE,
    fourier_selection_criterion = c("AICc", "BIC"),
    fourierK_max = 6,
    cimethod = c("model", "bootstrapiid", "bootstrapmbb"),
    nboot = 1000,
    blocksize = NULL,
    seasonalfrequency = 7,
    stlrobust = TRUE,
    specspans = c(7, 7),
    auto_seasonality = TRUE,
    lagmax = NULL,
    loess_span_mode = c("auto_aicc", "auto_gcv", "cv", "fixed"),
    loess_span_fixed = NULL,
    loess_span_grid = seq(0.15, 0.60, by = 0.05),
    loess_cv_k = 5,
    blocklength_mode = c("auto_pwsd", "heuristic", "fixed"),
    blocklength_fixed = NULL,
    robust = TRUE,
    use_gamm = FALSE,
    group_var = NULL,
    group_values = NULL,
    random_effect = NULL,
    cor_struct = c("none", "ar1", "arma"),
    arma_p = 1, arma_q = 0,
    forecast_holdout_h = 0,
    forecast_origin_mode = c("expanding", "sliding"),
    train_window = NULL,
    forecast_lock_K = TRUE
) {

  `%||%` <- function(a, b) if (!is.null(a)) a else b

  trendmethod <- match.arg(trendmethod)
  cimethod    <- match.arg(cimethod)
  loess_span_mode <- match.arg(loess_span_mode)
  blocklength_mode <- match.arg(blocklength_mode)
  forecast_origin_mode <- match.arg(forecast_origin_mode)
  fourier_selection_criterion <- match.arg(fourier_selection_criterion)
  cor_struct <- match.arg(cor_struct)
  dates_type <- match.arg(dates_type)

  # ---- Validate basic ----
  if (!is.numeric(signal)) stop("signal must be numeric.")
  if (length(signal) != length(dates)) stop("signal and dates must have same length.")
  if (usefourier && fourierK < 0) stop("fourierK must be >= 0.")
  N <- length(signal)
  if (forecast_holdout_h < 0 || forecast_holdout_h > max(0, N - 5)) {
    stop("forecast_holdout_h must be between 0 and N-5.")
  }
  if (use_gamm && !requireNamespace("nlme", quietly = TRUE)) {
    stop("use_gamm=TRUE requires the 'nlme' package. Please install.packages('nlme').")
  }
  if (use_gamm && identical(trendmethod, "loess")) {
    message("Note: use_gamm=TRUE is ignored because trendmethod='loess'. Set trendmethod='gam' to enable GAMM.")
  }

  # ---- Normalize ----
  if (normalize) {
    signal <- as.numeric(scale(signal))
    message("Data normalized using Z-score standardization.")
  } else {
    message("Using original scale.")
  }

  # ---- Time handling: timenum + PlotDate ----
  if (dates_type == "date") {
    if (!inherits(dates, "Date"))
      stop("dates must be of class Date when dates_type='date'.")
    timenum <- as.numeric(dates)          # days since origin
    PlotDate <- dates
    use_stl <- TRUE
  } else if (dates_type == "posix") {
    if (!inherits(dates, "POSIXct"))
      stop("dates must be POSIXct when dates_type='posix'.")
    timenum <- as.numeric(difftime(dates, min(dates), units = "secs"))
    PlotDate <- dates
    use_stl <- FALSE
    auto_seasonality <- FALSE
  } else { # numeric
    if (!is.numeric(dates))
      stop("dates must be numeric when dates_type='numeric'.")
    timenum <- dates
    PlotDate <- timenum
    use_stl <- FALSE
    auto_seasonality <- FALSE
  }

  # ---- Regularity ----
  datediff <- diff(timenum)
  irregular <- if (length(datediff) > 0) any(abs(datediff - median(datediff)) > 1e-6) else FALSE
  message(ifelse(irregular,
                 "Irregular time index: Lomb-Scargle + LOESS/GAM(M)",
                 "Regular time index: STL (if dates_type='date') + LOESS/GAM(M)"))

  avginterval <- if (length(datediff)) mean(datediff) else 1

  # ---- Data frame ----
  df <- data.frame(
    PlotDate = PlotDate,
    Value    = signal,
    timenum  = timenum
  )

  # ---- Attach/convert group_var ----
  if (use_gamm && !is.null(group_var)) {
    if (!is.character(group_var) || length(group_var) != 1L || nchar(group_var) == 0L) {
      stop("`group_var` must be a non-empty character scalar naming a column.")
    }
    if (!group_var %in% names(df)) {
      if (!is.null(group_values)) {
        if (length(group_values) != nrow(df)) {
          stop(sprintf("`group_values` length (%d) must match data length (%d).",
                       length(group_values), nrow(df)))
        }
        df[[group_var]] <- group_values
        message(sprintf("Attached grouping column '%s' from `group_values`.", group_var))
      }
    }
    if (!group_var %in% names(df)) {
      stop(sprintf("group_var '%s' not found in the data.", group_var))
    }
    if (!is.factor(df[[group_var]])) {
      df[[group_var]] <- as.factor(df[[group_var]])
      message(sprintf("Converted group_var '%s' to factor (%d levels).",
                      group_var, nlevels(df[[group_var]])))
    }
  }

  # ---- Dominant period estimation (unified) ----
  estimate_dominant <- function(x, timenum, irregular, specspans, avginterval) {
    if (!irregular && dates_type == "date") {
      spec <- spec.pgram(x, spans = specspans, log = "no", taper = 0.1, detrend = TRUE)
      freq <- spec$freq; power <- spec$spec
      if (length(power) > 0) {
        ix <- which.max(power); dfreq <- freq[ix]
        list(
          freq = dfreq,
          period_samples = if (!is.na(dfreq) && dfreq > 0) 1 / dfreq else NA_real_,
          period_units   = if (!is.na(dfreq) && dfreq > 0) (1 / dfreq) * avginterval else NA_real_
        )
      } else {
        list(freq = NA_real_, period_samples = NA_real_, period_units = NA_real_)
      }
    } else {
      tnum <- timenum - min(timenum)
      lombres <- try(lsp(x, tnum, type = "frequency", plot = FALSE), silent = TRUE)
      if (inherits(lombres, "try-error") ||
          is.null(lombres$peak.at) || length(lombres$peak.at) < 2 ||
          any(!is.finite(lombres$peak.at))) {
        return(list(freq = NA_real_, period_samples = NA_real_, period_units = NA_real_))
      }
      dfreq <- lombres$peak.at[1]
      p_units <- lombres$peak.at[2]
      list(
        freq = dfreq,
        period_samples = if (!is.na(dfreq) && dfreq > 0) 1 / dfreq else NA_real_,
        period_units   = p_units
      )
    }
  }

  dom <- estimate_dominant(signal, timenum, irregular, specspans, avginterval)

  # ---- Seasonality from dominant period ----
  if (dates_type == "date" && auto_seasonality &&
      !is.na(dom$period_samples) && dom$period_samples > 1) {
    seasonalfrequency <- max(2L, round(dom$period_samples))
    message(sprintf("Auto seasonality: dominant period approx %.2f samples -> seasonalfrequency=%d",
                    dom$period_samples, seasonalfrequency))
  } else if (dates_type == "date") {
    message(sprintf("Using provided seasonalfrequency=%d", seasonalfrequency))
  } else {
    message("No STL/seasonality for dates_type != 'date'.")
  }

  period_units <- if (!is.na(dom$period_units)) dom$period_units else seasonalfrequency * avginterval

  # ---- Fourier design (in units of timenum) ----
  makefourier <- function(timenum, period_units, K) {
    phi <- 2 * pi * timenum / period_units
    out <- data.frame(row.names = seq_along(timenum))
    if (K >= 1) for (k in seq_len(K)) {
      out[[paste0("sin", k)]] <- sin(k * phi)
      out[[paste0("cos", k)]] <- cos(k * phi)
    }
    out
  }

  maxK <- if (usefourier) max(fourierK, fourierK_max) else 0
  full_fourier_design <- if (usefourier && maxK > 0 && is.finite(period_units) && period_units > 0) {
    makefourier(timenum, period_units, maxK)
  } else {
    NULL
  }

  # ---- Helper: Blocked k-fold CV for LOESS span ----
  blocked_loess_cv <- function(y, x, spans, k = 5, robust = TRUE) {
    n <- length(y)
    k <- min(k, max(2, floor(n/2)))
    idx <- seq_len(n)
    folds <- split(idx, cut(idx, breaks = k, labels = FALSE))
    mse <- sapply(spans, function(sp) {
      errs <- numeric(0)
      for (fi in seq_along(folds)) {
        val <- folds[[fi]]; train <- setdiff(idx, val)
        fit <- try(loess(y ~ x, span = sp, degree = 2,
                         family = if (robust) "symmetric" else "gaussian",
                         subset = train), silent = TRUE)
        if (inherits(fit, "try-error")) next
        pred <- try(predict(fit, newdata = data.frame(x = x[val])), silent = TRUE)
        if (inherits(pred, "try-error")) next
        errs <- c(errs, mean((y[val] - pred)^2, na.rm = TRUE))
      }
      mean(errs, na.rm = TRUE)
    })
    spans[which.min(mse)]
  }

  # ---- Trend (base, without Fourier) ----
  df$FourierComponent <- 0
  trendlabel <- NULL
  trendobj <- NULL

  # Decide LOESS span if needed
  span_opt <- NULL
  if (trendmethod == "loess") {
    if (loess_span_mode == "fixed") {
      if (is.null(loess_span_fixed)) stop("loess_span_fixed must be provided when loess_span_mode='fixed'.")
      span_opt <- loess_span_fixed
      message(sprintf("Using fixed LOESS span = %.3f", span_opt))
    } else {
      have_fancova <- requireNamespace("fANCOVA", quietly = TRUE)
      if (have_fancova && loess_span_mode %in% c("auto_aicc", "auto_gcv")) {
        crit <- if (loess_span_mode == "auto_aicc") "aicc" else "gcv"
        as_fit <- try(
          fANCOVA::loess.as(
            x = df$timenum, y = df$Value,
            criterion = crit,
            family = if (robust) "symmetric" else "gaussian",
            plot = FALSE
          ),
          silent = TRUE
        )
        if (!inherits(as_fit, "try-error") && !is.null(as_fit$pars$span)) {
          span_opt <- as_fit$pars$span
          message(sprintf("loess.as proposed span (criterion=%s) = %.3f", crit, span_opt))
        }
      }
      if (is.null(span_opt) || loess_span_mode == "cv") {
        span_opt <- blocked_loess_cv(
          y = df$Value, x = df$timenum,
          spans = loess_span_grid, k = loess_cv_k, robust = robust
        )
        message(sprintf("CV-selected LOESS span = %.3f", span_opt))
      }
    }
    loessfit <- loess(Value ~ timenum, data = df,
                      span = span_opt, degree = 2,
                      family = if (robust) "symmetric" else "gaussian")
    base_trend <- as.numeric(predict(loessfit, newdata = data.frame(timenum = timenum)))
    trendobj <- loessfit
    trendlabel <- sprintf("LOESS (span=%.2f%s)", span_opt, if (robust) ", robust" else "")
  } else {
    # GAM or GAMM
    family_for_fit <- if (use_gamm) gaussian() else if (robust) mgcv::scat() else gaussian()
    if (use_gamm) {
      rand_effect <- if (!is.null(random_effect)) random_effect else {
        if (!is.null(group_var)) setNames(list(~1), group_var) else NULL
      }
      correlation <- NULL
      if (cor_struct != "none") {
        if (!requireNamespace("nlme", quietly = TRUE)) stop("nlme is required for cor_struct.")
        if (cor_struct == "ar1") {
          form <- if (!is.null(group_var)) as.formula(paste0("~ timenum | ", group_var)) else ~ timenum
          correlation <- nlme::corAR1(form = form)
        } else if (cor_struct == "arma") {
          form <- if (!is.null(group_var)) as.formula(paste0("~ timenum | ", group_var)) else ~ timenum
          correlation <- nlme::corARMA(p = arma_p, q = arma_q, form = form)
        }
      }
      gammfit <- mgcv::gamm(
        Value ~ s(timenum, bs = "cs"),
        data = df,
        family = family_for_fit,
        method = "REML",
        random = rand_effect,
        correlation = correlation
      )
      base_trend <- try(as.numeric(stats::predict(gammfit$lme,
                                                  newdata = df,
                                                  level = if (!is.null(group_var) || !is.null(random_effect)) 1 else 0)),
                        silent = TRUE)
      if (inherits(base_trend, "try-error") || any(!is.finite(base_trend))) {
        base_trend <- as.numeric(predict(gammfit$gam, newdata = df))
      }
      trendobj <- gammfit
      edf <- tryCatch(sum(summary(gammfit$gam)$s.table[, "edf"]), error = function(e) NA)
      trendlabel <- paste0("GAMM (REML",
                           if (!is.na(edf)) paste0(", edf approx", round(edf, 2)) else "",
                           if (!is.null(group_var) || !is.null(random_effect)) ", random effects" else "",
                           if (cor_struct != "none") paste0(", cor=", cor_struct) else "",
                           ")")
    } else {
      gamfit <- mgcv::gam(Value ~ s(timenum, bs = "cs"), data = df, method = "REML",
                          family = family_for_fit)
      base_trend <- as.numeric(predict(gamfit, newdata = df))
      trendobj <- gamfit
      edf <- tryCatch(sum(summary(gamfit)$s.table[, "edf"]), error = function(e) NA)
      trendlabel <- paste0("GAM (REML",
                           if (!is.na(edf)) paste0(", edf approx", round(edf, 2)) else "",
                           if (robust && !use_gamm) ", robust=scat" else "",
                           ")")
    }
  }

  # ---- Automatic Fourier K selection via AICc/BIC ----
  selectedK <- 0
  fourfit <- NULL
  if (usefourier) {
    resid_base <- df$Value - base_trend
    crit_vals <- rep(Inf, fourierK_max + 1)   # index 1 corresponds to K=0
    fit_list <- vector("list", length = fourierK_max + 1)
    crit_vals[1] <- {
      rss <- sum((resid_base - mean(resid_base))^2, na.rm = TRUE)
      n <- sum(is.finite(resid_base))
      kparams <- 1
      if (fourier_selection_criterion == "BIC") {
        n*log(rss/n) + kparams*log(n)
      } else {
        aic <- n*log(rss/n) + 2*kparams
        if (n - kparams - 1 > 0) aic + (2*kparams*(kparams+1))/(n - kparams - 1) else aic
      }
    }
    if (!is.null(full_fourier_design)) {
      for (K in 1:fourierK_max) {
        terms <- paste(
          unlist(lapply(seq_len(K), function(k) c(paste0("sin", k), paste0("cos", k)))),
          collapse = " + "
        )
        fml <- as.formula(paste0("resid_base ~ ", terms))
        fitK <- try(lm(fml, data = cbind(df, full_fourier_design)), silent = TRUE)
        if (inherits(fitK, "try-error")) next
        fit_list[[K + 1]] <- fitK
        n <- nrow(df)
        kparams <- length(coef(fitK))
        rss <- sum(residuals(fitK)^2)
        if (fourier_selection_criterion == "BIC") {
          crit_vals[K + 1] <- n*log(rss/n) + kparams*log(n)
        } else {
          aic <- n*log(rss/n) + 2*kparams
          crit_vals[K + 1] <- if (n - kparams - 1 > 0) aic + (2*kparams*(kparams+1))/(n - kparams - 1) else aic
        }
      }
      selected_ix <- which.min(crit_vals)
      selectedK <- selected_ix - 1
      if (selectedK > 0) {
        fourfit <- fit_list[[selected_ix]]
        df$FourierComponent <- as.numeric(predict(fourfit, newdata = cbind(df, full_fourier_design)))
        message(sprintf("Fourier selection: criterion=%s -> K=%d (from 0..%d)",
                        fourier_selection_criterion, selectedK, fourierK_max))
      } else {
        df$FourierComponent <- 0
        message(sprintf("Fourier selection: criterion=%s -> K=0 (no harmonics)", fourier_selection_criterion))
      }
    } else {
      df$FourierComponent <- 0
      selectedK <- 0
      message("Fourier design not available; skipping Fourier component.")
    }
  } else {
    selectedK <- fourierK
    df$FourierComponent <- 0
  }

  # ---- Final Trend (base + Fourier) ----
  df$Trend <- base_trend + df$FourierComponent
  if (usefourier) {
    trendlabel <- sprintf("%s + Fourier (K=%d; period approx %.2f time units)",
                          trendlabel, selectedK, period_units)
  }

  # ---- Residuals & diagnostics ----
  residuals <- df$Value - df$Trend
  if (is.null(lagmax)) lagmax <- max(5L, floor(N/10))
  normalitytest <- nortest::sf.test(residuals)
  acfvalues <- acf(residuals, plot = FALSE, lag.max = lagmax)
  acfsig <- any(abs(acfvalues$acf[-1]) > 1.96 / sqrt(length(residuals)))
  ljungboxtest <- Box.test(residuals, lag = lagmax, type = "Ljung-Box")

  # ---- Confidence intervals ----
  if (missing(cimethod) || is.null(cimethod)) {
    cimethod <- if (ljungboxtest$p.value < 0.05) "bootstrapmbb" else "bootstrapiid"
  }
  cilower <- ciupper <- rep(NA_real_, N)

  refitpredict <- function(yvec) {
    if (trendmethod == "loess") {
      fit1 <- loess(yvec ~ timenum, span = if (exists("span_opt")) span_opt else 0.3,
                    degree = 2, family = if (robust) "symmetric" else "gaussian")
      tr1 <- as.numeric(predict(fit1, newdata = data.frame(timenum = timenum)))
    } else {
      if (use_gamm) {
        rand_effect <- if (!is.null(random_effect)) random_effect else {
          if (!is.null(group_var)) setNames(list(~1), group_var) else NULL
        }
        correlation <- NULL
        if (cor_struct != "none") {
          if (cor_struct == "ar1") {
            form <- if (!is.null(group_var)) as.formula(paste0("~ timenum | ", group_var)) else ~ timenum
            correlation <- nlme::corAR1(form = form)
          } else if (cor_struct == "arma") {
            form <- if (!is.null(group_var)) as.formula(paste0("~ timenum | ", group_var)) else ~ timenum
            correlation <- nlme::corARMA(p = arma_p, q = arma_q, form = form)
          }
        }
        fit <- mgcv::gamm(
          Value ~ s(timenum, bs = "cs"),
          data = transform(df, Value = yvec),
          family = gaussian(), method = "REML",
          random = rand_effect, correlation = correlation
        )
        tr1 <- try(as.numeric(stats::predict(fit$lme,
                                             newdata = df,
                                             level = if (!is.null(group_var) || !is.null(random_effect)) 1 else 0)),
                   silent = TRUE)
        if (inherits(tr1, "try-error") || any(!is.finite(tr1))) {
          tr1 <- as.numeric(predict(fit$gam, newdata = df))
        }
      } else {
        fit <- mgcv::gam(Value ~ s(timenum, bs = "cs"),
                         data = transform(df, Value = yvec),
                         method = "REML",
                         family = if (robust) mgcv::scat() else gaussian())
        tr1 <- as.numeric(predict(fit, newdata = df))
      }
    }
    if (usefourier && selectedK > 0 && !is.null(full_fourier_design)) {
      resid1 <- yvec - tr1
      terms <- paste(unlist(lapply(seq_len(selectedK), function(k) c(paste0("sin", k), paste0("cos", k)))), collapse = " + ")
      fml <- as.formula(paste0("resid1 ~ ", terms))
      fit2 <- lm(fml, data = cbind(df, full_fourier_design))
      four <- as.numeric(predict(fit2, newdata = cbind(df, full_fourier_design)))
      tr1 + four
    } else tr1
  }

  if (cimethod == "model") {
    if (trendmethod == "loess" && (!usefourier || selectedK == 0)) {
      pred <- predict(trendobj, newdata = data.frame(timenum = timenum), se = TRUE)
      ciupper <- pred$fit + 1.96 * pred$se.fit
      cilower <- pred$fit - 1.96 * pred$se.fit
    } else if (trendmethod == "gam" && !use_gamm && (!usefourier || selectedK == 0)) {
      pred <- predict(trendobj, newdata = df, se.fit = TRUE)
      ciupper <- pred$fit + 1.96 * pred$se.fit
      cilower <- pred$fit - 1.96 * pred$se.fit
    } else {
      warning("Model-based CI not implemented for this configuration; switching to bootstrapiid.")
      cimethod <- "bootstrapiid"
    }
  }

  if (cimethod %in% c("bootstrapiid", "bootstrapmbb")) {
    if (cimethod == "bootstrapmbb") {
      if (!is.null(blocksize)) {
        message(sprintf("Using user-specified MBB blocksize = %d", as.integer(blocksize)))
      } else if (blocklength_mode == "fixed") {
        if (is.null(blocklength_fixed)) stop("blocklength_fixed must be provided when blocklength_mode='fixed'.")
        blocksize <- as.integer(blocklength_fixed)
        message(sprintf("Using fixed MBB blocksize = %d", blocksize))
      } else if (blocklength_mode == "auto_pwsd") {
        have_blocklength <- requireNamespace("blocklength", quietly = TRUE)
        if (have_blocklength) {
          pw <- try(blocklength::pwsd(series = residuals, plots = FALSE), silent = TRUE)
          if (!inherits(pw, "try-error") && !is.null(pw$block_length)) {
            blocksize <- as.integer(round(pw$block_length))
            message(sprintf("PWSD-selected MBB blocksize = %d", blocksize))
          }
        }
        if (is.null(blocksize)) {
          n <- length(residuals)
          crit <- 1.96 / sqrt(n)
          acfobj <- acf(residuals, plot = FALSE, lag.max = max(5L, floor(n/10)))
          siglags <- which(abs(acfobj$acf[-1]) > crit)
          corr_len <- if (length(siglags)) min(siglags) else 1L
          blocksize <- as.integer(max(5L, floor(max(n^(1/3), 1.5 * corr_len))))
          message(sprintf("Heuristic MBB blocksize = %d (fallback)", blocksize))
        }
      } else {
        n <- length(residuals)
        crit <- 1.96 / sqrt(n)
        acfobj <- acf(residuals, plot = FALSE, lag.max = max(5L, floor(n/10)))
        siglags <- which(abs(acfobj$acf[-1]) > crit)
        corr_len <- if (length(siglags)) min(siglags) else 1L
        blocksize <- as.integer(max(5L, floor(max(n^(1/3), 1.5 * corr_len))))
        message(sprintf("Heuristic MBB blocksize = %d", blocksize))
      }
    }

    mbbresample <- function(residvec, B, blocksize) {
      n  <- length(residvec)
      bs <- as.integer(blocksize)
      out <- matrix(NA_real_, nrow = n, ncol = B)
      starts <- 1:(n - bs + 1)
      for (b in seq_len(B)) {
        blocks <- integer(0)
        while (length(blocks) < n) {
          s <- sample(starts, 1)
          blocks <- c(blocks, residvec[s:(s + bs - 1)])
        }
        out[, b] <- blocks[seq_len(n)]
      }
      out
    }

    boottrends <- matrix(NA_real_, nrow = N, ncol = nboot)
    if (cimethod == "bootstrapiid") {
      for (b in seq_len(nboot)) {
        epsb <- sample(residuals, replace = TRUE)
        yb   <- df$Trend + epsb
        boottrends[, b] <- refitpredict(yb)
      }
    } else {
      mbbmat <- mbbresample(residuals, nboot, blocksize)
      for (b in seq_len(nboot)) {
        yb <- df$Trend + mbbmat[, b]
        boottrends[, b] <- refitpredict(yb)
      }
    }
    cilower <- apply(boottrends, 1, quantile, probs = 0.025, na.rm = TRUE)
    ciupper <- apply(boottrends, 1, quantile, probs = 0.975, na.rm = TRUE)
    message(sprintf("Bootstrap CI (%s): %d resamples%s",
                    cimethod, nboot,
                    if (cimethod == "bootstrapmbb") paste0("; blocksize=", blocksize) else ""))
  }

  # ---- Outliers ----
  df$Outlier <- df$Trend < cilower | df$Trend > ciupper

  # ---- Stationarity tests, change-points, spectra ----
  adftest  <- tseries::adf.test(signal, alternative = "stationary")
  kpsstest <- tseries::kpss.test(signal, null = "Level")
  adfinterpret  <- if (adftest$p.value < 0.05) "ADF: Reject H0 -> likely stationary" else "ADF: Fail to reject H0 -> likely non stationary"
  kpssinterpret <- if (kpsstest$p.value < 0.05) "KPSS: Reject H0 -> likely non stationary" else "KPSS: Fail to reject H0 -> likely stationary"

  cpobj    <- changepoint::cpt.meanvar(df$Trend, method = "PELT", penalty = "BIC")
  cppoints <- changepoint::cpts(cpobj)
  cpdates  <- if (length(cppoints)) df$PlotDate[cppoints] else PlotDate[0]

  # ---- Spectra for plotting ----
  spectrumdf <- significantspectrum <- data.frame()
  significantperiods <- numeric(0)
  dominantperiodunits <- dom$period_units
  dominantfrequency  <- dom$freq

  if (use_stl && !irregular && dates_type == "date") {
    tsdata <- stats::ts(signal, frequency = seasonalfrequency)
    decomp <- stats::stl(tsdata, s.window = "periodic", robust = stlrobust)
    df$Seasonal <- as.numeric(decomp$time.series[, "seasonal"])
    spec  <- stats::spec.pgram(signal, spans = specspans, log = "no", taper = 0.1, detrend = TRUE)
    freq  <- spec$freq; power <- spec$spec
    spectrumdf <- data.frame(Frequency = freq, Power = power)
    k      <- spec$df; alpha <- 0.01
    upper  <- power * stats::qchisq(1 - alpha/2, 2 * k) / (2 * k)
    sigix  <- which(power > upper)
    significantspectrum <- spectrumdf[sigix, , drop = FALSE]
    significantperiods <- round(1 / significantspectrum$Frequency * avginterval, 2)
  } else {
    tnum <- timenum - min(timenum)
    lombres  <- lomb::lsp(signal, tnum, type = "frequency", plot = FALSE)
    lombdf   <- data.frame(Frequency = lombres$scanned, Power = lombres$power)
    spectrumdf <- lombdf
    signiflevel <- lombres$sig.level
    sigix <- which(lombres$power > signiflevel)
    significantspectrum <- lombdf[sigix, , drop = FALSE]
    significantperiods  <- round(1 / significantspectrum$Frequency, 2)
  }

  # ---- Plots ----
  ptrend <- ggplot2::ggplot(df, ggplot2::aes(x = PlotDate)) +
    ggplot2::geom_line(ggplot2::aes(y = Value), color = "steelblue", linewidth = 0.8) +
    ggplot2::geom_line(ggplot2::aes(y = Trend), color = "darkred", linewidth = 0.9) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = cilower, ymax = ciupper),
                         fill = "grey80", alpha = 0.35) +
    ggplot2::geom_point(data = subset(df, Outlier),
                        ggplot2::aes(y = Trend), color = "orange", size = 2) +
    { if (length(cpdates)) ggplot2::geom_vline(xintercept = cpdates,
                                               linetype = "dotted", color = "orange") } +
    ggplot2::labs(title = sprintf("Trend (%s) with 95%% CI (%s)",
                                  trendlabel, cimethod),
                  y = "Signal", x = "Time") +
    ggplot2::theme_minimal()

  pperiod <- ggplot2::ggplot(spectrumdf, ggplot2::aes(x = Frequency, y = Power)) +
    ggplot2::geom_line(color = "black", linewidth = 0.6) +
    ggplot2::geom_point(data = significantspectrum,
                        ggplot2::aes(x = Frequency, y = Power),
                        color = "red", size = 1.6) +
    ggplot2::theme_minimal()

  # ---- Return ----
  list(
    Data = df,
    Trend = df$Trend,
    CI = list(lower = cilower, upper = ciupper),
    Residuals = residuals,
    Fourier = list(K = selectedK,
                   dominant_period = dominantperiodunits,
                   dominant_frequency = dominantfrequency),
    ChangePoints = cpdates,
    Spectrum = spectrumdf,
    Plot = list(Trend = ptrend, Spectrum = pperiod),
    Tests = list(
      ADF = list(statistic = adftest$statistic, p.value = adftest$p.value, interpretation = adfinterpret),
      KPSS = list(statistic = kpsstest$statistic, p.value = kpsstest$p.value, interpretation = kpssinterpret),
      LjungBox = list(statistic = ljungboxtest$statistic, p.value = ljungboxtest$p.value)
    )
  )
}
