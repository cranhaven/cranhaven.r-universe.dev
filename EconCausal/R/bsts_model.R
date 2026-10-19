#' Bayesian structural time series with leave-future-out validation
#'
#' Evaluates every directed pair between six circulation variables and seven production
#' variables, in both directions, by comparing a structural time series model of the response
#' with and without lagged values of the predictor. Each comparison is repeated over successive
#' forecast windows, and a direction counts as supported in a window only when adding the
#' predictor improves both the predictive density and the point forecast. Use it to rank directed
#' relationships by how consistently the predictor improves out-of-sample forecasts.
#'
#' @param data_path Character scalar. Path to an Excel file with a date column and the series
#'   named in `circ_vars` and `prod_vars`. Column names are cleaned before matching: a leading
#'   `as.numeric.`, trailing `.NEW.` or `.1.588.` and trailing dots are removed, and remaining
#'   dots become underscores. A date column named `Month` is used, or the first date-time
#'   column is renamed to `Month`.
#' @param circ_vars Character vector of exactly six circulation variable names, after cleaning.
#' @param prod_vars Character vector of exactly seven production variable names, after cleaning.
#' @param max_lag Integer scalar >= 1, default `6`. Number of lags of the predictor used as
#'   regressors.
#' @param lfo_init_frac Numeric scalar in (0, 1), default `0.8`. Fraction of the complete
#'   observations in the first training window (at least 30 rows).
#' @param lfo_h Integer scalar >= 1, default `6`. Forecast horizon of each validation window.
#' @param lfo_step Integer scalar >= 1, default `6`. Advance between validation windows.
#' @param niter Integer scalar > `burn`, default `2000`. Number of MCMC iterations per model.
#' @param burn Integer scalar >= 0 and < `niter`, default `500`. MCMC iterations discarded
#'   before forecasting.
#' @param seed Integer scalar, default `123`. Base seed; model fits and forecasts in window `i`
#'   use `seed + i` (model without predictor) and `seed + 1000 + i` (model with predictor).
#' @param seasonality `NULL` (default) or an integer scalar >= 2 giving the number of seasons
#'   of a seasonal state component.
#' @param support_min Numeric scalar in \[0, 1\], default `0.6`. Minimum support for
#'   `pass_support`.
#' @param folds_min Integer scalar >= 0, default `5`. Minimum number of evaluated windows for
#'   `pass_support`.
#' @param sup_hi Numeric scalar in \[0, 1\], default `0.7`. Support threshold of
#'   `winners_ss_070`.
#' @param sup_lo Numeric scalar in \[0, 1\], default `0.6`. Support threshold of
#'   `winners_ss_060`.
#' @param out_dir `NULL` (default) or a character scalar. If given, the directory is created and
#'   the three ranking tables are written there as CSV files.
#'
#' @return A list with four tibbles, or `NULL` with a warning when no direction produced a
#'   result: `summaries_ss`, one row per direction with the selected specification `spec`
#'   (`"LL"` or `"LLT"`), `folds`, `wins`, `support` (`wins / folds`), `dELPD_mean`,
#'   `dRMSE_mean`, `dMAE_mean`, `RMSE_base_mean`, `RMSE_full_mean`, `MAE_base_mean`,
#'   `MAE_full_mean`, `cover80_mean`, `cover95_mean`, `Y` and `X`; `rank_ss_all`, the same rows
#'   with `pair` and `pass_support`, sorted by decreasing `support`, `dELPD_mean` and
#'   `dRMSE_mean`; `winners_ss_070` and `winners_ss_060`, the rows of `rank_ss_all` with
#'   `pass_support`, support at least `sup_hi` or `sup_lo`, and positive `dELPD_mean` and
#'   `dRMSE_mean`. The names `winners_ss_070` and `winners_ss_060` do not change with `sup_hi`
#'   and `sup_lo`.
#'
#' @details
#' For each direction the predictor is lagged `max_lag` times and incomplete rows are dropped.
#' Validation windows use an expanding training sample and a test block of `lfo_h`
#' observations that always lies after the training sample. In each window the lags are
#' standardised with the training mean and standard deviation, and two models are fitted to the
#' training response with the same state specification: a model without regressors and a model
#' that adds a regression on the standardised lags with a spike-and-slab prior. The prior is
#' built on the design matrix including the intercept, with an expected model size equal to the
#' number of columns of that matrix capped at 5, prior information weight `0.01` and diagonal
#' shrinkage `0.5`.
#'
#' Forecasts use the posterior predictive draws after `burn`. For each model the window records
#' the Gaussian log predictive density of the test observations given the forecast mean and
#' standard deviation, RMSE, MAE and the coverage of central 80 and 95 percent Gaussian
#' intervals built from the same mean and standard deviation. A
#' window is a win when the model with the predictor has both higher log predictive density and
#' lower RMSE. Two state specifications are evaluated, local level (`"LL"`) and local linear
#' trend (`"LLT"`), each with the optional seasonal component, and the one with the highest
#' `dELPD_mean`, then `support`, then `dRMSE_mean`, is reported for the direction.
#'
#' @section Methodological notes:
#' `support` requires improvement in probabilistic fit and in point error at the same time; a
#' predictor that improves only one of them in a window does not count. The log predictive
#' density is a Gaussian approximation computed from the forecast mean and standard deviation,
#' not the exact predictive density of the draws. Standardising lags with training statistics
#' only keeps test-window information out of the fit. Models without regressors are fitted on
#' the response vector, as `bsts` documents for models with no regression component. Tying each
#' forecast seed to the seed of its fit makes the results reproducible for a given `seed`; with
#' a different `seed`, support can change, especially with few MCMC iterations.
#'
#' @section Dependencies:
#' `bsts` builds the state specification, runs the MCMC and produces the forecasts;
#' `BoomSpikeSlab` builds the spike-and-slab prior. Both are suggested packages that this
#' function requires; it stops with an informative message when either is missing. `readxl`
#' reads the data; `dplyr`, `tidyr`, `tibble` and `purrr` build lags, windows and summaries;
#' `stats` and `utils` provide the metrics and the CSV export.
#'
#' @references
#' \enc{Bürkner}{Burkner}, P.-C., Gabry, J., & Vehtari, A. (2020). Approximate leave-future-out
#' cross-validation for Bayesian time series models. *Journal of Statistical Computation and
#' Simulation, 90*(14), 2499\enc{–}{-}2523. \doi{10.1080/00949655.2020.1783262}
#'
#' Scott, S. L., & Varian, H. R. (2014). Predicting the present with Bayesian structural time
#' series. *International Journal of Mathematical Modelling and Numerical Optimisation,
#' 5*(1/2), 4\enc{–}{-}23. \doi{10.1504/IJMMNO.2014.059942}
#'
#' @seealso [ecm_mars()], [bglmar1()]; the vignettes `bsts-eng` and `bsts-esp`.
#'
#' @examples
#' \dontrun{
#' result <- bsts_model(
#'   data_path = file.path(tempdir(), "data.xlsx"),
#'   circ_vars = c("TC_SPOT_CAN_US", "TC_SPOT_US_CAN", "TC_SPOT_US_REMB",
#'                 "IPC", "TdI_LdelT", "TasaDescuento"),
#'   prod_vars = c("ValorExportaciones", "Real_Net_Profit",
#'                 "RealSocialConsumptionPerWorker2017", "RealWage_PPP2017",
#'                 "CapitalStock_PPP2017", "LaborProductivity_PPP2017",
#'                 "InvestmentPerWorker_PPP2017")
#' )
#' }
#'
#' @export
bsts_model <- function(data_path, circ_vars, prod_vars, max_lag = 6, lfo_init_frac = 0.8,
                       lfo_h = 6, lfo_step = 6, niter = 2000, burn = 500, seed = 123,
                       seasonality = NULL, support_min = 0.6, folds_min = 5, sup_hi = 0.7,
                       sup_lo = 0.6, out_dir = NULL) {

  if (!requireNamespace("bsts", quietly = TRUE)) {
    stop("Package 'bsts' is required by bsts_model(); install it to use this function.", call. = FALSE)
  }
  if (!requireNamespace("BoomSpikeSlab", quietly = TRUE)) {
    stop("Package 'BoomSpikeSlab' is required by bsts_model(); install it to use this function.", call. = FALSE)
  }

  if (!is.null(out_dir)) {
    dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  }
  old_options <- options()
  on.exit(options(old_options), add = TRUE)
  options(scipen = 0)
  
  simple_name <- function(nm) {
    nm <- gsub("^as\\.numeric\\.", "", nm)
    nm <- gsub("\\.NEW\\.$", "", nm)
    nm <- gsub("\\.1\\.588\\.$", "", nm)
    nm <- gsub("\\.+$", "", nm)
    nm <- gsub("\\.", "_", nm)
    nm
  }
  
  ensure_time_index <- function(df) {
    if (!"Month" %in% names(df)) {
      tc <- names(Filter(function(x) inherits(x, c("POSIXct", "POSIXt", "Date")), df))
      if (length(tc) == 0) stop("No temporal column found")
      names(df)[match(tc[1], names(df))] <- "Month"
    }
    df <- df %>% dplyr::arrange(.data$Month)
    if (!"time_idx" %in% names(df)) df <- df %>% dplyr::mutate(time_idx = dplyr::row_number())
    df
  }
  
  make_lags <- function(x, max_lag) {
    tibble::as_tibble(stats::setNames(lapply(1:max_lag, function(k) dplyr::lag(x, k)),
                                       paste0("L", 1:max_lag)))
  }
  
  make_lfo_splits <- function(n, initial, h, step) {
    splits <- list()
    end_train <- initial
    while ((end_train + 1) <= n - 1) {
      st <- end_train + 1L
      ed <- min(end_train + h, n)
      if (st > ed) break
      splits[[length(splits) + 1L]] <- list(train = 1:end_train, test = st:ed)
      end_train <- min(end_train + step, n - 1L)
    }
    splits
  }
  
  rmse <- function(yhat, y) sqrt(mean((yhat - y)^2))
  mae  <- function(yhat, y) mean(abs(yhat - y))
  sd_from_q <- function(q025, q975) {
    pmax(q975 - q025, 1e-8) / (2 * stats::qnorm(0.975))
  }
  
  elpd_gaussian <- function(y, mean_vec, sd_vec) {
    sdv <- pmax(sd_vec, 1e-8)
    sum(stats::dnorm(y, mean = mean_vec, sd = sdv, log = TRUE))
  }
  
  coverage_and_pit_norm <- function(y, mean_vec, sd_vec) {
    sdv <- pmax(sd_vec, 1e-8)
    pit <- stats::pnorm(y, mean = mean_vec, sd = sdv)
    cover80 <- mean(y >= (mean_vec + stats::qnorm(.10) * sdv) & y <= (mean_vec + stats::qnorm(.90) * sdv))
    cover95 <- mean(y >= (mean_vec + stats::qnorm(.025) * sdv) & y <= (mean_vec + stats::qnorm(.975) * sdv))
    list(cover80 = cover80, cover95 = cover95, pit = pit)
  }
  
  predict_stats_bsts <- function(model, h, newdata = NULL, burn = burn, seed = NULL,
                                 probs80 = c(.10, .90), probs95 = c(.025, .975)) {
    pr <- stats::predict(model, horizon = h, newdata = newdata, burn = burn, seed = seed,
                        quantiles = c(probs95[1], probs95[2]))
    
    draws <- NULL
    if (!is.null(pr$prediction.matrix) && is.matrix(pr$prediction.matrix)) {
      draws <- pr$prediction.matrix
    } else if (!is.null(pr$distribution)) {
      D <- pr$distribution
      if (is.matrix(D)) draws <- D
      else if (is.list(D) && length(D) > 0 && is.numeric(D[[1]])) draws <- do.call(rbind, D)
    }
    
    if (!is.null(draws) && is.matrix(draws) && nrow(draws) > 1) {
      mean_vec <- colMeans(draws)
      qu <- apply(draws, 2, stats::quantile, probs = c(probs80, probs95), na.rm = TRUE)
      sdv <- apply(draws, 2, stats::sd)
      sdv <- pmax(sdv, 1e-8)
      return(list(mean = mean_vec, sd = sdv, q10 = qu[1, ], q90 = qu[2, ], q025 = qu[3, ], q975 = qu[4, ]))
    }
    
    if (!is.null(pr$interval) && is.matrix(pr$interval) && ncol(pr$interval) >= 2) {
      mean_vec <- as.numeric(pr$mean)
      q025 <- pr$interval[, 1]
      q975 <- pr$interval[, ncol(pr$interval)]
      sdv  <- sd_from_q(q025, q975)
      q10  <- mean_vec + stats::qnorm(probs80[1]) * sdv
      q90  <- mean_vec + stats::qnorm(probs80[2]) * sdv
      sdv  <- pmax(sdv, 1e-8)
      return(list(mean = mean_vec, sd = sdv, q10 = q10, q90 = q90, q025 = q025, q975 = q975))
    }
    
    stop("predict() did not return draws or interval")
  }
  
  build_state_spec <- function(y, trend = FALSE, season = NULL) {
    ss <- bsts::AddLocalLevel(list(), y = y)
    if (isTRUE(trend)) ss <- bsts::AddLocalLinearTrend(ss, y = y)
    if (!is.null(season)) ss <- bsts::AddSeasonal(ss, y = y, nseasons = season)
    ss
  }
  
  make_reg_prior <- function(Xmat, y) {
    if (is.null(Xmat) || ncol(Xmat) == 0) return(NULL)
    BoomSpikeSlab::SpikeSlabPrior(x = Xmat, y = y,
                              expected.model.size = max(1, min(5, ncol(Xmat))),
                              prior.information.weight = 0.01,
                              diagonal.shrinkage = 0.5)
  }
  
  fit_pair_bsts_lfo_once <- function(df, Y, X, struct_name = "LLT", trend = TRUE, season = NULL) {
    df2 <- df %>%
      dplyr::select(dplyr::all_of(c("Month", "time_idx", Y, X))) %>%
      dplyr::mutate(dplyr::across(dplyr::all_of(c(Y, X)), as.numeric)) %>%
      dplyr::filter(is.finite(.data[[Y]]), is.finite(.data[[X]]))

    Xlags <- make_lags(df2[[X]], max_lag)
    names(Xlags) <- paste0(X, "_", names(Xlags))
    Z <- dplyr::bind_cols(df2, Xlags) %>% tidyr::drop_na()

    n <- nrow(Z)
    initial <- max(30, floor(lfo_init_frac * n))
    if (n < (initial + lfo_h + 1)) return(NULL)

    splits <- make_lfo_splits(n, initial, lfo_h, lfo_step)
    if (length(splits) == 0L) return(NULL)

    fold_rows <- vector("list", length(splits))
    for (i in seq_along(splits)) {
      try({
        tr <- splits[[i]]$train; te <- splits[[i]]$test
        train <- Z[tr, , drop = FALSE]
        test  <- Z[te, , drop = FALSE]

        y_tr <- train[[Y]]
        if (!is.numeric(y_tr) || sum(is.finite(y_tr)) < 10) next
        if (stats::sd(y_tr, na.rm = TRUE) <= .Machine$double.eps) next

        regressors <- paste0(X, "_L", 1:max_lag)
        regressors <- intersect(regressors, names(train))
        if (length(regressors)) {
          for (nm in regressors) {
            mu <- mean(train[[nm]], na.rm = TRUE); sdv <- stats::sd(train[[nm]], na.rm = TRUE)
            if (!is.finite(sdv) || sdv <= .Machine$double.eps) {
              train[[nm]] <- NULL; test[[nm]] <- NULL
            } else {
              train[[nm]] <- (train[[nm]] - mu) / sdv
              test[[nm]]  <- (test[[nm]]  - mu) / sdv
            }
          }
          regressors <- intersect(regressors, names(train))
        }

        ss_b <- build_state_spec(y_tr, trend = trend, season = season)
        set.seed(seed + i)
        m_base <- bsts::bsts(
          train[[Y]],
          state.specification = ss_b,
          data = train,
          niter = niter, ping = 0, seed = seed + i
        )

        form_full <- if (length(regressors))
          stats::as.formula(paste(Y, "~", paste(regressors, collapse = " + ")))
        else
          train[[Y]]

        ss_f <- build_state_spec(y_tr, trend = trend, season = season)
        Xmat <- if (length(regressors)) stats::model.matrix(form_full, data = train) else NULL
        prior_f <- make_reg_prior(Xmat, y_tr)

        set.seed(seed + 1000 + i)
        m_full <- bsts::bsts(
          form_full,
          state.specification = ss_f,
          data = train,
          niter = niter, ping = 0, seed = seed + 1000 + i,
          prior = prior_f
        )

        newdata_base <- NULL
        newdata_full <- if (length(regressors)) test[, regressors, drop = FALSE] else NULL

        ps_b <- tryCatch(
          predict_stats_bsts(m_base, h = length(te), newdata = newdata_base, burn = burn, seed = seed + i),
          error = function(e) { message("predict(BASE) failed for ", Y, " <- ", X, " [fold ", i, "]: ", e$message); NULL }
        )
        ps_f <- tryCatch(
          predict_stats_bsts(m_full, h = length(te), newdata = newdata_full, burn = burn, seed = seed + 1000 + i),
          error = function(e) { message("predict(FULL) failed for ", Y, " <- ", X, " [fold ", i, "]: ", e$message); NULL }
        )
        if (is.null(ps_b) || is.null(ps_f)) next

        y_te   <- test[[Y]]
        yhat_b <- ps_b$mean
        yhat_f <- ps_f$mean

        RMSE_base <- rmse(yhat_b, y_te); RMSE_full <- rmse(yhat_f, y_te)
        MAE_base  <- mae(yhat_b, y_te);  MAE_full  <- mae(yhat_f, y_te)

        ELPD_base <- elpd_gaussian(y_te, ps_b$mean, ps_b$sd)
        ELPD_full <- elpd_gaussian(y_te, ps_f$mean, ps_f$sd)

        cal <- coverage_and_pit_norm(y_te, ps_f$mean, ps_f$sd)

        fold_rows[[i]] <- tibble::tibble(
          spec = struct_name,
          fold = i,
          n_train = nrow(train), n_test = nrow(test),
          ELPD_base = ELPD_base, ELPD_full = ELPD_full, dELPD = ELPD_full - ELPD_base,
          RMSE_base = RMSE_base, RMSE_full = RMSE_full, dRMSE = RMSE_base - RMSE_full,
          MAE_base  = MAE_base,  MAE_full  = MAE_full,  dMAE  = MAE_base  - MAE_full,
          cover80 = cal$cover80, cover95 = cal$cover95,
          pit = list(cal$pit),
          q10 = list(ps_f$q10), q90 = list(ps_f$q90), q025 = list(ps_f$q025), q975 = list(ps_f$q975),
          start_test = min(test$Month), end_test = max(test$Month),
          y_obs = list(y_te), yhat_base = list(yhat_b), yhat_full = list(yhat_f),
          idx_test = list(test$time_idx)
        )
      }, silent = TRUE)
    }

    res_folds <- purrr::compact(fold_rows)
    if (length(res_folds) == 0) return(NULL)
    res_folds <- dplyr::bind_rows(res_folds) %>%
      dplyr::mutate(win = (.data$dELPD > 0) & (.data$dRMSE > 0))

    summary <- res_folds %>%
      dplyr::summarise(
        spec           = dplyr::first(.data$spec),
        folds          = dplyr::n(),
        wins           = sum(.data$win, na.rm = TRUE),
        support        = .data$wins / .data$folds,
        dELPD_mean     = mean(.data$dELPD, na.rm = TRUE),
        dRMSE_mean     = mean(.data$dRMSE, na.rm = TRUE),
        dMAE_mean      = mean(.data$dMAE,  na.rm = TRUE),
        RMSE_base_mean = mean(.data$RMSE_base, na.rm = TRUE),
        RMSE_full_mean = mean(.data$RMSE_full, na.rm = TRUE),
        MAE_base_mean  = mean(.data$MAE_base,  na.rm = TRUE),
        MAE_full_mean  = mean(.data$MAE_full,  na.rm = TRUE),
        cover80_mean   = mean(.data$cover80, na.rm = TRUE),
        cover95_mean   = mean(.data$cover95, na.rm = TRUE)
      )

    list(results = res_folds, summary = summary)
  }

  fit_pair_bsts_lfo_tuned <- function(df, Y, X) {
    struct_grid <- list(
      list(name = "LL",  trend = FALSE, season = seasonality),
      list(name = "LLT", trend = TRUE,  season = seasonality)
    )
    fits <- list(); summs <- list()
    for (sg in struct_grid) {
      nm <- sg$name
      cat(sprintf("  - Spec %s (trend=%s, season=%s)\n", nm, sg$trend, ifelse(is.null(sg$season), "NULL", sg$season)))
      fit <- tryCatch(
        fit_pair_bsts_lfo_once(df, Y, X, struct_name = nm, trend = sg$trend, season = sg$season),
        error = function(e) NULL
      )
      fits[[nm]]  <- fit
      summs[[nm]] <- if (is.null(fit)) NULL else fit$summary %>% dplyr::mutate(Y = !!Y, X = !!X)
    }
    summaries <- purrr::compact(summs) %>% purrr::list_rbind()
    if (is.null(summaries) || nrow(summaries) == 0) return(NULL)
    best_row <- summaries %>%
      dplyr::arrange(dplyr::desc(.data$dELPD_mean), dplyr::desc(.data$support), dplyr::desc(.data$dRMSE_mean)) %>%
      dplyr::slice(1)
    best_spec <- best_row$spec[1]; best_fit <- fits[[best_spec]]
    best_row  <- best_row %>% dplyr::mutate(Y = !!Y, X = !!X)
    list(best_summary = best_row,
         best_results = best_fit$results %>% dplyr::mutate(Y = !!Y, X = !!X),
         all_summaries = summaries)
  }
  
  DATA <- readxl::read_excel(data_path) %>%
    dplyr::rename_with(simple_name) %>%
    ensure_time_index()
  
  if (length(circ_vars) != 6L || length(prod_vars) != 7L) {
    stop("Incorrect number of circulation or production variables")
  }
  
  pairs_ss <- dplyr::bind_rows(
    tidyr::expand_grid(Y = prod_vars, X = circ_vars),
    tidyr::expand_grid(Y = circ_vars, X = prod_vars)
  )
  
  out_list_ss <- vector("list", nrow(pairs_ss))
  
  for (i in seq_len(nrow(pairs_ss))) {
    cat(sprintf("[BSTS %d/%d] %s <- %s\n", i, nrow(pairs_ss), pairs_ss$Y[i], pairs_ss$X[i]))
    
    result <- tryCatch(
      fit_pair_bsts_lfo_tuned(DATA, Y = pairs_ss$Y[i], X = pairs_ss$X[i]),
      error = function(e) {
        cat(sprintf("Error processing pair %s -> %s: %s\n", pairs_ss$X[i], pairs_ss$Y[i], e$message))
        NULL
      }
    )
    
    out_list_ss[[i]] <- result
  }
  
  summaries_ss <- purrr::map_dfr(out_list_ss, function(x) {
    if (is.null(x) || is.null(x$best_summary)) return(tibble::tibble())
    x$best_summary
  })
  
  if (nrow(summaries_ss) == 0) {
    warning("No valid results obtained from any pair")
    return(NULL)
  }
  
  rank_ss_all <- summaries_ss %>%
    dplyr::mutate(pair = paste0(.data$X, " -> ", .data$Y)) %>%
    dplyr::arrange(dplyr::desc(.data$support), dplyr::desc(.data$dELPD_mean), dplyr::desc(.data$dRMSE_mean)) %>%
    dplyr::mutate(pass_support = (.data$support >= support_min) & (.data$folds >= folds_min))
  
  winners_ss_070 <- rank_ss_all %>%
    dplyr::filter(.data$pass_support, .data$support >= sup_hi, .data$dELPD_mean > 0, .data$dRMSE_mean > 0) %>%
    dplyr::arrange(dplyr::desc(.data$support), dplyr::desc(.data$dELPD_mean), dplyr::desc(.data$dRMSE_mean))
  
  winners_ss_060 <- rank_ss_all %>%
    dplyr::filter(.data$pass_support, .data$support >= sup_lo, .data$dELPD_mean > 0, .data$dRMSE_mean > 0) %>%
    dplyr::arrange(dplyr::desc(.data$support), dplyr::desc(.data$dELPD_mean), dplyr::desc(.data$dRMSE_mean))
  
  if (!is.null(out_dir)) {
    f_all <- file.path(out_dir, "bsts_rank_all_pairs.csv")
    f_hi  <- file.path(out_dir, "bsts_winners_sup70.csv")
    f_lo  <- file.path(out_dir, "bsts_winners_sup60.csv")
    
    utils::write.csv(rank_ss_all, f_all, row.names = FALSE)
    utils::write.csv(winners_ss_070, f_hi, row.names = FALSE)
    utils::write.csv(winners_ss_060, f_lo, row.names = FALSE)
  }
  
  return(list(
    rank_ss_all = rank_ss_all,
    winners_ss_070 = winners_ss_070,
    winners_ss_060 = winners_ss_060,
    summaries_ss = summaries_ss
  ))
}
