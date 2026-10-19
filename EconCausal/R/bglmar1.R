#' Bayesian generalized linear model with AR(1) errors and leave-future-out validation
#'
#' Evaluates every directed pair between six circulation variables and seven production
#' variables, in both directions, by comparing a Bayesian regression of the response on a linear
#' time trend with and without lagged values of the predictor, both with first-order
#' autoregressive errors. Each comparison is repeated over successive forecast windows, and a
#' direction counts as supported in a window only when adding the lags improves both the
#' predictive density and the point forecast. Use it to rank directed relationships by how
#' consistently the predictor improves out-of-sample forecasts.
#'
#' @param data_path Character scalar. Path to an Excel file with a date column and the series
#'   named in `circ_vars` and `prod_vars`. Column names are cleaned before matching: a leading
#'   `as.numeric.`, trailing `.NEW.` or `.1.588.` and trailing dots are removed, and remaining
#'   dots become underscores. A date column named `Month` is used, or the first date-time
#'   column is renamed to `Month`.
#' @param circ_vars Character vector of exactly six circulation variable names, after cleaning.
#' @param prod_vars Character vector of exactly seven production variable names, after cleaning.
#' @param max_lag Integer scalar >= 1, default `3`. Number of lags of the predictor used as
#'   regressors.
#' @param initial_frac Numeric scalar in (0, 1), default `0.7`. Fraction of the complete
#'   observations that sizes the first training window; the size actually used is the larger of
#'   that fraction and `initial_min`.
#' @param initial_min Integer scalar >= 1, default `90`. Lower bound of the first training
#'   window.
#' @param test_h Integer scalar >= 1, default `12`. Number of observations in the test block of
#'   each validation window; one year with monthly data.
#' @param step_h Integer scalar >= 1, default `12`. Advance of the end of the training sample
#'   between windows.
#' @param lfo_window `"sliding"` (default) or `"expanding"`. With `"sliding"` the training
#'   sample keeps at most the initial number of rows and moves forward; with `"expanding"` it
#'   grows from the first observation.
#' @param chains Integer scalar >= 1, default `4`. Number of MCMC chains per model.
#' @param parallel_chains Integer scalar >= 1, default `4`. Number of chains run in parallel,
#'   passed as the number of cores of the sampler.
#' @param iter Integer scalar > `warmup`, default `1500`. Total iterations per chain, warmup
#'   included.
#' @param warmup Integer scalar >= 0 and < `iter`, default `750`. Warmup iterations per chain.
#' @param adapt_delta Numeric scalar in (0, 1), default `0.95`. Target acceptance rate of the
#'   sampler.
#' @param trees Integer scalar >= 1, default `12`. Maximum tree depth of the sampler.
#' @param seed Integer scalar, default `2025`. Base seed; the model without lags is fitted with
#'   `seed + 101` and the model with lags with `seed + 202`, in every window and for every pair.
#' @param support_min Numeric scalar, default `0.6`. Not used by the function; the support
#'   thresholds applied to the returned rankings are `sup_hi` and `sup_lo`.
#' @param folds_min Integer scalar >= 0, default `5`. Minimum number of evaluated windows
#'   required by `winners_070` and `winners_060`.
#' @param sup_hi Numeric scalar in \[0, 1\], default `0.7`. Support threshold of `winners_070`.
#' @param sup_lo Numeric scalar in \[0, 1\], default `0.6`. Support threshold of `winners_060`.
#' @param backend Character scalar, one of `"auto"` (default), `"rstan"` or `"cmdstanr"`. Engine
#'   used to fit the models. With `"auto"` the function uses `rstan` when it is installed and
#'   otherwise a working `cmdstanr`. The option `EconCausal.backend`, when set, overrides this
#'   argument, and the function stops when neither engine is available.
#'
#' @return A list with four elements. `bench_bayes` is a tibble with one row per direction and
#'   the columns `pair` (written as `"X -> Y"`), `folds`, `folds_pass`, `support`
#'   (`folds_pass / folds`), `ELPD_diff_mean`, `RMSE_diff_mean`, `RMSE_full_mean`,
#'   `RMSE_base_mean`, `MAE_full_mean`, `MAE_base_mean`, `sMAPE_full_mean`, `sMAPE_base_mean`,
#'   `R2_full_mean` and `R2_base_mean`, sorted by decreasing `support`, decreasing
#'   `ELPD_diff_mean` and increasing `RMSE_diff_mean`. `winners_070` and `winners_060` are the
#'   rows of that tibble with at least `folds_min` evaluated windows, finite support of at least
#'   `sup_hi` or `sup_lo`, positive `ELPD_diff_mean` and negative `RMSE_diff_mean`; their names
#'   do not change with `sup_hi` and `sup_lo`. `rank_out` is a list with `all`, the sorted
#'   benchmark, `winners_hi` and `winners_lo`, the two tibbles just described, and `ratios`,
#'   with `pair`, `support`, `ELPD_diff_mean`, `RMSE_diff_mean`, `RMSE_ratio` and `MAE_ratio`,
#'   sorted by increasing `RMSE_ratio`. A direction with too few observations, without any
#'   validation window or without any evaluated window returns a row with `folds` equal to zero
#'   and missing metrics.
#'
#' @details
#' For each direction the predictor is lagged `max_lag` times and incomplete rows are dropped. A
#' direction is skipped when the remaining rows are fewer than `initial_min + test_h + max_lag +
#' 5`. The first training sample holds the larger of `initial_min` rows and `initial_frac` of the
#' remaining rows, each test block holds the next `test_h` observations, and the end of the
#' training sample advances by `step_h` until the last observation is reached. In each window the
#' response, the time index and the lags are standardised with the training mean and standard
#' deviation, the response is returned to its original scale before the metrics are computed, and
#' a lag whose training standard deviation is zero is dropped from that window.
#'
#' Two models are fitted to the standardised response with Gaussian errors, a normal prior on the
#' regression coefficients, a Student t prior on the intercept and an exponential prior on the
#' residual scale: a model on the standardised time index alone and a model that adds the
#' standardised lags of the predictor. Both carry a first-order autoregressive term on a single
#' series indexed by time, so the comparison isolates the contribution of the lags. For the test
#' block the window records the expected log predictive density of each observation, obtained as
#' the log of the mean of the posterior likelihood draws and summed over the block, together with
#' RMSE, MAE, symmetric MAPE and the coefficient of determination of the posterior mean forecast.
#' A window is a win when the model with the lags has both a higher predictive density and a
#' lower RMSE, and `support` is the share of wins among the evaluated windows.
#'
#' @section Methodological notes:
#' `support` requires improvement in probabilistic fit and in point error at the same time; a
#' predictor that improves only one of them in a window does not count. The predictive density is
#' computed from the posterior likelihood draws of the test observations, with group-level terms
#' excluded, and not from a Gaussian approximation to the forecast. Standardising the response,
#' the time index and the lags with training statistics only keeps test-window information out of
#' the fit. The linear time trend enters both models, so a direction cannot be supported by the
#' trend itself. Windows in which the response does not vary, or in which a fit or a predictive
#' evaluation fails, are dropped and do not count in `folds`, so `support` is a proportion over
#' evaluated windows and not over possible ones. Both seeds are derived from `seed` and not from
#' the window or the pair, which makes a run reproducible for a given `seed`. The 84 directions
#' are ranked under a common criterion and without any correction for multiplicity: `support`
#' describes how consistent a single direction is across windows, and is not a joint test.
#'
#' @section Dependencies:
#' `brms` builds the formulas and the priors, fits the models, and produces the pointwise
#' likelihood and the posterior expectation of the test block; the fitting engine is `rstan` or
#' `cmdstanr`, which are suggested packages chosen at run time and not imported. `readxl` reads
#' the data; `dplyr`, `tidyr` and `tibble` build lags, windows and summaries; `magrittr` supplies
#' the pipe and `rlang` the data pronoun; `stats` provides the standard deviation, the Gaussian
#' family and the formula built for each window; `parallel` counts the available cores for the
#' duration of the call; and `utils` writes the ranking tables when the internal ranking helper
#' is given output paths, which the defaults do not do.
#'
#' @references
#' \enc{Bürkner}{Burkner}, P.-C. (2017). brms: An R package for Bayesian multilevel models using
#' Stan. *Journal of Statistical Software, 80*(1), 1\enc{–}{-}28.
#' \doi{10.18637/jss.v080.i01}
#'
#' \enc{Bürkner}{Burkner}, P.-C., Gabry, J., & Vehtari, A. (2020). Approximate leave-future-out
#' cross-validation for Bayesian time series models. *Journal of Statistical Computation and
#' Simulation, 90*(14), 2499\enc{–}{-}2523. \doi{10.1080/00949655.2020.1783262}
#'
#' @seealso [bsts_model()], [ecm_mars()]; the vignettes `bglmar1-eng` and `bglmar-esp`.
#'
#' @examples
#' \dontrun{
#' result <- bglmar1(
#'   data_path = file.path(tempdir(), "data.xlsx"),
#'   circ_vars = c("TC_SPOT_CAN_US", "TC_SPOT_US_CAN", "TC_SPOT_US_REMB",
#'                 "IPC", "TdI_LdelT", "TasaDescuento"),
#'   prod_vars = c("ValorExportaciones", "Real_Net_Profit",
#'                 "RealSocialConsumptionPerWorker2017", "RealWage_PPP2017",
#'                 "CapitalStock_PPP2017", "LaborProductivity_PPP2017",
#'                 "InvestmentPerWorker_PPP2017"),
#'   backend = "auto"
#' )
#' }
#'
#' @export
bglmar1 <- function(data_path, circ_vars, prod_vars, max_lag = 3, initial_frac = 0.7, 
                    initial_min = 90, test_h = 12, step_h = 12, lfo_window = "sliding",
                    chains = 4, parallel_chains = 4, iter = 1500, warmup = 750,
                    adapt_delta = 0.95, trees = 12, seed = 2025, support_min = 0.6,
                    folds_min = 5, sup_hi = 0.7, sup_lo = 0.6, backend = c("auto","rstan","cmdstanr")) {
  
  backend <- match.arg(backend)
  pick_backend <- function(pref = "auto") {
    opt <- getOption("EconCausal.backend", NA_character_)
    if (!is.na(opt)) pref <- opt
    
    if (identical(pref, "cmdstanr")) {
      if (requireNamespace("cmdstanr", quietly = TRUE)) {
        ver <- try(cmdstanr::cmdstan_version(error_on_NA = FALSE), silent = TRUE)
        if (!inherits(ver, "try-error") && !is.null(ver)) return("cmdstanr")
      }
      message("EconCausal: 'cmdstanr' not available; trying 'rstan'.")
      pref <- "rstan"
    }
    if (identical(pref, "rstan") || identical(pref, "auto")) {
      if (requireNamespace("rstan", quietly = TRUE)) return("rstan")
    }
    if (!identical(pref, "cmdstanr")) {
      if (requireNamespace("cmdstanr", quietly = TRUE)) {
        ver <- try(cmdstanr::cmdstan_version(error_on_NA = FALSE), silent = TRUE)
        if (!inherits(ver, "try-error") && !is.null(ver)) return("cmdstanr")
      }
    }
    stop("Neither 'rstan' nor 'cmdstanr' is available. Please install one of them to fit models.")
  }

  old_options <- options()
  on.exit(options(old_options), add = TRUE)
  options(mc.cores = parallel::detectCores())
  options(scipen = 0)
  
  DATA <- readxl::read_excel(data_path)

  simple_name <- function(nm) {
    nm <- gsub("^as\\.numeric\\.", "", nm)
    nm <- gsub("\\.NEW\\.$", "", nm)
    nm <- gsub("\\.1\\.588\\.$", "", nm)
    nm <- gsub("\\.+$", "", nm)
    nm <- gsub("\\.", "_", nm)
    nm
  }
  
  DATA <- DATA %>% dplyr::rename_with(simple_name)
  
  if (!"Month" %in% names(DATA)) {
    time_candidates <- names(Filter(function(x) inherits(x, c("POSIXct","POSIXt","Date")), DATA))
    if (length(time_candidates) == 0) stop("No temporal column found")
    names(DATA)[match(time_candidates[1], names(DATA))] <- "Month"
  }
  
  DATA <- DATA %>% dplyr::arrange(.data$Month) %>% dplyr::mutate(time_idx = dplyr::row_number())
  
  present <- setdiff(names(DATA), "Month")
  
  if (length(circ_vars) != 6L || length(prod_vars) != 7L) {
    stop("Incorrect number of circulation or production variables")
  }

  backend_used <- pick_backend(backend)

  pairs <- rbind(
    expand.grid(Y = prod_vars, X = circ_vars, stringsAsFactors = FALSE),
    expand.grid(Y = circ_vars, X = prod_vars, stringsAsFactors = FALSE)
  )
  
  make_lags <- function(x, L) {
    out <- as.data.frame(sapply(1:L, function(k) dplyr::lag(x, k)))
    names(out) <- paste0("X_l", 1:L)
    out
  }
  
  make_splits <- function(n, initial, test, step, window = c("expanding","sliding")){
    window <- match.arg(window)
    splits <- list()
    start_train <- 1L
    end_train   <- initial
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
      splits[[length(splits)+1L]] <- list(train=tr, test=te)
      end_train <- min(end_train + step, n - 1L)
    }
    splits
  }
  
  smape <- function(pred, obs) {
    ok <- is.finite(pred) & is.finite(obs) & (abs(pred)+abs(obs) > .Machine$double.eps)
    if (!any(ok)) return(NA_real_)
    mean( 2*abs(pred[ok]-obs[ok]) / (abs(pred[ok]) + abs(obs[ok])) ) * 100
  }
  
  safe_r2 <- function(pred, obs) {
    SST <- sum((obs - mean(obs))^2)
    if (!is.finite(SST) || SST <= .Machine$double.eps) return(NA_real_)
    1 - sum((pred-obs)^2)/SST
  }
  
  log_mean_exp <- function(v) { m <- max(v); m + log(mean(exp(v - m))) }
  
  df <- DATA %>%
    dplyr::arrange(.data$Month) %>%
    dplyr::mutate(t_index = seq_len(n()))
  
  pri <- c(
    brms::set_prior("normal(0, 1)",         class = "b"),
    brms::set_prior("student_t(3, 0, 2.5)", class = "Intercept"),
    brms::set_prior("exponential(1)",       class = "sigma")
  )
  
  res_list <- vector("list", nrow(pairs))
  
  for (pp in seq_len(nrow(pairs))) {
    Ynm <- pairs$Y[pp]
    Xnm <- pairs$X[pp]
    
    dat0 <- df %>%
      dplyr::select(.data$Month, .data$t_index, dplyr::all_of(c(Ynm, Xnm))) %>%
      dplyr::rename(Y = !!Ynm, X = !!Xnm) %>%
      dplyr::mutate(dplyr::across(c(.data$Y, .data$X), as.numeric))
    
    dat <- dat0 %>%
      dplyr::bind_cols(make_lags(dat0$X, max_lag)) %>%
      tidyr::drop_na()
    
    n <- nrow(dat)
    if (n < (initial_min + test_h + max_lag + 5)) {
      res_list[[pp]] <- tibble::tibble(
        pair = paste0(Xnm, " -> ", Ynm),
        folds = 0, folds_pass = 0, support = NA_real_,
        ELPD_diff_mean = NA_real_, RMSE_diff_mean = NA_real_,
        RMSE_full_mean = NA_real_, RMSE_base_mean = NA_real_,
        MAE_full_mean  = NA_real_, MAE_base_mean  = NA_real_,
        sMAPE_full_mean= NA_real_, sMAPE_base_mean= NA_real_,
        R2_full_mean   = NA_real_, R2_base_mean   = NA_real_
      )
      next
    }
    
    initial <- max(initial_min, floor(initial_frac * n))
    splits  <- make_splits(n, initial, test_h, step_h, window = lfo_window)
    
    if (length(splits) == 0L) {
      res_list[[pp]] <- tibble::tibble(
        pair = paste0(Xnm, " -> ", Ynm),
        folds = 0, folds_pass = 0, support = NA_real_,
        ELPD_diff_mean = NA_real_, RMSE_diff_mean = NA_real_,
        RMSE_full_mean = NA_real_, RMSE_base_mean = NA_real_,
        MAE_full_mean  = NA_real_, MAE_base_mean  = NA_real_,
        sMAPE_full_mean= NA_real_, sMAPE_base_mean= NA_real_,
        R2_full_mean   = NA_real_, R2_base_mean   = NA_real_
      )
      next
    }
    
    wins <- logical(0)
    elpd_diffs <- numeric(0)
    rmse_diffs <- numeric(0)
    
    rmse_full_v <- mae_full_v <- smape_full_v <- r2_full_v <- numeric(0)
    rmse_base_v <- mae_base_v <- smape_base_v <- r2_base_v <- numeric(0)
    
    for (ff in seq_along(splits)) {
      sp <- splits[[ff]]
      train <- dat[sp$train, , drop = FALSE]
      test  <- dat[sp$test,  , drop = FALSE]
      
      mu_y <- mean(train$Y); sd_y <- stats::sd(train$Y)
      if (!is.finite(sd_y) || sd_y <= .Machine$double.eps) {
        next
      }
      train$Y_s <- (train$Y - mu_y) / sd_y
      test$Y_s  <- (test$Y  - mu_y) / sd_y
      
      t_mu <- mean(train$t_index); t_sd <- stats::sd(train$t_index)
      if (!is.finite(t_sd) || t_sd <= .Machine$double.eps) t_sd <- 1
      train$t_s <- (train$t_index - t_mu) / t_sd
      test$t_s  <- (test$t_index  - t_mu) / t_sd
      
      x_lag_names <- paste0("X_l", 1:max_lag)
      for (nm in x_lag_names) {
        mu_x <- mean(train[[nm]], na.rm = TRUE)
        sd_x <- stats::sd(train[[nm]], na.rm = TRUE)
        if (!is.finite(sd_x) || sd_x <= .Machine$double.eps) {
          train[[nm]] <- NULL; test[[nm]] <- NULL
        } else {
          train[[nm]] <- (train[[nm]] - mu_x) / sd_x
          test[[nm]]  <- (test[[nm]]  - mu_x) / sd_x
        }
      }
      x_lag_used <- intersect(x_lag_names, names(train))
      
      train$series <- factor("one")
      test$series  <- factor("one")
      
      f_base <- brms::bf(
        Y_s ~ 1 + t_s,
        autocor = brms::cor_ar(~ t_index | series, p = 1)
      )
      rhs <- paste(c("1", "t_s", x_lag_used), collapse = " + ")
      f_full <- brms::bf(
        stats::as.formula(paste("Y_s ~", rhs)),
        autocor = brms::cor_ar(~ t_index | series, p = 1)
      )
      
      m_base <- tryCatch(
        brms::brm(
          formula = f_base, data = train, family = stats::gaussian(), prior = pri,
          chains = chains, iter = iter, warmup = warmup, seed = seed + 101,
          backend = backend_used, refresh = 50,
          cores = parallel_chains,
          control = list(adapt_delta = adapt_delta, max_treedepth = trees)
        ),
        error = function(e) NULL
      )
      m_full <- tryCatch(
        brms::brm(
          formula = f_full, data = train, family = stats::gaussian(), prior = pri,
          chains = chains, iter = iter, warmup = warmup, seed = seed + 202,
          backend = backend_used, refresh = 50,
          cores = parallel_chains,
          control = list(adapt_delta = adapt_delta, max_treedepth = trees)
        ),
        error = function(e) NULL
      )
      if (is.null(m_base) || is.null(m_full)) next
      
      ll_base <- tryCatch(brms::log_lik(m_base, newdata = test, re_formula = NA), error = function(e) NULL)
      ll_full <- tryCatch(brms::log_lik(m_full, newdata = test, re_formula = NA), error = function(e) NULL)
      if (is.null(ll_base) || is.null(ll_full)) {
        next
      }
      elpd_base <- sum(apply(ll_base, 2, log_mean_exp))
      elpd_full <- sum(apply(ll_full, 2, log_mean_exp))
      elpd_diff <- elpd_full - elpd_base
      
      ep_base <- brms::posterior_epred(m_base, newdata = test, re_formula = NA)
      ep_full <- brms::posterior_epred(m_full, newdata = test, re_formula = NA)
      yhat_base <- colMeans(ep_base) * sd_y + mu_y
      yhat_full <- colMeans(ep_full) * sd_y + mu_y
      
      obs <- test$Y
      rmse_base <- sqrt(mean((yhat_base - obs)^2))
      rmse_full <- sqrt(mean((yhat_full - obs)^2))
      mae_base  <- mean(abs(yhat_base - obs))
      mae_full  <- mean(abs(yhat_full - obs))
      sm_base   <- smape(yhat_base, obs)
      sm_full   <- smape(yhat_full, obs)
      r2_base   <- safe_r2(yhat_base, obs)
      r2_full   <- safe_r2(yhat_full, obs)
      
      win <- is.finite(elpd_diff) && (elpd_diff > 0) &&
        is.finite(rmse_base) && is.finite(rmse_full) && (rmse_full < rmse_base)
      
      wins        <- c(wins, win)
      elpd_diffs  <- c(elpd_diffs, elpd_diff)
      rmse_diffs  <- c(rmse_diffs, rmse_full - rmse_base)
      
      rmse_full_v <- c(rmse_full_v, rmse_full)
      rmse_base_v <- c(rmse_base_v, rmse_base)
      mae_full_v  <- c(mae_full_v,  mae_full)
      mae_base_v  <- c(mae_base_v,  mae_base)
      smape_full_v<- c(smape_full_v,sm_full)
      smape_base_v<- c(smape_base_v,sm_base)
      r2_full_v   <- c(r2_full_v,   r2_full)
      r2_base_v   <- c(r2_base_v,   r2_base)
    }
    
    folds <- length(wins)
    if (folds == 0L) {
      res_list[[pp]] <- tibble::tibble(
        pair = paste0(Xnm, " -> ", Ynm),
        folds = 0, folds_pass = 0, support = NA_real_,
        ELPD_diff_mean = NA_real_, RMSE_diff_mean = NA_real_,
        RMSE_full_mean = NA_real_, RMSE_base_mean = NA_real_,
        MAE_full_mean  = NA_real_, MAE_base_mean  = NA_real_,
        sMAPE_full_mean= NA_real_, sMAPE_base_mean= NA_real_,
        R2_full_mean   = NA_real_, R2_base_mean   = NA_real_
      )
    } else {
      folds_pass <- sum(wins, na.rm = TRUE)
      support    <- folds_pass / folds
      res_list[[pp]] <- tibble::tibble(
        pair = paste0(Xnm, " -> ", Ynm),
        folds = folds, folds_pass = folds_pass, support = support,
        ELPD_diff_mean = if (length(elpd_diffs)) mean(elpd_diffs, na.rm = TRUE) else NA_real_,
        RMSE_diff_mean = if (length(rmse_diffs)) mean(rmse_diffs, na.rm = TRUE) else NA_real_,
        RMSE_full_mean = if (length(rmse_full_v)) mean(rmse_full_v, na.rm = TRUE) else NA_real_,
        RMSE_base_mean = if (length(rmse_base_v)) mean(rmse_base_v, na.rm = TRUE) else NA_real_,
        MAE_full_mean  = if (length(mae_full_v))  mean(mae_full_v,  na.rm = TRUE) else NA_real_,
        MAE_base_mean  = if (length(mae_base_v))  mean(mae_base_v,  na.rm = TRUE) else NA_real_,
        sMAPE_full_mean= if (length(smape_full_v))mean(smape_full_v,na.rm = TRUE) else NA_real_,
        sMAPE_base_mean= if (length(smape_base_v))mean(smape_base_v,na.rm = TRUE) else NA_real_,
        R2_full_mean   = if (length(r2_full_v))   mean(r2_full_v,   na.rm = TRUE) else NA_real_,
        R2_base_mean   = if (length(r2_base_v))   mean(r2_base_v,   na.rm = TRUE) else NA_real_
      )
    }
  }
  
  bench_bayes <- dplyr::bind_rows(res_list) %>%
    dplyr::arrange(dplyr::desc(.data$support), dplyr::desc(.data$ELPD_diff_mean), .data$RMSE_diff_mean)
  
  rank_bglm_results <- function(bench = bench_bayes,
                                sup_hi = sup_hi, sup_lo = sup_lo,
                                min_folds = folds_min,
                                out_all = NULL,
                                out_hi  = NULL,
                                out_lo  = NULL) {
    
    top_all <- bench %>%
      dplyr::arrange(dplyr::desc(.data$support), dplyr::desc(.data$ELPD_diff_mean), .data$RMSE_diff_mean)
    
    winners_hi <- bench %>%
      dplyr::filter(.data$folds >= min_folds, is.finite(.data$support), .data$support >= sup_hi,
             .data$ELPD_diff_mean > 0, .data$RMSE_diff_mean < 0) %>%
      dplyr::arrange(dplyr::desc(.data$support), dplyr::desc(.data$ELPD_diff_mean), .data$RMSE_diff_mean)
    
    winners_lo <- bench %>%
      dplyr::filter(.data$folds >= min_folds, is.finite(.data$support), .data$support >= sup_lo,
             .data$ELPD_diff_mean > 0, .data$RMSE_diff_mean < 0) %>%
      dplyr::arrange(dplyr::desc(.data$support), dplyr::desc(.data$ELPD_diff_mean), .data$RMSE_diff_mean)
    
    bench_ratios <- bench %>%
      dplyr::mutate(
        RMSE_ratio = .data$RMSE_full_mean / .data$RMSE_base_mean,
        MAE_ratio  = .data$MAE_full_mean  / .data$MAE_base_mean
      ) %>%
      dplyr::arrange(.data$RMSE_ratio) %>%
      dplyr::select(.data$pair, .data$support, .data$ELPD_diff_mean, .data$RMSE_diff_mean, .data$RMSE_ratio, .data$MAE_ratio)
    
    if (!is.null(out_all)) utils::write.csv(top_all, out_all, row.names = FALSE)
    if (!is.null(out_hi)) utils::write.csv(winners_hi, out_hi, row.names = FALSE)
    if (!is.null(out_lo)) utils::write.csv(winners_lo, out_lo, row.names = FALSE)
    
    invisible(list(all = top_all, winners_hi = winners_hi, winners_lo = winners_lo, ratios = bench_ratios))
  }
  
  rank_out <- rank_bglm_results()
  
  return(list(
    bench_bayes = bench_bayes,
    winners_070 = rank_out$winners_hi,
    winners_060 = rank_out$winners_lo,
    rank_out = rank_out
  ))
}