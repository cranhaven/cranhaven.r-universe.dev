#' @name fit_incidence
#' @title Fit incidence curve to reported data
#' 
#' @description 
#' This is a function that fits an incidence curve to a set of reported cases and delay distribution
#' using an empirical Bayes estimation method, which fits parameters for a spline basis. All hyper 
#' parameter tuning and data processing are done within this function.
#'
#' @param reported An integer vector of reported cases.
#' @param delay_dist A positive vector that sums to one, which describes the delay distribution.
#' @param dof_grid An integer vector of degrees of freedom for the spline basis.
#' @param lam_grid A vector of regularization strengths to scan.
#' @param regularization_order An integer (typically 0, 1, 2), indicating differencing order for L2 
#'        regularization of spline parameters. Default is 2 for second derivative penalty.
#' @param dof_method Metric to choose "best" spline degrees of freedom: 'aic': Akaike information 
#'         criterion, 'bic': Bayesian information criterion, 'val': validation likelihood.
#' @param lam_method metric to choose "best" regularization strength lambda: 'aic': Akaike information 
#'         criterion, 'bic': Bayesian information criterion, 'val': validation likelihood.
#' @param percent_thresh If using validation likelihood to select best,
#'        the largest (strongest) lambda that is within `percent_thresh` of
#'        the highest validation lambda will be selected. Default is 2. Must be greater than 0.
#' @param num_ar_steps An integer number of AR steps after last observation.
#' @param num_ar_samps An integer number of AR samples.
#' @param seed Seed for RNG.
#' @param linear_tail An integer number of days used to fit linear model on tail to be used as a mean
#'        for AR extrapolation.
#' @param front_pad_size An integer for initial number of 0's before first observation.
#' @param extrapolation_prior_precision A positive scalar for extrapolation slope shrinkage prior precision.
#' @param frac_train A numeric between 0 and 1 for fraction of data used to train lambda validation.
#' @param num_samps_per_ar An integer for the number of Laplace samples per AR fit.
#' @param val_restarts An integer for the number of times to refit hyperparameters if 'val' is used for 
#'        either. Set to 1 for faster but more unstable fits.
#' @param fisher_approx_cov A flag to use either the Fisher Information (TRUE) or the Hessian (FALSE) to approx posterior covariance over parameters.
#' @param end_pad_size And integer number of steps the spline is defined beyond the final 
#'        observation.
#' @return A list with the following entries: \itemize{
#'         \item Isamps -- sample of the incidence curve from a Laplace approximation per AR sample;
#'         \item Ihat -- MAP incidence curve estimate;
#'         \item Chat -- expected cases given MAP incidence curve estimate;
#'         \item beta_hats -- matrix of beta's per AR sample;
#'         \item best_dof -- best degrees of freedom from tuning;
#'         \item best_lambda -- best regularization parameter from tuning; and
#'         \item reported -- a copy of reported values used for fitting.}
#' @examples
#' \donttest{
#'	indiana_model <- fit_incidence(
#'                   reported = spanish_flu$Indiana, 
#'                   delay_dist = spanish_flu_delay_dist$proportion) }
#' @export fit_incidence
fit_incidence <- function(
  reported,
  delay_dist,
  dof_grid = seq(6, 20, 2),
  dof_method = "aic",
  lam_grid = 10**(seq(-1,-8, length.out = 20)),
  lam_method = "val",
  percent_thresh = 2,
  regularization_order = 2,
  num_ar_steps = 10,
  num_ar_samps = 100,
  linear_tail = 14,
  front_pad_size = 10,
  extrapolation_prior_precision = 10,
  frac_train = .75,
  fisher_approx_cov=TRUE,
  end_pad_size  = 50,
  num_samps_per_ar = 10,
  val_restarts = 2,
  seed = 1) {

  # Data check
  processed_data_list <- data_processing(
    reported = reported,
    delay_dist = delay_dist,
    num_ar_steps = num_ar_steps,
    num_ar_samps = num_ar_samps,
    seed = seed,
    linear_tail = linear_tail,
    front_pad_size = front_pad_size,
    extrapolation_prior_precision = extrapolation_prior_precision
  )
  reported_matrix <- processed_data_list$extrap
  original_data_logical <- processed_data_list$original
  reported_ar_median <- round(apply(reported_matrix, 2, stats::median))
  
  # Parameter check
  if (any(lam_grid <= 0)) {
    stop("All values in lambda grid must be greater than 0.")
  }
  if ((percent_thresh >= 50)||(percent_thresh <= 0)) {
    stop("percent_thresh must have a value in range [1, 49].")
  }
  if (any(dof_grid < 4)) {
    stop("All degrees of freedom need to be at least 4.")
  }

  # Run spline model
  best_dofs = c()
  val_restarts_dof = val_restarts
  if (dof_method != "val") val_restarts_dof = 1
  # Scan to get dof
  for(i in 1:val_restarts_dof) {
    reported_dof = reported_ar_median
    reported_val = NULL
    if (dof_method == "val") {
      train_val_list = train_val_split(
        reported = reported_ar_median,
        frac_train = frac_train
      )
      reported_dof = train_val_list$reported_train
      reported_val = train_val_list$reported_val
    }
    dof_list = scan_spline_dof(
      reported = reported_dof,
      delay_dist = delay_dist,
      dof_grid = dof_grid,
      method = dof_method,
      lam=1/sum(reported_ar_median),
      regularization_order = regularization_order,
      reported_val = reported_val,
      end_pad_size = end_pad_size,
      fisher_approx_cov = fisher_approx_cov)
    best_dofs = c(best_dofs, dof_list$best_dof)
  }
  best_dofs = sort(best_dofs)
  best_dof = best_dofs[ceiling(length(best_dofs)/2)]

  # Scan over multiple folds to get lambda
  best_lams = c()
  val_restarts_lam = val_restarts
  if (lam_method != "val") val_restarts_lam = 1
  for(i in 1:val_restarts_lam){
      # train/val split (TODO -- make true partition)
      reported_lam = reported_ar_median
      reported_val = NULL
      if (lam_method == "val") {
        train_val_list = train_val_split(
          reported = reported_ar_median,
          frac_train = frac_train
        )
        reported_lam = train_val_list$reported_train
        reported_val = train_val_list$reported_val
      }

      lambda_list = scan_spline_lam(
        reported = reported_lam,
        delay_dist = delay_dist,
        lam_grid = lam_grid,
        method = lam_method,
        percent_thresh=percent_thresh,
        dof = best_dof,
        regularization_order = regularization_order,
        reported_val = reported_val,
        end_pad_size = end_pad_size,
        fisher_approx_cov=fisher_approx_cov)
      best_lams = c(best_lams, lambda_list$best_lam)

  }
  best_lam = mean(best_lams)

  # Data preparation for output

  # Loop through AR samples to get posterior samples
  Isamps_list = list()
  Chat_list = list()
  Ihat_list = list()
  beta_list = list()
  beta0 = init_params(best_dof)
  for (idx in 1:num_ar_samps) {
    ar_output = train_and_validate(
      reported = reported_matrix[idx,],
      delay_dist = delay_dist,
      lam = best_lam,
      dof = best_dof,
      regularization_order = regularization_order,
      beta0 = beta0,
      end_pad_size = end_pad_size,
      fisher_approx_cov=fisher_approx_cov,
      num_samps_per_ar = num_samps_per_ar
    )
    beta0 = ar_output$beta_hat
    Isamps_list[[idx]] = ar_output$Isamps
    Ihat_list[[idx]] = ar_output$Ihat
    Chat_list[[idx]] = ar_output$Chat
    beta_list[[idx]] = ar_output$beta_hat
  }
  spline_list = list(
    Isamps = do.call("rbind", Isamps_list),
    Chat = apply(do.call("rbind", Chat_list), 2, stats::median),
    Ihat = apply(do.call("rbind", Ihat_list), 2, stats::median),
    beta_hats = do.call("rbind", beta_list)
  )

  # Subset to original dates
  output_spline_list = list()
  if (any(class(spline_list$Isamps) =="matrix")) {
    output_spline_list$Isamps = spline_list$Isamps[,original_data_logical]
  } else if (any(class(spline_list$Isamps) == "numeric")) {
    output_spline_list$Isamps = spline_list$Isamps[original_data_logical]
  }
  output_spline_list$Ihat = spline_list$Ihat[original_data_logical]
  output_spline_list$Chat = spline_list$Chat[original_data_logical]
  output_spline_list$beta_hats = spline_list$beta_hats
  output_spline_list$best_dof = best_dof
  output_spline_list$best_lambda = best_lam
  output_spline_list$reported = reported
  class(output_spline_list) = "incidence_spline_model"
  return(output_spline_list)
}


#' @name incidence_to_df
#' 
#' @title Export incidence model to data frame
#' 
#' @description 
#' Export the output of \code{\link{fit_incidence}} to a data frame with an optional
#' addition of a time index.
#' 
#' @param x An "incidence_spline_model" output from \code{\link{fit_incidence}}.
#' @param times An optional vector of time indices.
#' @param low_quantile A scalar that specifies the low quantile value for the output CI.
#' @param high_quantile A scalar that specifies the high quantile value for the output CI.
#' 
#' @return A data frame with the following entries: \itemize{
#'         \item Time -- a time index; if `ts` is `NULL` it is the observation number;
#'         \item Reported -- the value of `reported`;
#'         \item Ihat -- MAP incidence curve estimate;
#'         \item Chat -- expected cases given MAP incidence curve estimate;
#'         \item LowCI  -- lower pointwise credible interval bands around the incidence curve; and
#'         \item HighCI  -- higher pointwise credible interval bands around the incidence curve.}
#' 
#' @examples
#' \donttest{
#'	indiana_model <- fit_incidence(
#'                   reported = spanish_flu$Indiana, 
#'                   delay_dist = spanish_flu_delay_dist$proportion)
#'  indiana_df <- incidence_to_df(indiana_model, times = spanish_flu$Date)
#' }
#' @export
incidence_to_df <- function(
  x,
  times = NULL,
  low_quantile = .05,
  high_quantile = .95
) {
  if (is.null(times)) {
    times <- 1:length(x$Chat)
  } 
  df <- data.frame(
    Time = times,
    Reported = x$reported,
    Ihat = x$Ihat,
    Chat = x$Chat,
    LowCI = apply(x$Isamps, 2, function(x) stats::quantile(x, low_quantile)),
    HighCI = apply(x$Isamps, 2, function(x) stats::quantile(x, high_quantile))
  )
  return(df)
}

#' Plot model from fit_incidence
#' 
#' @description 
#' Plot time, reported cases, incidence curve with credible interval, and implied case curve.
#' 
#' @inheritParams incidence_to_df
#' @param ... Other parameters that can be included: \itemize{
#'        \item `times`: an optional vector of time indices.
#'        \item `plot_Chat`: a logical for whether Chat should be plotted. 
#'        \item `plot_reported`: a logical for whether reported cases should be plotted.
#'        \item `plot_CI`: a logical for whether CI should be plotted.
#'        } 
#' @examples
#' \donttest{
#'	indiana_model <- fit_incidence(
#'                   reported = spanish_flu$Indiana, 
#'                   delay_dist = spanish_flu_delay_dist$proportion)
#'  plot(indiana_model, times = spanish_flu$Date)
#' }
#' @export
plot.incidence_spline_model <- function(
  x, ...
) {
  dots <- list(...)
  if(is.null(dots$low_quantile)) {
  	dots$low_quantile = 0.05
  }
  if(is.null(dots$high_quantile)) {
  	dots$high_quantile = 0.95
  }
  if (is.null(dots$plot_Chat)) {
  	dots$plot_Chat = TRUE
  }
  if (is.null(dots$plot_reported)) {
  	dots$plot_reported = TRUE
  }
  if (is.null(dots$plot_CI)) {
  	dots$plot_CI = TRUE
  }
  
  if (is.null(dots$times)) {
    dots$times <- 1:length(x$Chat)
  } 
  data_subset <- data.frame(
    Time = dots$times,
    Reported = x$reported,
    Ihat = x$Ihat,
    Chat = x$Chat,
    LowCI = apply(x$Isamps, 2, function(x) stats::quantile(x, dots$low_quantile)),
    HighCI = apply(x$Isamps, 2, function(x) stats::quantile(x, dots$high_quantile)),
    shape = "shape"
  )
  with(data_subset, {
      p <- ggplot2::ggplot(data_subset, ggplot2::aes(x = Time, y = Ihat)) +
        ggplot2::geom_line(ggplot2::aes(x = Time, y = Ihat, color = "black")) +
        ggplot2::ggtitle("Fitted Incidence") + ggplot2::ylab("Cases")
      line_breaks = c("black")
      line_labels = c("Incidence Estimate (Ihat)")
      if (dots$plot_Chat) {
        p <- p + ggplot2::geom_line(ggplot2::aes(x = Time, y = Chat, color = "blue"))
        line_breaks = c(line_breaks, "blue")
        line_labels = c(line_labels, "Convolution Fit (Chat)")
      }
      p <- p + ggplot2::scale_color_identity(guide = "legend", name = "Model Fits", breaks = line_breaks, labels = line_labels)
      

      if (dots$plot_reported) {
        p <- p + ggplot2::geom_point(
          ggplot2::aes(x = Time, y = Reported), color = "coral2", shape = 3)
          
      }
      if (dots$plot_CI) {
        p <- p + ggplot2::geom_ribbon(ggplot2::aes(ymin = LowCI, ymax = HighCI), color = NA, fill = "cornflowerblue", alpha = 0.2)
      }
      p <- p + ggplot2::theme(legend.position = "bottom")
      print(p)
  })
}
