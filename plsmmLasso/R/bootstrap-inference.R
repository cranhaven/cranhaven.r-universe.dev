sample_boot <- function(data, n_boot) {
  unique_series <- unique(data$series)
  n_series <- length(unique_series)

  bootstrap_samples <- lapply(1:n_boot, function(x) {
    sample_id <- sample(unique_series, n_series, replace = TRUE)
    mat_boot <- do.call(rbind, lapply(sample_id, function(j) data[data$series == j, ]))
    return(mat_boot)
  })

  return(bootstrap_samples)
}

create_bases_boot <- function(data, timexgroup = TRUE) {
  bases <- create_bases(data$t)$bases

  if (timexgroup) {
    n <- nrow(data)
    vec_group <- data$group
    ref_group <- vec_group[1]
    M <- ncol(bases)

    # Create a logical index indicating where vec_group is equal to ref_group
    index_ref_group <- vec_group == ref_group

    # Initialize bases_timexgroup matrix
    bases_timexgroup <- matrix(0, nrow = n, ncol = M * 2)

    # Fill the bases_timexgroup matrix using vectorized operations
    bases_timexgroup[index_ref_group, 1:M] <- bases[index_ref_group, ]
    bases_timexgroup[!index_ref_group, (M + 1):(2 * M)] <- bases[!index_ref_group, ]

    bases <- bases_timexgroup
  }


  return(bases)
}

fit_boot <- function(data, min_lambda) {
  boot_bases <- create_bases_boot(data)

  lambda_grid <- seq(1.2, min_lambda, -0.1) * sqrt(2 * log(ncol(boot_bases)) / length(data$y))

  cv_fit <- glmnet::cv.glmnet(boot_bases, data$y,
    alpha = 1,
    lambda = lambda_grid
  )

  final_fit <- glmnet::glmnet(boot_bases, data$y, alpha = 1, lambda = cv_fit$lambda.min)
  fitted_values <- glmnet::predict.glmnet(final_fit, newx = boot_bases, s = cv_fit$lambda.min)

  data$f_fit <- fitted_values

  out_f <- data[, c("t", "f_fit", "group")]

  out_f[out_f$group == 0, ]$f_fit <- out_f[out_f$group == 0, ]$f_fit - attr(scale(unique(out_f[out_f$group == 0, ]$f_fit),
    scale = FALSE
  ), "scaled:center")
  out_f[out_f$group == 1, ]$f_fit <- out_f[out_f$group == 1, ]$f_fit - attr(scale(unique(out_f[out_f$group == 1, ]$f_fit),
    scale = FALSE
  ), "scaled:center")
  
  t0 = sort(unique(out_f[out_f$group == 0, ]$t))
  t1 = sort(unique(out_f[out_f$group == 1, ]$t))

  if (length(t0) != length(t1)) {
    combined_t <- sort(unique(out_f$t))
    out_f <- pred_f(model = list(out_f = out_f, alpha = as.vector(final_fit$beta[, 1])),
                    data = data, t_seq = combined_t)
  } else if (!all(t0 == t1)) {
    combined_t <- sort(unique(out_f$t))
    out_f <- pred_f(model = list(out_f = out_f, alpha = as.vector(final_fit$beta[, 1])),
                    data = data, t_seq = combined_t)
  }
  
  return(list(out_f = out_f, alpha = as.vector(final_fit$beta[, 1])))
}

calc_f_diff <- function(out_f) {
  fit0 <- out_f[out_f$group == 0, ]
  fit0 <- fit0[!duplicated(fit0$t), ]
  fit0 <- fit0[order(fit0$t), ]

  fit1 <- out_f[out_f$group == 1, ]
  fit1 <- fit1[!duplicated(fit1$t), ]
  fit1 <- fit1[order(fit1$t), ]

  out <- data.frame(t = fit1$t, diff = fit0$f_fit - fit1$f_fit)
  return(out)
}

f_predict = function(t, coef, group, keep = NULL) {
  
  bases = create_bases(t, keep = keep)$bases
  # browser()
  if(length(coef) == ncol(bases) | is.null(group)) {
    return(bases %*% coef)
  } else {
    if(group == 0) {
      coef = coef[1:ncol(bases)]
    } else {
      coef = coef[(ncol(bases)+1):length(coef)]
    }
  }
  
  return(bases %*% coef)
  
}

pred_f <- function(model, data, t_seq) {
  selected_bases <- create_bases(data$t)$selected_bases

  # t_cont <- seq(min(data$t), max(data$t), by = byseq)
  t_obs <- sort(unique(data$t))

  df.F <- data.frame(
    c(t_seq, t_seq),
    c(f_predict(
      t = t_seq,
      coef = model$alpha, group = model$out_f$group[1],
      keep = selected_bases
    ) - mean(f_predict(
      t = t_obs,
      coef = model$alpha, group = model$out_f$group[1],
      keep = selected_bases
    )), f_predict(
      t = t_seq,
      coef = model$alpha, group = 1 - model$out_f$group[1],
      keep = selected_bases
    ) - mean(f_predict(
      t = t_obs,
      coef = model$alpha, group = 1 - model$out_f$group[1],
      keep = selected_bases
    ))),
    c(rep(0, length(t_seq)), rep(1, length(t_seq)))
  )

  colnames(df.F) <- c("t", "f_fit", "group")
  return(df.F)
}

create_CI <- function(list_diff_CI, data, min_lambda) {
  d_bar <- colMeans(do.call("rbind", lapply(list_diff_CI, function(x) {
    x$diff
  })))

  s_bar <- sqrt(colMeans(do.call("rbind", lapply(list_diff_CI, function(x) {
    (x$diff - d_bar)^2
  }))))

  M_b <- unlist(lapply(list_diff_CI, function(x) {
    max(abs(x$diff - d_bar) / s_bar)
  }))

  q_b <- stats::quantile(M_b, probs = 0.975)

  CI_low <- data.frame(t = list_diff_CI[[1]]$t, low = d_bar - q_b * s_bar)
  CI_up <- data.frame(t = list_diff_CI[[1]]$t, up = d_bar + q_b * s_bar)

  obs <- calc_f_diff(pred_f(fit_boot(data, min_lambda), data, 
                            t_seq = seq(min(data$t), max(data$t), by = 0.1)))

  
  CI_diff_f <- data.frame(obs, CI_low[, 2], CI_up[, 2])
  colnames(CI_diff_f) <- c("t", "f diff.", "Lower 95%", "Upper 95%")
  rownames(CI_diff_f) <- NULL

  return(CI_diff_f)
}

L2_test_f <- function(list_fitted_boot, plsmm_output) {
  df_list_fit <- lapply(seq_along(list_fitted_boot), function(i) {
    df <- list_fitted_boot[[i]]$out_f
    df$boot <- as.character(i)
    return(df)
  })
  
  diff_list <- lapply(df_list_fit, calc_f_diff)
  
  T_obs <- sum((calc_f_diff(plsmm_output$lasso_output$out_f)$diff)^2)
  T_boot <- sapply(diff_list, function(x) {
    sum(x$diff^2)
  })
  
  pvalue <- stats::pnorm(abs((2 * T_obs - mean(T_boot)) / sqrt(stats::var(T_boot))),
                  lower.tail = FALSE
  ) * 2
  
  overall_f <- data.frame(T_obs, pvalue)
  
  colnames(overall_f) <- c("T", "p-value")
  
  return(overall_f)
}


#' Bootstrap joint confidence bands and L2-norm based test on nonlinear functions 
#'
#' This function conducts a test of overall equality of two nonlinear functions
#' and generates confidence bands for the estimated difference of the nonlinear functions using a bootstrap method.
#'
#' @param x A matrix of predictors.
#' @param y A continuous vector of response variable.
#' @param series A variable representing different series or groups in the data modeled as a random intercept.
#' @param t A numeric vector indicating the time points.
#' @param name_group_var A character string specifying the name of the grouping variable.
#' @param plsmm_output Output object obtained from the \code{\link{plsmm_lasso}} function.
#' @param n_boot Numeric specifying the number of bootstrap samples (default is 1000).
#' @param predicted Logical indicating whether to plot predicted values. If \code{FALSE} only the observed time points are used.
#' @param show_obs Logical. If \code{TRUE} the observed time points are used for the position scale of the x-axis.
#' @param verbose Logical indicating whether to display bootstrap progress. Default is \code{TRUE}.
#'
#' @return 
#' A plot showing the estimated difference and confidence bands of the nonlinear functions.
#' 
#' A list containing:
#'   \item{overall_test_results}{Results from the L2-norm test of equality.}
#'   \item{CI_f}{Confidence intervals values for the difference of the estimated functions used for plotting.}
#'
#' @details
#' The function generate bootstrap samples and estimate the nonlinear functions for each \code{n_boot} sample. 
#' These bootstrap estimates are then used to compute the L2-norm test of equality and the joint confidence bands.
#'
#' @examples
#' 
#' set.seed(123)
#' data_sim <- simulate_group_inter(
#'   N = 50, n_mvnorm = 3, grouped = TRUE,
#'   timepoints = 3:5, nonpara_inter = TRUE,
#'   sample_from = seq(0, 52, 13), 
#'   cos = FALSE, A_vec = c(1, 1.5)
#' )
#' sim <- data_sim$sim
#' x <- as.matrix(sim[, -1:-3])
#' y <- sim$y
#' series <- sim$series
#' t <- sim$t
#' bases <- create_bases(t)
#' lambda <- 0.0046
#' gamma <- 0.00000001
#' plsmm_output <- plsmm_lasso(x, y, series, t,
#'   name_group_var = "group", bases$bases,
#'   gamma = gamma, lambda = lambda, timexgroup = TRUE,
#'   criterion = "BIC"
#' )
# Note: For illustration purposes only, set n_boot to at least 1000 for reliable results.
#'test_f_results <- test_f(x, y, series, t,
#'  name_group_var = "group", plsmm_output,
#'  n_boot = 10
#')
#' test_f_results[[1]]
#' test_f_results[[2]]
#' 
#'
#' @importFrom rlang .data
#' @export
test_f <- function(x, y, series, t, name_group_var, plsmm_output, n_boot = 1000,
                   predicted = FALSE, show_obs = FALSE, verbose = TRUE) {

  f0 <- plsmm_output$lasso_output$out_f[plsmm_output$lasso_output$out_f$group == 0, ]
  f0 <- f0[!duplicated(f0$t), ]
  f0 <- f0[order(f0$t), ]

  f1 <- plsmm_output$lasso_output$out_f[plsmm_output$lasso_output$out_f$group == 1, ]
  f1 <- f1[!duplicated(f1$t), ]
  f1 <- f1[order(f1$t), ]

  if (identical(f1$f_fit, f0$f_fit)) {
    stop("The nonlinear functions are equal in the plsmm_output. The test is irrelevant in this case. Try running plsmm_lasso with timexgroup = TRUE")
  }
  
  y <- y - plsmm_output$lasso_output$x_fit - rep(plsmm_output$out_phi$phi, plsmm_output$ni)

  t_obs <- sort(unique(t))

  data <- data.frame(y, series, t, x[, name_group_var])
  colnames(data)[4] <- "group"

  samples <- sample_boot(data = data, n_boot = n_boot)

  pb <- utils::txtProgressBar(min = 0, max = length(samples), style = 3)

  min_lambda <- scalreg::scalreg(scale(create_bases_boot(data)), scale(data$y))$hsigma

  fitted_boot <- vector("list", n_boot)

  for (k in 1:n_boot) {
    fitted_boot[[k]] <- fit_boot(data = samples[[k]], min_lambda = min_lambda)

    if(verbose) {
      utils::setTxtProgressBar(pb, k)
    }

  }

  if(verbose) {
    message("\nCompleted fitting Bootstrap samples. Now formatting results, and generating figure.\n")
  }

  overall_test_results <- L2_test_f(
    list_fitted_boot = fitted_boot,
    plsmm_output = plsmm_output
  )

  predicted_f <- lapply(1:length(fitted_boot), function(i) {
    pred_f(model = fitted_boot[[i]], data = samples[[i]], 
           t_seq = seq(min(samples[[i]]$t), max(samples[[i]]$t), by = 0.1))
  })


  diff_predicted_f <- lapply(predicted_f, calc_f_diff)
  CI_f <- create_CI(diff_predicted_f, data, min_lambda)

  if (predicted) {
    plot_CI <- ggplot2::ggplot(CI_f, ggplot2::aes(x = t, y = .data$`f diff.`)) +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
      ggplot2::geom_ribbon(ggplot2::aes(x = t, ymin = .data$`Lower 95%`, 
                                        ymax = .data$`Upper 95%`),
        data = CI_f,
        fill = "gray", alpha = 0.6
      ) +
      ggplot2::geom_line(linewidth = 0.7) +
      ggplot2::geom_point(
        ggplot2::aes(x = t, y = .data$`f diff.`),
        CI_f[CI_f$t %in% t_obs, ]
      ) 
  } else {
    CI_obs_f <- CI_f[CI_f$t %in% t_obs, ]

    plot_CI <- ggplot2::ggplot(CI_obs_f, ggplot2::aes(x = t, y = .data$`f diff.`)) +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
      ggplot2::geom_ribbon(ggplot2::aes(x = t, ymin = .data$`Lower 95%`,
                                        ymax = .data$`Upper 95%`),
        data = CI_obs_f,
        fill = "gray", alpha = 0.6
      ) +
      ggplot2::geom_line(linewidth = 0.7) +
      ggplot2::geom_point()
  }
  
  if(show_obs) {
    plot_CI = plot_CI +
      ggplot2::scale_x_continuous(breaks = t_obs)
  }
  
  print(plot_CI)
  return(list(
    overall_test_results = overall_test_results, CI_f = CI_f
  ))
}


