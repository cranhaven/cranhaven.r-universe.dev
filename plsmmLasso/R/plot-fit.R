#' Visualization of estimated mean trajectories and nonlinear functions from a PLSMM
#'
#' This function plots the observed data, the estimated mean trajectories, and
#' the estimated nonlinear functions from the output of \code{\link{plsmm_lasso}}.
#'
#' @param x A matrix of predictors.
#' @param y A continuous vector of response variable.
#' @param series A variable representing different series or groups in the data modeled as a random intercept.
#' @param t A numeric vector indicating the time points.
#' @param name_group_var A character string specifying the name of the grouping variable.
#' @param plsmm_output Output object obtained from the \code{\link{plsmm_lasso}} function.
#' @param predicted Logical indicating whether to plot predicted values. If \code{FALSE} only the observed time points are used.
#' @param show_obs Logical. If \code{TRUE} the observed time points are used for the position scale of the x-axis.
#'
#' @return Two plots:
#'   - The first plot shows the observed data and the estimated mean trajectories.
#'   - The second plot shows the estimated nonlinear functions.
#'
#' @details
#' If \code{predicted} is \code{TRUE} the function uses the model from \code{plsmm_output} to predict unobserved time points on a continuous grid of time.
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
#' plot_fit(x, y, series, t, name_group_var = "group", plsmm_output)
#'
#' @importFrom rlang .data
#' @export
plot_fit <- function(x, y, series, t, name_group_var,
                     plsmm_output, predicted = FALSE, show_obs = FALSE) {
  data <- data.frame(y, series, t, x)

  data$f_fit <- plsmm_output$lasso_output$out_f$f_fit
  data$x_fit <- plsmm_output$lasso_output$x_fit
  data$phi <- rep(plsmm_output$out_phi$phi, table(data$series))

  bases_functions <- create_bases(t)

  t_obs <- sort(unique(t))

  t_cont <- seq(min(t_obs), max(t_obs), by = 0.1)

  predicted_f <- data.frame(
    c(t_cont, t_cont),
    c(f_predict(
      t = t_cont,
      coef = plsmm_output$lasso_output$alpha, group = plsmm_output$lasso_output$out_f$group[1],
      keep = bases_functions$selected_bases
    ) - mean(f_predict(
      t = t_obs,
      coef = plsmm_output$lasso_output$alpha, group = plsmm_output$lasso_output$out_f$group[1],
      keep = bases_functions$selected_bases
    )), f_predict(
      t = t_cont,
      coef = plsmm_output$lasso_output$alpha, group = 1 - plsmm_output$lasso_output$out_f$group[1],
      keep = bases_functions$selected_bases
    ) - mean(f_predict(
      t = t_obs,
      coef = plsmm_output$lasso_output$alpha, group = 1 - plsmm_output$lasso_output$out_f$group[1],
      keep = bases_functions$selected_bases
    ))),
    c(rep(0, length(t_cont)), rep(1, length(t_cont)))
  )

  colnames(predicted_f) <- c("t", "f_cont", "group")

  means <- stats::aggregate(cbind(phi, x_fit) ~ .,
    data = data[, c("phi", "x_fit", name_group_var)],
    FUN = mean
  )
  if(is.null(name_group_var)) {
    names(means) <- c("phi", "x_fit")
    predicted_f$mean_trajectories <- predicted_f$f_cont + means$x_fit + means$phi
  } else {
    names(means) <- c("group", "phi", "x_fit")
    predicted_f <- merge(predicted_f, means, by = "group")
    predicted_f$mean_trajectories <- predicted_f$f_cont + predicted_f$x_fit + predicted_f$phi
  }

  obs_f <- predicted_f[predicted_f$t %in% t_obs, ]

  p <- ggplot2::ggplot(data = data, ggplot2::aes(x = t, y = y))

  if (predicted) {
    p.F.overall <- p + ggplot2::geom_line(ggplot2::aes(x = t, y = y, group = series)) +
      ggplot2::geom_line(ggplot2::aes(x = t, y = .data$mean_trajectories),
        data = predicted_f, size = 1,
        col = "red"
      ) + ggplot2::geom_point(ggplot2::aes(x = t, y = .data$mean_trajectories),
        data = obs_f, size = 2,
        col = "red"
      ) 

    p.F <- ggplot2::ggplot(ggplot2::aes(x = t, y = .data$f_cont), data = predicted_f) +
      ggplot2::geom_line(size = 1, col = "red") +
      ggplot2::geom_point(ggplot2::aes(x = t, y = .data$f_cont),
        data = obs_f, size = 2, col = "red"
      ) 
    
    if(!is.null(name_group_var)) {
      p.F.overall = p.F.overall + ggplot2::facet_grid(. ~ group)
      p.F = p.F + ggplot2::facet_grid(. ~ group)
      }
    
  } else {
    p.F.overall <- p + ggplot2::geom_line(ggplot2::aes(x = t, y = y, group = series)) +
      ggplot2::geom_line(ggplot2::aes(x = t, y = .data$mean_trajectories),
        data = obs_f, size = 1,
        col = "red"
      ) +
      ggplot2::geom_point(ggplot2::aes(x = t, y = .data$mean_trajectories),
        data = obs_f, size = 2, col = "red"
      ) 

    p.F <- ggplot2::ggplot(ggplot2::aes(x = t, y = .data$f_cont), data = obs_f) +
      ggplot2::geom_line(size = 1, col = "red") +
      ggplot2::geom_point(size = 2, col = "red")
    
    if(!is.null(name_group_var)) {
      p.F.overall = p.F.overall + ggplot2::facet_grid(. ~ group)
      p.F = p.F + ggplot2::facet_grid(. ~ group)
    }
  }

  if(show_obs) {
    p.F.overall = p.F.overall +
      ggplot2::scale_x_continuous(breaks = t_obs)
    p.F = p.F +
      ggplot2::scale_x_continuous(breaks = t_obs)
  }
  print(p.F.overall)
  print(p.F)
}
