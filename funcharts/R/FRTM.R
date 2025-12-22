
# Simulate data -----------------------------------------------------------
#' @title Simulate data for real-time monitoring of univariate functional data
#' @description Generate synthetic data as in the simulation study of Centofanti et al. (2024).
#' @param n_obs Number of curves generated.
#' @param scenario A  character string indicating the scenario considered. It could be "1",  and "2".
#' @param shift A  character string indicating the shift considered.
#' It could be "IC",  in-control data, "OC_h", Shift A (Phase),"OC_x", Shift B (Amplitude) and "OC_xh", Shift C (Amplitude and Phase).
#' @param alignemnt_level A  character string indicating the alignment level considered. It could be "M1", "M2",  and "M3".
#' @param t_out_type If "0.3", change point at the 30% of the process.  If "0.6", change point at the 60% of the process.
#' @param severity Severity level.
#' @param grid Grid of evaluation points.
#'
#' @references
#' Centofanti, F., A. Lepore, M. Kulahci, and M. P. Spooner (2024).
#' Real-time monitoring of functional data. \emph{Journal of Quality Technology}, 57(2):135--152,
#' doi:https://doi.org/10.1080/00224065.2024.2430978.
#'
#' @return   A list containing the following arguments:
#'
#'  \code{x_err}: A list containing the discrete observations for each curve.
#'
#'  \code{grid_i}:  A list of vector of time points where the curves are sampled.
#'
#'  \code{h}: A list containing the discrete observations of the warping function  for each curve.
#'
#'  \code{template}: The discrete observations of the true template function.
#'
#'  \code{grid_template}: Time points where the template is sampled.
#'
#'  \code{x_true}: A list containing the discrete observations of the amplitude function for each curve.
#'
#'  \code{grid}: Grid of evaluation points.
#'
#'  \code{out_control_t}: Time of the change point.
#' @export
#' @examples
#' library(funcharts)
#' data<-simulate_data_FRTM(n_obs=20)
simulate_data_FRTM <-
  function(n_obs = 100,
           scenario = "1",
           shift = "IC",
           alignemnt_level = "M1",
           t_out_type = "0.3",
           severity = 0.5,
           grid = seq(0, 1, length.out = 100)) {
    if (alignemnt_level == "M1")
      frac <- 1
    if (alignemnt_level == "M2")
      frac <- 4
    if (alignemnt_level == "M3")
      frac <- 8

    var_e <- 0.2 ^ 2
    var_b <- (0.2 / frac) ^ 2
    var_a  <-  0.15 ^ 2
    var_tend  <-  (0.25 / frac) ^ 2
    var_es  <-  (0.01 / frac) ^ 2


    length_grid <- length(grid)
    if ((scenario == "1" | scenario == "2") & shift == "IC") {
      multi_h <- 0
      multi_delta_end <- 0
      multi_x <- 0
      t_out <- NULL
    }
    else if (scenario == "1" & shift == "OC_h" & t_out_type == "0.3") {
      multi_h <- 1
      multi_delta_end <- 2
      multi_x <- 0
      t_out <- 0.3
    }
    else if (scenario == "2" & shift == "OC_h" & t_out_type == "0.3") {
      multi_h <- 1.5
      multi_delta_end <- 2
      multi_x <- 0
      t_out <- 0.3
    }
    else if (scenario == "1" & shift == "OC_h" & t_out_type == "0.6") {
      multi_h <- 1.5
      multi_delta_end <- 4
      multi_x <- 0
      t_out <- 0.6
    }
    else if (scenario == "2" & shift == "OC_h" & t_out_type == "0.6") {
      multi_h <- 2
      multi_delta_end <- 4
      multi_x <- 0
      t_out <- 0.6
    }
    else if (shift == "OC_x" & t_out_type == "0.3") {
      multi_h <- 0
      multi_delta_end <- 0
      multi_x <- 20
      t_out <- 0.3
    }
    else if (shift == "OC_x" & t_out_type == "0.6") {
      multi_h <- 0
      multi_delta_end <- 0
      multi_x <- 30
      t_out <- 0.6
    }
    else if (scenario == "1" & shift == "OC_xh" & t_out_type == "0.3") {
      multi_h <- 0.5
      multi_delta_end <- 1
      multi_x <- 10
      t_out <- 0.3
    }
    else if (scenario == "2" & shift == "OC_xh" & t_out_type == "0.3") {
      multi_h <- 0.75
      multi_delta_end <- 1
      multi_x <- 10
      t_out <- 0.3
    }
    else if (scenario == "1" & shift == "OC_xh" & t_out_type == "0.6") {
      multi_h <- 0.75
      multi_delta_end <- 2
      multi_x <- 15
      t_out <- 0.6
    }
    else if (scenario == "2" & shift == "OC_xh" & t_out_type == "0.6") {
      multi_h <- 1
      multi_delta_end <- 2
      multi_x <- 15
      t_out <- 0.6
    }
    else{
      multi_h <- 0
      multi_delta_end <- 0
      multi_x <- 0
      t_out <- NULL
    }
    endd <- 10
    end_out <- multi_delta_end * severity
    par_och <- multi_h * severity
    par_ocx <- multi_x * severity
    if (scenario == "1") {
      a_i <- stats::rnorm(n_obs, 1, sqrt(var_a))
      b_in <- stats::rnorm(n_obs, 0, sqrt(var_b))
      if (is.null(t_out))
        pp <- function(b, t)
          b * (2 * t - 1) + 1
      else
        pp <-
        function(b, t)
          b * (2 * t - 1) + 1 - par_och + (2 * (par_och - par_och * t_out) / (1 -
                                                                                t_out ^ 2)) * t_out
      pp <- function(b, t)
        b * (2 * t - 1) + 1
      min <-
        which.min(abs(sapply(seq(0, 1, length.out = 600), function(ii)
          min(pp(ii, seq(0, 1, length.out = 500))))))
      thre <- seq(0, 1, length.out = 600)[min]
      c_i <- rep(1, n_obs)
      ind <- which(abs(b_in) >= thre)
      while (length(ind) > 0) {
        b_in[ind] <- 0.95 * (b_in[ind])
        ind <- which(abs(b_in) >= 1)
      }
      b_i <- b_in
      warping_core <- function(t, b_i, c_i, par_och) {
        (t + (b_i * t * (t - 1)))
      }
    }
    else if (scenario == "2") {
      a_i <- stats::rnorm(n_obs, 1, sqrt(var_a))
      c_i <- rep(1, n_obs)
      b_in <- stats::rnorm(n_obs, 0, sqrt(var_b))
      if (is.null(t_out))
        pp <- function(b, t)
          b * (-(6 / 5) * pi * cos(3 * pi * t) * sin(3 * pi * t) + 2 * t - 1) + 1
      else
        pp <-
        function(b, t)
          b * (-(6 / 5) * pi * cos(3 * pi * t) * sin(3 * pi * t) + 2 * t - 1) + 1 -
        par_och + (2 * (par_och - par_och * t_out) / (1 - t_out ^ 2)) * t_out
      min <-
        which.min(abs(sapply(seq(0, 1, length.out = 1000), function(ii)
          min(pp(ii, seq(0, 1, length.out = 500))))))
      thre <- seq(0, 1, length.out = 1000)[min]
      ind <- which(abs(b_in) >= thre)
      # print(length(ind))
      while (length(ind) > 0) {
        b_in[ind] <- 0.95 * (b_in[ind])
        ind <- which(abs(b_in) >= thre)
      }
      b_i <- b_in
      warping_core <- function(t, b_i, c_i, par_och) {
        t + b_i * (t * (t - 1) - 0.2 * sin(3 * pi * c_i * t) ^ 2)
      }
    }

    if (shift == "IC") {
      mean <- function(t, par_ocx, t_out_x) {
        15 * (exp(-20 * (t - 0.7) ^ 2)) - 5 * (exp(-50 * (t - 0.45) ^ 2)) + 6 *
          (exp(-100 * (t - 0.3) ^ 2)) - 6 * (exp(-150 * (t - 0.2) ^ 2)) + 5 * (exp(-200 *
                                                                                     (t - 0.15) ^ 2))
      }
      warping <- function(t, b_i, c_i, par_och) {
        warping_core(t, b_i, c_i, par_och)
      }
    }
    else if (shift == "OC_h") {
      mean <- function(t, par_ocx, t_out_x) {
        15 * (exp(-20 * (t - 0.7) ^ 2)) - 5 * (exp(-50 * (t - 0.45) ^ 2)) + 6 *
          (exp(-100 * (t - 0.3) ^ 2)) - 6 * (exp(-150 * (t - 0.2) ^ 2)) + 5 * (exp(-200 *
                                                                                     (t - 0.15) ^ 2))
      }
      warping <- warping_ic <- function(t, b_i, c_i, par_och) {
        b <- -par_och
        a <- (-b + b * t_out) / (1 - t_out ^ 2)
        c <- -a * t_out ^ 2 - b * t_out
        warping_core(t, b_i, c_i, par_och) + sapply(1:length(t), function(ii)
          if(t[ii] > t_out)
            a * t[ii] ^ 2 + b * t[ii] + c
          else
            0)
      }
    }
    else if (shift == "OC_x") {
      mean <- function(t, par_ocx, t_out_x) {
        15 * (exp(-20 * (t - 0.7) ^ 2)) - 5 * (exp(-50 * (t - 0.45) ^ 2)) + 6 *
          (exp(-100 * (t - 0.3) ^ 2)) - 6 * (exp(-150 * (t - 0.2) ^ 2)) + 5 * (exp(-200 *
                                                                                     (t - 0.15) ^ 2)) + sapply(1:length(t), function(ii)
                                                                                       if(t[ii] > t_out_x)
                                                                                         t[ii] * par_ocx - par_ocx * t_out_x
                                                                                       else
                                                                                         0)
      }
      warping <- function(t, b_i, c_i, par_och) {
        warping_core(t, b_i, c_i, par_och)
      }
    }
    else if (shift == "OC_xh") {
      mean <- function(t, par_ocx, t_out_x) {
        15 * (exp(-20 * (t - 0.7) ^ 2)) - 5 * (exp(-50 * (t - 0.45) ^ 2)) + 6 *
          (exp(-100 * (t - 0.3) ^ 2)) - 6 * (exp(-150 * (t - 0.2) ^ 2)) + 5 * (exp(-200 *
                                                                                     (t - 0.15) ^ 2)) + sapply(1:length(t), function(ii)
                                                                                       if(t[ii] > t_out_x)
                                                                                         t[ii] * par_ocx - par_ocx * t_out_x
                                                                                       else
                                                                                         0)
      }
      warping <- warping_ic <- function(t, b_i, c_i, par_och) {
        b <- -par_och
        a <- (-b + b * t_out) / (1 - t_out ^ 2)
        c <- -a * t_out ^ 2 - b * t_out
        warping_core(t, b_i, c_i, par_och) + sapply(1:length(t), function(ii)
          if(t[ii] > t_out)
            a * t[ii] ^ 2 + b * t[ii] + c
          else
            0)
      }
    }

    T_1 <- stats::rnorm(n_obs, endd, sqrt(var_tend))
    t_out_x_i <-
      sapply(1:n_obs, function(ii)
        warping(t_out, b_i[ii], c_i[ii], par_och))
    x_true <-
      lapply(1:n_obs, function(ii)
        a_i[ii] * mean(grid, par_ocx, t_out_x_i[ii]))
    x <-
      lapply(1:n_obs, function(ii)
        a_i[ii] * mean(warping(grid, b_i[ii], c_i[ii], par_och), par_ocx, t_out_x_i[ii]))
    error <-
      MASS::mvrnorm(n_obs, rep(0, length_grid), diag(length_grid) * var_e)
    x_err <-
      lapply(1:n_obs, function(ii)
        a_i[ii] * mean(warping(grid, b_i[ii], c_i[ii], par_och), par_ocx, t_out_x_i[ii]) +
          error[ii, ])
    h <-
      lapply(1:n_obs, function(ii)
        warping(grid, b_i[ii], c_i[ii], par_och))
    new_length <- max(unlist(h))
    grid_t_cut_1 <- 0.05 * new_length
    grid_t_cut_2 <- 0.95 * new_length

    T_1 <- T_1

    ind_start <- round(grid_t_cut_1 * (length_grid - 1)) + 1
    ind_end <- round(grid_t_cut_2 * (length_grid - 1)) + 1
    delta_start <- stats::rnorm(n_obs, 0, sqrt(var_es))
    delta_end <- stats::rnorm(n_obs, 0, sqrt(var_es))
    delta_start[grid_t_cut_1 + delta_start < 0] <- -grid_t_cut_1
    delta_end[delta_end + grid_t_cut_2 > (1)] <- 1 - grid_t_cut_2
    ind_start <-
      sapply(1:n_obs, function(ii)
        min(which(h[[ii]] >= grid_t_cut_1 + delta_start[ii])))
    ind_end <-
      sapply(1:n_obs, function(ii)
        max(which(h[[ii]] <= grid_t_cut_2 + delta_end[ii])))
    ind_end[ind_end == Inf] <- length_grid
    ind_start[ind_start == -Inf] <- 1
    x_red_err <-
      lapply(1:n_obs, function(ii)
        a_i[ii] * mean(warping(
          grid * (grid[ind_end[[ii]]] - grid[ind_start[[ii]]]) + grid[ind_start[[ii]]], b_i[ii], c_i[ii], par_och
        ), par_ocx, t_out_x_i[ii]) + error[ii, ])
    h <-
      lapply(1:n_obs, function(ii)
        warping(grid * (grid[ind_end[[ii]]] - grid[ind_start[[ii]]]) + grid[ind_start[[ii]]], b_i[ii], c_i[ii], par_och))
    if ((shift == "OC_h" | shift == "OC_xh") & !is.null(end_out)) {
      t_out_i <-
        sapply(1:n_obs, function(ii)
          (t_out - grid[ind_start[[ii]]])/(grid[ind_end[[ii]]]-grid[ind_start[[ii]]]))
      T_1out <- T_1 + end_out
      T_1out <- T_1out
      grid_i <-
        lapply(1:n_obs, function(ii)
          c(
            grid[grid < t_out_i[ii]] * T_1[ii],
            (grid[grid >= t_out_i[ii]] - t_out_i[ii]) * (T_1out[ii] - T_1[ii] * t_out_i[ii]) /
              (1 - t_out_i[ii]) + T_1[ii] * t_out_i[ii]
          ))
      out_control_t <- sapply(1:n_obs, function(ii)
        t_out_i[ii] * T_1[ii])
    }
    else if (shift == "OC_x") {
      grid_i <- lapply(1:n_obs, function(ii)
        grid * T_1[ii])
      t_out_i <-
        sapply(1:n_obs, function(ii)
          (t_out - grid[ind_start[[ii]]])/(grid[ind_end[[ii]]]-grid[ind_start[[ii]]]))
      out_control_t <- sapply(1:n_obs, function(ii)
        t_out_i[ii] * T_1[ii])
    }
    else{
      grid_i <- lapply(1:n_obs, function(ii)
        grid * T_1[ii])
      out_control_t <- NULL
    }

    grid_template <- grid[grid > grid_t_cut_1 & grid < grid_t_cut_2]
    h_i_norm <-
      lapply(1:n_obs, function(ii)
        (h[[ii]] - min(grid_template))/(max(grid_template)-min(grid_template)))
    template <- mean(grid_template, par_ocx, 1)
    grid_template <-
      (grid[grid > grid_t_cut_1 &
              grid < grid_t_cut_2] - min(grid_template)) / (max(grid_template) - min(grid_template))
    out <- list(
      x_err = x_red_err,
      grid_i = grid_i,
      h = h,
      template = template,
      grid_template = grid_template,
      x_true = x_true,
      grid = grid,
      out_control_t = out_control_t
    )
    return(out)
  }

# Registration functions ---------------------------------------------------------------
get_fd_smooth <-
  function(x_value_i,
           grid,
           nbasis_max,
           plot = FALSE,
           norder = 4,
           Lfdobj = 2) {
    dom <- range(grid)
    basis_x <-
      fda::create.bspline.basis(dom, nbasis = nbasis_max, norder = norder)
    grid_new <- seq(min(grid), max(grid), length.out = 200)
    data_new <- data.frame(grid = grid_new)
    fit <-
      mgcv::gam(x_value_i ~ s(
        grid,
        k = nbasis_max,
        m = c(norder - 1, Lfdobj),
        bs = "bs"
      ))
    predict <- predict(fit, newdata = data_new)
    fd <- fda::smooth.basis(grid_new, as.numeric(predict), basis_x)$fd
    return(fd)
  }
get_lambda_new <-
  function(x_fd_list,
           der_x_fd_list,
           template_fd,
           der_template_fd,
           seq_t,
           N,
           M,
           alpha_vec,
           smin,
           smax,
           frac_oeob,
           iter,
           eta,
           threshold = 10 ^ -2,
           iter_tem = 2,
           lambda = c(0, 10 ^ seq(-5, -1, by = 0.25), 10 ^ 5),
           ncores = 1) {
    n_obs <- length(x_fd_list)
    threshold_improvement <- threshold
    if (length(lambda) > 1) {
      parr_func12 <- function(ll) {
        align_output_IC <-
          FDTW_group(
            x_fd_list,
            template_fd,
            der_x_fd_list,
            der_template_fd,
            seq_t = seq_t,
            N = N,
            M = M,
            iter = iter,
            eta = eta,
            lambda = lambda[ll],
            alpha_vec = alpha_vec,
            smin = smin,
            smax = smax,
            PLOT = FALSE,
            frac_oeob = frac_oeob,
            fit_c = TRUE,
            n_basis_x = x_fd_list[[1]]$basis$nbasis,
            get_fd = "x_reg"
          )

        value_fit <-
          sapply(1:n_obs, function(ii)
            min(align_output_IC$FDTW_output_list[[ii]]$fit))
        cvvv <- (1 / n_obs) * (sum(value_fit))
        sd <- stats::sd(value_fit) / sqrt(n_obs)
        out <- cvvv
        return(out)
      }

      if (.Platform$OS.type == "unix")
        out <-
          parallel::mclapply(1:length(lambda), function(ii)
            parr_func12(ii), mc.cores = ncores)
      else
        out <- lapply(1:length(lambda), function(ii)
          parr_func12(ii))
      cv_fit <- unlist(out)
      cv_norm <- (cv_fit - min(cv_fit)) / (max(cv_fit) - min(cv_fit))
      ind_lambda_opt <- max(which(cv_norm < threshold_improvement))
    }
    else{
      cv_fit  <-  0
      cv_norm <- cv_fit
      ind_lambda_opt  <-  1
    }
    lambda_opt <- lambda[ind_lambda_opt]
    print(paste0("lambda_opt = 10^", log10(lambda_opt)))
    align_output_IC <-
      FDTW_group(
        x_fd_list,
        template_fd,
        der_x_fd_list,
        der_template_fd,
        seq_t = seq_t ,
        N = N,
        M = M,
        iter = iter,
        eta = eta,
        lambda = lambda_opt,
        alpha_vec = alpha_vec,
        smin = smin,
        smax = smax,
        PLOT = FALSE,
        frac_oeob = frac_oeob,
        fit_c = FALSE,
        n_basis_x = x_fd_list[[1]]$basis$nbasis,
        get_fd = "x_reg"
      )

    complete_fd <-
      completion_fd(align_output_IC$x_reg_fd,
                    align_output_IC$h_fd,
                    template_fd,
                    seq_t = seq_t)
    # if(PLOT){
    #   par(mfrow=c(1,2))
    #   plot(cv_norm)
    #   abline(h=threshold_improvement)
    #   plot(complete_fd$x_fd_com)
    #   graphics::lines(template_fd,lwd=3)
    # }
    out <- list(complete_fd = complete_fd,
                lambda_opt = lambda_opt)

  }



FDTW_group <-
  function(x_fd_list,
           template_fd,
           der_x_fd_list,
           der_template_fd ,
           lambda = 10 ^ -3,
           N = 600,
           M = 100,
           iter = 5,
           eta = 0.5,
           alpha_vec = 1,
           h_fd_list = NULL,
           seq_t = NULL,
           fit_c = FALSE,
           point_pen = NULL,
           frac_oeob = frac_oeob,
           ...) {
    n_obs <- length(x_fd_list)
    FDTW_output_list <- list()
    x_reg_fd <- h_fd <- list()
    for (ii in 1:n_obs) {
      dom_x <- x_fd_list[[ii]]$basis$rangeval
      dom_tem <- template_fd$basis$rangeval
      if (is.null(point_pen))
        point_pen = c(0, 0)
      der_0  <-  (dom_x[2] - point_pen[2]) / (dom_tem[2] - point_pen[1])

      delta_xe  <-  delta_xs  <-  frac_oeob * abs(diff(dom_tem))
      delta_ye  <-  delta_ys  <-  frac_oeob * abs(diff(dom_x))
      FDTW_output_list[[ii]] <-
        OEBFDTW(
          x_fd_list[[ii]],
          template_fd,
          der_x_fd_list[[ii]] ,
          der_template_fd ,
          N = N,
          M = M,
          lambda = lambda ,
          alpha_vec = alpha_vec,
          iter = iter,
          eta = eta,
          seq_t = seq_t,
          fit_c = fit_c,
          der_0 = der_0,
          delta_xs = delta_xs,
          delta_ys = delta_ys,
          delta_xe = delta_xe,
          delta_ye = delta_ye,
          ...
        )
      x_reg_fd[[ii]] <- FDTW_output_list[[ii]]$mod$x_reg_fd
      h_fd[[ii]] <- FDTW_output_list[[ii]]$mod$h_fd
    }
    out <- list(FDTW_output_list = FDTW_output_list,
                x_reg_fd = x_reg_fd,
                h_fd = h_fd)
  }


#' @title Open-end/open-begin Functional Dynamic Time Warping (OEB-FDTW)
#' @description This function implements the OEB-FDTW.
#' @param x_fd  An object of class fd corresponding to the misaligned  function.
#' @param template_fd  An object of class fd corresponding to the template function.
#' @param der_x_fd  An object of class fd corresponding to the first derivative of \code{x_fd}.
#' @param der_template_fd  An object of class fd corresponding to the first derivative of \code{template_fd}.
#' @param fit_c  If TRUE, the value of the objective function without the penalization evaluated at the solution is returned.
#' @param lambda  The smoothing parameter \eqn{\lambda_i}.
#' @param delta_xs  Maximum allowed misalignment at the beginning of the process along the misaligned  function domain.
#' @param delta_xe  Maximum allowed misalignment at the end of the process along the misaligned  function domain.
#' @param delta_ys  Maximum allowed misalignment at the beginning of the process along the template domain.
#' @param delta_ye  Maximum allowed misalignment at the end of the process along the template domain.
#' @param seq_t Discretized sequence in the template domain \eqn{\mathcal{D}_{Y}}.  If NULL, an equally spaced grid of length \code{N} in the template domain is used.
#' @param der_0  The target values towards which shrinking the warping function slope. If NULL, it is equal to the ratio between the size of the domain of \code{x_fd}
#' and the  size of the domain of \code{template_fd}.
#' @param get_fd  If "x_reg",  the registered function as a class fd object is returned. If "no",  the registered function as a class fd object is not returned.
#' @param n_basis_x  Number of basis to obtain the  registered function. If NULL, it is set as 0.5 the length of the optimal path.
#' @inheritParams par.FDTW
#' @inheritParams par.rtr
#'
#' @references
#' Centofanti, F., A. Lepore, M. Kulahci, and M. P. Spooner (2024).
#' Real-time monitoring of functional data. \emph{Journal of Quality Technology}, 57(2):135--152,
#' doi:https://doi.org/10.1080/00224065.2024.2430978.
#'
#' @return   A list containing the following arguments:
#'
#' \code{mod} that is a list composed by
#' \itemize{
#' \item \code{h_fd}: The estimated warping function.
#'
#' \item \code{x_reg}: The registered function.
#'
#' \item \code{h_list}: A list containing the discretized warping function for each iteration of the iterative refinement.
#'
#' \item \code{min_cost}: Optimal value of the objective function.
#'
#' \item \code{lambda}: The smoothing parameter \eqn{\lambda}.
#'
#' \item \code{alpha}: Optimal value of the  parameter \eqn{\alpha_i}.
#'}
#'
#' \code{obj} Values of the objective function for each value in \code{alpha_vec}.
#'
#' \code{fit} Values of the objective function without the penalization for each value in \code{alpha_vec}.
#'
#' \code{obj_opt} Optimal value of the objective function.
#'
#' \code{fit_opt} Optimal value of the objective function without the penalization.
#' @export
#' @examples
#'
#' set.seed(1)
#' data <- simulate_data_FRTM(n_obs = 100)
#'
#' dom <- c(0, 1)
#' basis_x <- fda::create.bspline.basis(c(0, 1), nbasis = 30)
#' x_fd <-
#'   fda::smooth.basis(data$grid_i[[1]] / max(data$grid_i[[1]]), data$x_err[[1]], basis_x)$fd
#' template_fd <-
#'   fda::smooth.basis(data$grid_template, data$template, basis_x)$fd
#' der_x_fd <- fda::deriv.fd(x_fd, 1)
#' der_template_fd <- fda::deriv.fd(template_fd, 1)
#'
#' mod <-
#'   OEBFDTW(x_fd, template_fd, der_x_fd , der_template_fd, get_fd = "x_reg")
#'@references
#' Deriso, D. and S. Boyd (2022).
#'  A general optimization framework for dynamic time warping.
#'  \emph{Optimization and Engineering, 1-22}.
#'
OEBFDTW <-
  function(x_fd,
           template_fd,
           der_x_fd,
           der_template_fd,
           alpha_vec = c(0, 0.5, 1),
           fit_c = FALSE,
           N = 100,
           M = 50,
           smin = 0.01,
           smax = 1000,
           lambda = 10 ^ -5,
           eta = 0.5,
           iter = 3,
           delta_xs = 0,
           delta_xe = 0,
           delta_ys = 0,
           delta_ye = 0,
           der_0 = NULL,
           seq_t = NULL,
           get_fd = "no",
           n_basis_x = NULL) {
    FDTW_output_list <- list()
    obj1 <- numeric()
    for (mm in 1:length(alpha_vec)) {
      FDTW_output_list[[mm]] <-
        OEBFDTW_c(
          x_fd,
          template_fd,
          der_x_fd,
          der_template_fd  ,
          N = N,
          M = M,
          smin = smin,
          smax = smax,
          lambda = lambda,
          eta = eta,
          iter = iter,
          delta_xs = delta_xs,
          delta_xe = delta_xe,
          delta_ys = delta_ys,
          delta_ye = delta_ye,
          alpha = alpha_vec[mm],
          der_0 = der_0,
          seq_t = seq_t,
          get_fd = get_fd,
          n_basis_x = n_basis_x
        )
      aa_obj_mm <- FDTW_output_list[[mm]]$min_cost
      obj1[mm] <- FDTW_output_list[[mm]]$min_cost
    }
    if (fit_c) {
      fit <- numeric()
      for (mm in 1:length(alpha_vec)) {
        fit[mm] <-
          obj_function(
            x_fd,
            template_fd,
            der_x_fd,
            der_template_fd,
            FDTW_output_list[[mm]]$h_list[[length(FDTW_output_list[[mm]]$h_list)]],
            lambda = FDTW_output_list[[mm]]$lambda,
            alpha = FDTW_output_list[[mm]]$alpha
          )$fit
      }
    }
    else{
      fit <- obj1
    }
    ind_opt <- which.min(obj1)
    out <- list(
      mod = FDTW_output_list[[ind_opt]],
      obj = obj1,
      fit = fit,
      obj_opt = obj1[ind_opt],
      fit_opt = fit[ind_opt]
    )
    return(out)
  }
OEBFDTW_c <-
  function(x_fd,
           template_fd,
           der_x_fd,
           der_template_fd,
           N = 100,
           M = 100,
           smin = 0,
           smax = 10,
           lambda = 10,
           eta = 0.5,
           iter = 1,
           delta_xs = 0,
           delta_xe = 0,
           delta_ys = 0,
           delta_ye = 0,
           alpha = 0.5,
           der_0 = NULL,
           seq_t = NULL,
           get_fd = "no",
           n_basis_x = NULL) {
    range_x <- x_fd$basis$rangeval
    grid_eval_x <- seq(range_x[1], range_x[2], length.out = 100)
    local_mm_x <- fda::eval.fd(x_fd, grid_eval_x)
    local_mm_der_x <- fda::eval.fd(der_x_fd, grid_eval_x)

    range_tem <- template_fd$basis$rangeval
    grid_eval_tem <- seq(range_tem[1], range_tem[2], length.out = 100)
    local_mm_tem <- fda::eval.fd(template_fd, grid_eval_tem)
    local_mm_der_tem <- fda::eval.fd(der_template_fd, grid_eval_tem)

    norm_x <- max(abs(local_mm_x))
    norm_template <- max(abs(local_mm_tem))
    norm_der_x <- max(abs(local_mm_der_x))
    norm_der_template <- max(abs(local_mm_der_tem))

    x_fd_std <- norm_x ^ (-1) * x_fd
    template_fd_std <- norm_template ^ (-1) * template_fd
    der_x_fd_std <- norm_der_x ^ (-1) * der_x_fd
    der_template_fd_std <- norm_der_template ^ (-1) * der_template_fd

    dom_x <- abs(diff(range_x))
    dom_tem <- abs(diff(range_tem))
    if (is.null(der_0))
      der_0 <- dom_x / dom_tem
    if (is.null(seq_t)) {
      grid_t <- seq(range_tem[1], range_tem[2], length.out = N)
      delta_t <- grid_t[2] - grid_t[1]
    }
    else{
      grid_t <-
        unique(c(range_tem[1], seq_t[which(seq_t <= range_tem[2] &
                                             seq_t >= range_tem[1])], range_tem[2]))
      N <- length(grid_t)
    }
    template_eval <- fda::eval.fd(template_fd_std, grid_t)
    der_template_eval <- fda::eval.fd(der_template_fd_std, grid_t)

    l_0 <-
      sapply(1:N, function(ii)
        max(
          smin * grid_t[ii] + range_x[1] - smin * (range_tem[1] + delta_xs),
          (grid_t[ii]) * smax + range_x[2] - delta_ye - range_tem[2] * smax,
          range_x[1]
        ))
    u_0 <-
      sapply(1:N, function(ii)
        min(
          smax * grid_t[ii] + range_x[1] + delta_ys - range_tem[1] * smax,
          (grid_t[ii]) * smin + range_x[2] - (range_tem[2] - delta_xe) * smin,
          range_x[2]
        ))

    smin_n <- smin
    smax_n <- smax
    h <- l_p <- u_p <- grid_list <- list()
    for (iter_ii in 1:iter) {
      if (iter_ii == 1) {
        l_0[l_0 > range_x[2]] = range_x[2]
        l_0[l_0 < range_x[1]] = range_x[1]
        u_0[u_0 > range_x[2]] = range_x[2]
        u_0[u_0 < range_x[1]] = range_x[1]

        while (sum(l_0 - u_0) > 0) {
          smin_n <- smin_n / 100
          smax_n <- smax_n * 100
          # if(PLOT)print(paste("No feasibile solution increasing/deaccreasing smax and smin, smax =",smax_n,"smin=",smin_n))
          l_0 <-
            sapply(1:N, function(ii)
              max(
                smin_n * grid_t[ii] + range_x[1] - smin_n * (range_tem[1] + delta_xs),
                (grid_t[ii]) * smax_n + range_x[2] - delta_ye - range_tem[2] * smax_n,
                range_x[1]
              ))
          u_0 <-
            sapply(1:N, function(ii)
              min(
                smax_n * grid_t[ii] + range_x[1] + delta_ys - range_tem[1] * smax_n,
                (grid_t[ii]) * smin_n + range_x[2] - (range_tem[2] - delta_xe) * smin_n,
                range_x[2]
              ))
          l_0[l_0 > range_x[2]] = range_x[2]
          l_0[l_0 < range_x[1]] = range_x[1]
          u_0[u_0 > range_x[2]] = range_x[2]
          u_0[u_0 < range_x[1]] = range_x[1]
        }

        l <-   l_p[[iter_ii]] <- l_0
        u <-   u_p[[iter_ii]] <- u_0
      }
      else{
        h_start <- h[[iter_ii - 1]][1, 1]
        h_end <- h[[iter_ii - 1]][dim(h[[iter_ii - 1]])[1], 1]
        ii_start <- which(grid_t == h_start)
        ii_end <- which(grid_t == h_end)
        grid_tnew <- grid_t[ii_start:ii_end]
        l_p[[iter_ii]] <- l_p[[iter_ii - 1]]
        u_p[[iter_ii]] <- u_p[[iter_ii - 1]]
        h_start <- h[[iter_ii - 1]][1, 1]
        h_end <- h[[iter_ii - 1]][dim(h[[iter_ii - 1]])[1], 1]
        ii_start <- which(grid_t == h_start)
        ii_end <- which(grid_t == h_end)
        grid_tnew <- grid_t[ii_start:ii_end]
        l_p[[iter_ii]] <- l_p[[iter_ii - 1]]
        u_p[[iter_ii]] <- u_p[[iter_ii - 1]]
        l <- l_p[[iter_ii - 1]]
        u <- u_p[[iter_ii - 1]]

        for (ii in 1:length(ii_start:ii_end))
          l_p[[iter_ii]][ii_start + ii - 1] <-
          max(h[[iter_ii - 1]][ii, 2] - eta * (u[ii_start + ii - 1] - l[ii_start +
                                                                          ii - 1]) / 2, l_0[ii_start + ii - 1], l_p[[iter_ii]][ii_start + ii - 2])


        for (ii in 1:length(ii_start:ii_end))
          u_p[[iter_ii]][ii_start + ii - 1] <-
          min(
            h[[iter_ii - 1]][ii, 2] + eta * (u[ii_start + ii - 1] - l[ii_start + ii -
                                                                        1]) / 2,
            u_0[ii_start + ii - 1],
            if (ii > 1)
              smax_n * grid_tnew[ii] + u_p[[iter_ii]][ii_start + ii - 2] - smax_n * grid_tnew[ii -
                                                                                                1]
            else
              NULL
          )


        if (ii_end < N) {
          for (ii in ii_end:N)
            l_p[[iter_ii]][ii] <-
              max(l_p[[iter_ii]][ii_end] + ((range_x[2] - delta_ye - l_p[[iter_ii]][ii_end]) /
                                              (grid_t[N] - grid_t[ii_end])) * (grid_t[ii] - grid_t[ii_end]), l_p[[iter_ii]][ii_end])

        }
        if (ii_start > 1) {
          for (ii in 1:ii_start)
            u_p[[iter_ii]][ii] <-
              min(u_p[[iter_ii]][ii_start] + ((range_x[1] + delta_ys - u_p[[iter_ii]][ii_start]) /
                                                (grid_t[1] - grid_t[ii_start])) * (grid_t[ii] - grid_t[ii_start]), u_p[[iter_ii]][ii_start])
        }

        l_p[[iter_ii]][1:ii_start] <- l_p[[iter_ii]][ii_start]
        u_p[[iter_ii]][ii_end:N] <- u_p[[iter_ii]][ii_end]
        l_p[[iter_ii]][l_p[[iter_ii]] > range_x[2]] <- range_x[2]
        l_p[[iter_ii]][l_p[[iter_ii]] < range_x[1]] <- range_x[1]
        u_p[[iter_ii]][u_p[[iter_ii]] > range_x[2]] <- range_x[2]
        u_p[[iter_ii]][u_p[[iter_ii]] < range_x[1]] <- range_x[1]
        l <- l_p[[iter_ii]]
        u <- u_p[[iter_ii]]

      }

      DPmod <-
        DP3(
          N,
          M,
          l,
          u,
          as.matrix(range_x),
          as.matrix(range_tem),
          as.matrix(grid_t),
          x_fd_std,
          der_x_fd_std ,
          delta_xs,
          template_eval,
          der_template_eval,
          smin_n,
          smax_n,
          alpha,
          lambda,
          der_0,
          fda::eval.fd
        )
      grid_search <- DPmod[[1]]
      D <- DPmod[[2]]
      P <- DPmod[[3]]
      L <- DPmod[[4]]

      ind_end1 <-
        which(u == range_x[2])[which(P[which(u == range_x[2]), M] != Inf)]
      if (length(ind_end1) == 0)
        path_list1 <- list()
      else
        path_list1 <-
        get_path_list1(N,
                       M,
                       as.matrix(range_x),
                       as.matrix(range_tem),
                       grid_t,
                       ind_end1,
                       grid_search ,
                       P)

      ind_end2 <- which(P[N, ] != Inf)
      if (length(ind_end2) == 0) {
        path_list2 <- list()
      }
      else{
        if (length(unique(P[N, ])) == 1)
          ind_end2 <- 1
        path_list2 <-
          get_path_list2(N,
                         M,
                         as.matrix(range_x),
                         as.matrix(range_tem),
                         grid_t,
                         ind_end2,
                         grid_search ,
                         P)
      }
      path_list <- c(path_list1, rev(path_list2))
      length_path_list <-
        sapply(1:length(path_list), function(ii)
          path_list[[ii]][dim(path_list[[ii]])[1], 1] - path_list[[ii]][1, 1])
      new_D_end <-
        c(D[ind_end1, M] / L[ind_end1, M], rev(D[N, ind_end2] / L[N, ind_end2]))
      ind_min <- which.min(new_D_end)
      path_opt <- path_list[[ind_min]]

      h[[iter_ii]] <- path_opt
      grid_list[[iter_ii]] <- grid_search
    }
    min_cost <- min(new_D_end)
    h_opt <- h[[iter]]
    grid_x <- path_opt[, 1]
    startend <- c(path_opt[1, 1], path_opt[dim(path_opt)[1], 1])
    if (length(h_opt[, 2]) < 4) {
      basis_h <-
        fda::create.bspline.basis(startend, breaks = h_opt[, 1], norder = 2)
      basis_x <-
        fda::create.bspline.basis(startend,
                             nbasis = length(h_opt[, 2]),
                             norder = length(h_opt[, 2]))

    }
    else{
      n_basis_x <-
        if (!is.null(n_basis_x))
          min(n_basis_x, round(0.5 * length(h_opt[, 2])))
      else
        round(0.5 * length(h_opt[, 2]))
      basis_h <-
        fda::create.bspline.basis(startend, breaks = h_opt[, 1], norder = 2)
      basis_x <-
        fda::create.bspline.basis(startend,
                             nbasis = max(4, n_basis_x),
                             norder = 4)
    }
    h_fd <- fda::fd(h_opt[, 2], basis_h)
    if (get_fd == "x_reg") {
      x_tilde <- fda::eval.fd(h_opt[, 2], x_fd)
      x_reg <- fda::smooth.basis(h_opt[, 1], x_tilde, basis_x)$fd
      der_reg = r_reg = der_tilde = x_fd_std = template_fd_std = der_x_fd_std =
        der_template_fd_std = x_fd_reg_std = der_x_reg_std = NULL
    }
    else{
      x_reg <-
        x_tilde <-
        der_reg <-
        r_reg <-
        der_tilde <-
        x_fd_std <-
        template_fd_std <-
        der_x_fd_std <-
        der_template_fd_std <- x_fd_reg_std <- der_x_reg_std <- NULL
    }
    out <- list(
      h_fd = h_fd,
      x_reg_fd = x_reg,
      lambda = lambda,
      h_list = h,
      min_cost = min_cost,
      alpha = alpha
    )

    return(out)
  }
obj_function <-
  function(x_fd,
           template_fd,
           der_x_fd,
           der_template_fd,
           h,
           lambda = 1,
           lambda_c = 0,
           smin = 0.1,
           smax = 100,
           alpha = 1,
           der_0 = NULL) {
    range_x <- x_fd$basis$rangeval
    grid_eval_x <- seq(range_x[1], range_x[2], length.out = 100)
    local_mm_x <- fda::eval.fd(x_fd, grid_eval_x)
    local_mm_der_x <- fda::eval.fd(der_x_fd, grid_eval_x)
    range_tem <- template_fd$basis$rangeval
    grid_eval_tem <- seq(range_tem[1], range_tem[2], length.out = 100)
    local_mm_tem <- fda::eval.fd(template_fd, grid_eval_tem)
    local_mm_der_tem <- fda::eval.fd(der_template_fd, grid_eval_tem)
    norm_x <- max(abs(local_mm_x))
    norm_template <- max(abs(local_mm_tem))
    norm_der_x <- max(abs(local_mm_der_x))
    norm_der_template <- max(abs(local_mm_der_tem))

    x_fd_std <- norm_x ^ (-1) * x_fd
    template_fd_std <- norm_template ^ (-1) * template_fd
    der_x_fd_std <- norm_der_x ^ (-1) * der_x_fd
    der_template_fd_std <- norm_der_template ^ (-1) * der_template_fd

    N <- dim(h)[1]
    delta_t <- h[2, 1] - h[1, 1]
    range_h <- abs(h[1, 1] - h[N, 1])
    grid_t <- seq(h[1, 1], h[N, 1], length.out = N)

    dom_x <- abs(diff(range_x))
    dom_tem <- abs(diff(range_tem))

    if (is.null(der_0))
      der_0 <- dom_x / dom_tem
    eval_x_fd_st <- fda::eval.fd(h[, 2], x_fd_std)
    eval_tem_fd_st <- fda::eval.fd(h[, 1], template_fd_std)
    eval_der_x_fd_st <- fda::eval.fd(h[, 2], der_x_fd_std)
    eval_der_tem_fd_st <- fda::eval.fd(h[, 1], der_template_fd_std)
    loss_dtw <- quad_sum <- der_sum <- der <- numeric(N)
    loss_dtw[1] <- 0
    quad_sum[1] <- der_sum[1] <- der[1] <- 0
    for (ii in 2:N) {
      quad_sum[ii] <-
        delta_t * (
          loss2(
            eval_x_fd_st[ii],
            eval_tem_fd_st[ii],
            eval_der_x_fd_st[ii],
            eval_der_tem_fd_st[ii],
            alpha,
            ((h[ii, 2] - h[ii - 1, 2]) / (grid_t[ii] - grid_t[ii - 1])) * (der_0) ^
              -1
          )
        )
      der[ii] <-
        delta_t * der_func((h[ii, 2] - h[ii - 1, 2]) / (grid_t[ii] - grid_t[ii -
                                                                              1]), 0, 10 ^ 8)
      der_sum[ii] <-
        lambda * delta_t * der_func((h[ii, 2] - h[ii - 1, 2]) / (grid_t[ii] - grid_t[ii -
                                                                                       1]), smin, smax, der_0 = der_0)
      loss_dtw[ii] <- quad_sum[ii] + der_sum[ii]
    }
    out <-
      list(
        loss = sum(loss_dtw) / range_h,
        loss_ii = loss_dtw,
        fit = sum(quad_sum) / range_h,
        fit_ii = quad_sum,
        der = sum(der) / range_h,
        der_ii = der
      )
    return(out)
  }
completion_fd <- function(x_fd, h_fd, template_fd, seq_t) {
  range_t <- template_fd$basis$rangeval
  eval_seq_h <- c(0, seq_t)
  eval_seq_x <- seq(range_t[1], range_t[2], length.out = 200)
  eval_h_mat <- matrix(0, length(eval_seq_h), length(h_fd))
  for (ii in 1:length(h_fd)) {
    c_h <- complete_h(h_fd[[ii]], eval_seq = eval_seq_h, inv = FALSE)
    eval_h_mat[, ii] <- c_h$eval_h
  }
  basis_h_r <-
    fda::create.bspline.basis(range_t, breaks = eval_seq_h, norder = 2)
  h_fd_com <- fda::fd(eval_h_mat, basis_h_r)
  basis_x_r <-
    fda::create.bspline.basis(range_t, nbasis = x_fd[[1]]$basis$nbasis)
  eval_x_mat <- matrix(0, length(eval_seq_x), length(h_fd))
  for (ii in 1:length(h_fd)) {
    c_x <- complete_x(x_fd[[ii]], template_fd, eval_seq = eval_seq_x)
    eval_x_mat[, ii] <- c_x$eval_x
  }
  x_fd_com <- fda::smooth.basis(eval_seq_x, eval_x_mat, basis_x_r)$fd
  out <- list(x_fd_com = x_fd_com,
              h_fd_com = h_fd_com,
              template_fd_com = template_fd)
  return(out)
}
complete_h <-
  function(h_fd_i,
           eval_seq = seq(0, 1, length.out = 200),
           inv = TRUE) {
    range_h <- h_fd_i$basis$rangeval
    range_eval <- c(eval_seq[1], max(eval_seq))
    basis_h_i <-
      fda::create.bspline.basis(range_eval, breaks =  eval_seq, norder = 2)
    eval_h <-
      fda::eval.fd(eval_seq[which(eval_seq >= range_h[1] &
                                    eval_seq <= range_h[2])], h_fd_i)
    if (range_h[1] != range_eval[1]) {
      x <- eval_seq[which(eval_seq < range_h[1])]
      a <- abs(diff(range(eval_h)) / diff(range_h))
      b  <-  -a * range_h[1]
      y  <-  x * a + b
      eval_h_c1 <- y
    }
    else{
      eval_h_c1  <-  NULL
    }
    if (range_h[2] != range_eval[2]) {
      x <- eval_seq[which(eval_seq > range_h[2])]
      a <- abs(diff(range(eval_h)) / diff(range_h))
      b  <-  range(eval_h)[2] - a * eval_seq[which(eval_seq <= range_h[2])][length(eval_seq[which(eval_seq <=
                                                                                                 range_h[2])])]
      y  <-  x * a + b
      eval_h_c2 <- y
    }
    else{
      eval_h_c2  <-  NULL
    }
    eval_h  <-  c(eval_h_c1, eval_h, eval_h_c2)
    basis_h_r_inv <-
      if (inv)
        fda::create.bspline.basis(
          range(eval_h),
          nbasis = h_fd_i$basis$nbasis,
          norder = min(h_fd_i$basis$nbasis, 4)
        )
    else
      NULL
    h_fd_com_inv <-
      if (inv)
        fda::smooth.basis(eval_h, eval_seq, basis_h_r_inv)$fd
    else
      NULL
    h_fd_com <- fda::fd(eval_h, basis_h_i)
    out <- list(
      eval_h = eval_h,
      eval_seq = eval_seq,
      h_fd_com = h_fd_com,
      h_fd_com_inv = h_fd_com_inv
    )
    return(out)
  }
complete_x <-
  function(x_fd_i,
           template_fd,
           eval_seq = seq(0, 1, length.out = 200)) {
    range_x <- x_fd_i$basis$rangeval
    range_eval <- c(eval_seq[1], max(eval_seq))
    basis_x_i <-
      fda::create.bspline.basis(
        c(range_eval[1], range_eval[2]),
        nbasis = min(length(eval_seq), x_fd_i$basis$nbasis),
        norder = min(length(eval_seq), 4, x_fd_i$basis$nbasis)
      )
    eval_x <-
      fda::eval.fd(eval_seq[which(eval_seq >= range_x[1] &
                                    eval_seq <= range_x[2])], x_fd_i)
    if (range_x[1] != range_eval[1]) {
      x <- eval_seq[which(eval_seq < range_x[1])]
      x_con <- eval_seq[which(eval_seq >= range_x[1])][1]
      y <- fda::eval.fd(x, template_fd)
      eval_x_c1 <-
        y + as.numeric(fda::eval.fd(x_con, x_fd_i) - fda::eval.fd(x_con, template_fd))
    }
    else{
      eval_x_c1  <-  NULL
    }
    if (range_x[2] != range_eval[2]) {
      x <- eval_seq[which(eval_seq > range_x[2])]
      x_con <-
        eval_seq[which(eval_seq <= range_x[2])][length(eval_seq[which(eval_seq <=
                                                                        range_x[2])])]
      y <- fda::eval.fd(x, template_fd)
      eval_x_c2 <-
        y + as.numeric(fda::eval.fd(x_con, x_fd_i) - fda::eval.fd(x_con, template_fd))
    }
    else{
      eval_x_c2  <-  NULL
    }
    eval_x  <-  c(eval_x_c1, eval_x, eval_x_c2)
    x_fd_com <- fda::smooth.basis(eval_seq, eval_x, basis_x_i)$fd
    out <- list(eval_x = eval_x,
                eval_seq = eval_seq,
                x_fd_com = x_fd_com)
    return(out)

  }
get_template <-
  function(x_fd_list,
           der_x_fd_list,
           seq_t,
           N,
           M,
           alpha_vec,
           smin,
           smax,
           frac_oeob,
           iter,
           eta,
           threshold = 10 ^ -4,
           iter_tem = 2,
           lambda = c(0, 10 ^ seq(-9, -1, by = 1), 10 ^ 5),
           print = FALSE,
           ncores = 1,
           index_tem = NULL) {
    n_obs <- length(x_fd_list)
    duration <-
      sapply(1:n_obs, function(ii)
        abs(base::diff(range(
          x_fd_list[[ii]]$basis$rangeval
        ))))
    median_duration <- stats::median(duration)
    selected_f <- which.min(abs(duration - median_duration))
    x_fd_median_dur <-
      if (is.null(index_tem))
        x_fd_list[[selected_f]]
    else if (methods::is(index_tem, "fd"))
      index_tem
    else
      x_fd_list[[index_tem]]
    range_xx <- x_fd_median_dur$basis$rangeval
    template_fd <- x_fd_median_dur
    template_fd$basis$params = (template_fd$basis$params - range_xx[1]) /
      (range_xx[2] - range_xx[1])
    template_fd$basis$rangeval <- c(0, 1)##convention
    der_template_fd <- fda::deriv.fd(template_fd)
    threshold_improvement <- threshold
    for (iter_t in 1:iter_tem) {
      if (length(lambda) > 1) {
        parr_func <- function(ll) {
          align_output_IC <-
            FDTW_group(
              x_fd_list,
              template_fd,
              der_x_fd_list,
              der_template_fd,
              seq_t = seq_t,
              N = N,
              M = M,
              iter = iter,
              eta = eta,
              lambda = lambda[ll],
              alpha_vec = alpha_vec,
              smin = smin,
              smax = smax,
              frac_oeob = frac_oeob,
              fit_c = TRUE,
              n_basis_x = x_fd_list[[1]]$basis$nbasis,
              get_fd = "x_reg"
            )

          value_fit <-
            sapply(1:n_obs, function(ii)
              min(align_output_IC$FDTW_output_list[[ii]]$fit))
          cvvv <- (1 / n_obs) * (sum(value_fit))
          sd <- stats::sd(value_fit) / sqrt(n_obs)
          out <- cvvv
          return(out)
        }
        if (.Platform$OS.type == "unix")
          out <-
            parallel::mclapply(1:length(lambda), function(ii)
              parr_func(ii), mc.cores = ncores)
        else
          out <- lapply(1:length(lambda), function(ii)
            parr_func(ii))
        cv_fit <- unlist(out)
        cv_norm <- (cv_fit - min(cv_fit)) / (max(cv_fit) - min(cv_fit))
        ind_lambda_opt <- max(which(cv_norm < threshold_improvement))
      }
      else{
        cv_fit  <-  0
        cv_norm <- cv_fit
        ind_lambda_opt  <-  1
      }
      lambda_opt <- lambda[ind_lambda_opt]
      if (print)
        print(paste0("lambda_opt= 10^", log10(lambda_opt)))
      align_output_IC <-
        FDTW_group(
          x_fd_list,
          template_fd,
          der_x_fd_list,
          der_template_fd,
          seq_t = seq_t ,
          N = N,
          M = M,
          iter = iter,
          eta = eta,
          lambda = lambda_opt,
          alpha_vec = alpha_vec,
          smin = smin,
          smax = smax,
          frac_oeob = frac_oeob,
          fit_c = FALSE,
          n_basis_x = x_fd_list[[1]]$basis$nbasis,
          get_fd = "x_reg"
        )
      complete_fd <-
        completion_fd(align_output_IC$x_reg_fd,
                      align_output_IC$h_fd,
                      template_fd,
                      seq_t = seq_t)
      template_fd <- fda::mean.fd(complete_fd$x_fd_com)
      der_template_fd <- fda::deriv.fd(template_fd)
    }
    out <- list(
      template_fd = template_fd,
      der_template_fd = der_template_fd,
      complete_fd = complete_fd,
      lambda_opt = lambda_opt,
      median_duration = median_duration
    )
    return(out)
  }
get_ul <- function(h_fd,
                   perc = 0.001,
                   type = "perc",
                   bandp = 0.1,
                   bandm = 0.1) {
  length_t <- 100
  n_obs <- dim(h_fd$coefs)[2]
  grid <- seq(0, 1, length.out = length_t)
  eval <- fda::eval.fd(grid, h_fd)
  basis_list <-
    lapply(1:n_obs, function(ii)
      fda::create.bspline.basis(c(min(eval[, ii]), max(eval[, ii])), nbasis = 30, norder = 3))
  h_inv <-
    lapply(1:n_obs, function(ii)
      fda::smooth.basis(eval[, ii], grid, basis_list[[ii]])$fd)
  if (type == "perc") {
    grid_x <- seq(0, max(eval), length.out = length_t)
    eval_h <- matrix(0, length(h_inv), length(grid_x))
    for (ii in 1:length(grid_x)) {
      for (kk in 1:length(h_inv)) {
        range_h <- h_inv[[kk]]$basis$rangeval
        eval_h[kk, ii] <-
          if (grid_x[ii] < range_h[1])
            0
        else if (grid_x[ii] > range_h[2])
          1
        else
          fda::eval.fd(grid_x[ii], h_inv[[kk]])
      }
    }
    den_list <- apply(eval_h, 2, stats::density)
    lim_mat <-
      t(sapply(1:length(den_list), function(ii)
        spatstat.univar::quantile.density(den_list[[ii]], c(perc, 1 - perc))))
    lim_mat[lim_mat < 0] = 0
    lim_mat[lim_mat > 1] = 1
    ind_w <- which(lim_mat[1:10, 2] > 0.4)
    if (length(ind_w) > 0)
      lim_mat[ind_w, 2] = 0
    ind_w <- which(lim_mat[(length_t - 10):length_t, 1] < 0.6)
    if (length(ind_w) > 0)
      lim_mat[ind_w, 1] = 0
    l_t_r1 <- lim_mat[, 1]
    ind_del <-
      c(which(l_t_r1 <= 0)[-length(which(l_t_r1 <= 0))], which(l_t_r1 >= 1)[-1])
    l_t_r <- if (length(ind_del) == 0)
      l_t_r1
    else
      l_t_r1[-ind_del]
    grid_l_t <- if (length(ind_del) == 0)
      grid_x
    else
      grid_x[-ind_del]
    basis_l <-
      fda::create.bspline.basis(range(grid_l_t), breaks = grid_l_t , norder = 2)
    l_fd <- fda::fd(l_t_r, basis_l)
    u_t_r1 <- lim_mat[, 2]
    ind_del <-
      c(which(u_t_r1 <= 0)[-length(which(u_t_r1 <= 0))], which(u_t_r1 >= 1)[-1])
    u_t_r <- if (length(ind_del) == 0)
      u_t_r1
    else
      u_t_r1[-ind_del]
    grid_u_t <- if (length(ind_del) == 0)
      grid_x
    else
      grid_x[-ind_del]
    basis_u <-
      fda::create.bspline.basis(range(grid_u_t), breaks = grid_u_t , norder = 2)
    u_fd <- fda::fd(u_t_r, basis_u)
    fdp <- fdm <- NULL
  }
  else if (type == "band") {
    end <- stats::median(fda::eval.fd(1, h_fd))
    start <- stats::median(fda::eval.fd(0, h_fd))
    basis <- fda::create.bspline.basis(c(0, 1), nbasis = 2, norder = 2)
    fd <- fda::smooth.basis(c(0, 1), c(start, end), basis)$fd
    fdp <- fd + bandp
    fdm <- fd - bandm
    grid <- seq(0, 1, length.out = 200)
    eval <- fda::eval.fd(grid, fdp)
    eval[abs(eval) < 10 ^ -6] = 0
    ind <- which(eval >= 0)
    eval <- eval[ind]
    new_basis <- fda::create.bspline.basis(range(eval), nbasis = 2, norder =
                                        2)
    l_fd <- fda::smooth.basis(eval, grid[ind], new_basis)$fd
    eval <- fda::eval.fd(grid, fdm)
    eval[abs(eval) < 10 ^ -6] = 0
    ind <- which(eval >= 0)
    eval <- eval[ind]
    new_basis <- fda::create.bspline.basis(range(eval), nbasis = 2, norder =
                                        2)
    u_fd <- fda::smooth.basis(eval, grid[ind], new_basis)$fd
  }
  # plot_lfd(h_inv,xlim=c(0,100))
  # graphics::lines(l_fd,col=1,lwd=3)
  # graphics::lines(u_fd,col=2,lwd=3)

  out <- list(
    u_fd = u_fd,
    l_fd = l_fd,
    fdp = fdp,
    fdm = fdm
  )
  return(out)
}
get_point_pen <- function(h_fd) {
  point_y <- base::mean(fda::eval.fd(0, h_fd))
  point_end <- base::mean(fda::eval.fd(1, h_fd))
  x_new <- -point_y * (1) / (point_end - point_y)
  if (point_y > 0) {
    point <- c(x_new, 0)
  }
  else{
    point <- c(0, point_y)
  }
  return(point)
}
Phase_I_OEBFDTW_rt<-function(x_i,template_fd,seq_x=seq(0.1,5,length.out = 5),u_fd,l_fd,seq_t,
                          par.rtr=par.rtr(),PLOTRT=FALSE,trace=FALSE,get_mod_list=FALSE,...){


  delta_t <- seq_t[2] - seq_t[1]
  h_list <- x_reg_list <- mod_list <- h_list_raw <- list()
  range_x <-
    if (methods::is(x_i, "fd"))
      x_i$basis$rangeval
  else
    range(x_i[[2]])
  range_tem <- template_fd$basis$rangeval
  delta_d <- par.rtr$delta_d
  delta_v <- par.rtr$delta_v
  iter_der_min <- par.rtr$iter_der_min
  delta_c <- par.rtr$delta_c
  length_grid_window <- par.rtr$length_grid_window
  length_grid_owindow <- par.rtr$length_grid_owindow
  eval_seq_der <- par.rtr$eval_seq_der * abs(diff(range_tem))
  end_h <- 0
  cent <- t_x_old <- 10
  der_h <- point_end <- 10
  point_end_old <- 10
  iter_der <- 0
  start <- 0
  delta_x <- seq_x[2] - seq_x[1]
  for (ii in 1:length(seq_x)) {
    # print(ii)
    if (trace)
      print(paste("x=", round(seq_x[ii], digits = 2)))
    cond <-
      if (methods::is(x_i, "fd"))
        FALSE
    else
      length(which(x_i[[2]] <= seq_x[ii])) < 2
    if (cond) {
      if (trace)
        print("underlimit")
      h_list[[ii]] <- x_reg_list[[ii]] <- "underlimit"
    }
    else{
      inf_lim <- if (ii == 1)
        0
      else
        seq_x[ii - 1]
      if (range_x[2] < seq_x[ii] & range_x[2] <= inf_lim) {
        if (trace)
          print("overlimit")
        h_list[[ii]] <- x_reg_list[[ii]] <- "overlimit"
        if (PLOTRT == TRUE) {
          if (start != 1)
            graphics::points(c(st, end), c(seq_x[ii], seq_x[ii]))
        }
      }
      else{
        if (range_x[2] < seq_x[ii] & range_x[2] > inf_lim) {
          if (trace)
            print("Last point")
          t_x <- range_x[2]
        }
        else{
          t_x <- seq_x[ii]
        }
        range_u <- u_fd$basis$rangeval
        u_ti <-
          if (t_x < range_u[1])
            0
        else if (t_x > range_u[2])
          1
        else
          as.numeric(fda::eval.fd(t_x, u_fd))
        range_l <- l_fd$basis$rangeval
        l_ti <-
          if (t_x < range_l[1])
            0
        else if (t_x > range_l[2])
          1
        else
          as.numeric(fda::eval.fd(t_x, l_fd))
        if (u_ti <= min(seq_t))
          u_ti  <-  seq_t[2]
        if (start == 0) {
          st  <-  l_ti
          end  <-  u_ti
          length_grid  <-  length_grid_owindow
          start  <-  1
        }
        else{
          start  <-  2
          cent_old  <-  cent
          cent <- (t_x - t_x_old + der_h * end_h) / der_h
          if (abs(der_h - der_h_old) / abs(der_h_old) <= delta_d &
              abs(point_end - point_end_old) <= delta_v) {
            iter_der  <-  iter_der + 1
            if (iter_der > iter_der_min) {
              st  <-  min(max(cent - delta_c, range_tem[1]), range_tem[2])
              end  <-  min(cent + delta_c, range_tem[2])
              length_grid  <-  length_grid_window
              if (st == end & st == 1)
                st  <-  0.9 * range_tem[2]
            }
            else{
              st  <-  l_ti
              end  <-  u_ti
              length_grid  <-  length_grid_owindow
            }
          }
          else{
            st  <-  l_ti
            end  <-  u_ti
            iter_der  <-  0
            length_grid  <-  length_grid_owindow
          }
          point_end_old  <-  cent
        }
        seq_t_tem  <-  unique(seq_t[which(seq_t >= st &
                                         seq_t <= end)][seq(1, length(seq_t[which(seq_t >= st &
                                                                                    seq_t <= end)]), length.out = length_grid)])
        if (is.na(seq_t_tem)[1])
          seq_t_tem  <-  fda::eval.fd(max(x_i[[2]][which(x_i[[2]] <= t_x)]), u_fd)
        mod_rt <-
          OEBFDTW_rt_iter(
            x_i = x_i,
            template_fd = template_fd,
            t_x = t_x,
            seq_t_tem =   seq_t_tem,
            seq_t = seq_t,
            end_h_old = end_h,
            l_ti = l_ti,
            u_ti = u_ti,
            ...
          )

        end_h  <-  mod_rt$mod_opt$mod$h_fd$basis$rangeval[2]
        der_h_old  <-  der_h
        grid_der <- eval_seq_der
        eval_poi <-
          c(end_h, sapply(grid_der, function(ii)
            max(end_h - ii, mod_rt$mod_opt$mod$h_fd$basis$rangeval[1])))

        eval_poi <- unique(eval_poi)
        den <- abs(eval_poi[1] - eval_poi[2:length(eval_poi)])
        eval_diff <- fda::eval.fd(eval_poi, mod_rt$mod_opt$mod$h_fd)
        diff <- abs(eval_diff[1] - eval_diff[2:length(eval_diff)])
        der_h_i <- diff / den
        der_h  <-  stats::median(der_h_i)
        point_end <- end_h

        t_x_old  <-  t_x
        h_list[[ii]] <- mod_rt$mod_opt$mod$h_fd
        h_list_raw[[ii]] <-
          mod_rt$mod_opt$mod$h_list[[length(mod_rt$mod_opt$mod$h_list)]]
        x_reg_list[[ii]] <- mod_rt$mod_opt$mod$x_reg_fd
        mod_list[[ii]] <- if (get_mod_list)
          mod_rt
        else
          NULL
        if (PLOTRT == TRUE) {
          if (start == 1)
            plot(
              h_list_raw[[ii]],
              type = "l",
              ylim = c(0, max(seq_x)),
              xlim = c(0, 1)
            )
          else
            graphics::lines(h_list_raw[[ii]])
          graphics::points(seq_t_tem, rep(seq_x[ii], length(seq_t_tem)), cex = 0.2)

        }
      }
    }
  }
  out <- list(
    x_reg_list = x_reg_list,
    h_list = h_list,
    h_list_raw = h_list_raw,
    mod_list = mod_list
  )
  return(out)

}
OEBFDTW_rt_iter <-
  function(x_i,
           template_fd,
           h_fd_true = NULL,
           t_x,
           seq_t_tem = seq(0.4, 0.65, length.out = 5),
           end_h_old,
           g_iter = 2,
           seq_t = NULL,
           l_ti,
           u_ti,
           ...) {
    range_tem <- template_fd$basis$rangeval
    out <-
      OEBFDTW_rt(
        x_i = x_i,
        template_fd,
        t_x = t_x,
        seq_t_tem = seq_t_tem,
        seq_t = seq_t,
        ...
      )
    fit <- out$fit_vec
    t_opt <- seq_t_tem[which.min(fit)]
    out <-
      OEBFDTW_rt(
        x_i = x_i,
        template_fd,
        t_x = t_x,
        seq_t_tem = t_opt,
        seq_t = seq_t,
        x_fd_r = out$x_fd_tx,
        der_x_fd_r = out$der_x_fd_r,
        get_fd = "x_reg",
        ...
      )
    return(out)
  }
OEBFDTW_rt <-
  function(x_i,
           template_fd,
           t_x,
           seq_t_tem = seq(0.4, 0.65, length.out = 5),
           seq_t = NULL,
           x_fd_r = NULL,
           der_x_fd_r = NULL,
           n_basis_x = 20,
           point_pen = NULL,
           frac_end = 0.1,
           perc_basis_x = 0.3,
           ...) {
    fit_vec <- end_x <- alpha_opt <- end_h <- numeric()
    FDTW_output_list_opt <- h_list <- list()
    if (is.null(x_fd_r)) {
      grid_x_t <- x_i[[2]]
      grid_x <- grid_x_t[which(grid_x_t <= t_x)]
      dom_x <- range(grid_x)
      x_value_i <- x_i[[1]][which(grid_x_t <= t_x)]
      npoints <- length(x_value_i)
      if (npoints < 12) {
        basis_x <-
          fda::create.bspline.basis(
            c(dom_x[1], t_x),
            nbasis = min(npoints, max(2, round(
              perc_basis_x * npoints
            ))),
            norder = min(npoints, 2)
          )
        x_fd_r <- fda::smooth.basis(grid_x, x_value_i, basis_x)$fd
      }
      else{
        x_fd_r <-
          get_fd_smooth(x_value_i, grid_x, min(n_basis_x, max(6, round(
            perc_basis_x * npoints
          ))), plot = FALSE)
      }
      der_x_fd_r <- fda::deriv.fd(x_fd_r, 1)
    }
    dom_x <- x_fd_r$basis$rangeval
    for (kkk in 1:length(seq_t_tem)) {
      dom_tem <- template_fd$basis$rangeval
      seq_tem <- seq(dom_tem[1], seq_t_tem[kkk], length.out = n_basis_x *
                       6)
      eval_fd_tem <- fda::eval.fd(template_fd, seq_tem)
      template_fd_r <-
        get_fd_smooth(eval_fd_tem, seq_tem, nbasis_max = n_basis_x)
      der_template_fd_r <- fda::deriv.fd(template_fd_r, 1)
      dom_tem_r <- template_fd_r$basis$rangeval
      if (seq_t_tem[kkk] != dom_tem[2]) {
        dots <- list(...)
        delta_ye <- 0
      }
      else{
        dots <- list(...)
        delta_ye <- frac_end * abs(diff(dom_x))
      }
      delta_xe <- frac_end * abs(diff(dom_tem_r))
      if (is.null(point_pen))
        point_pen <- c(0, 0)
      der_0 <- (dom_x[2] - point_pen[2]) / (dom_tem_r[2] - point_pen[1])
      mod_alpha_opt <-
        OEBFDTW(
          x_fd_r,
          template_fd_r,
          der_x_fd_r ,
          der_template_fd_r ,
          seq_t = seq_t,
          delta_ye = delta_ye,
          delta_xe = delta_xe,
          der_0 = der_0,
          n_basis_x = n_basis_x,
          ...
        )
      end_h[kkk] <-
        max(mod_alpha_opt$mod$h_list[[length(mod_alpha_opt$mod$h_list)]][, 1])
      end_x[kkk] <-
        max(mod_alpha_opt$mod$h_list[[length(mod_alpha_opt$mod$h_list)]][, 2])
      alpha_opt[kkk] <- mod_alpha_opt$mod$alpha
      fit_vec[kkk] <- mod_alpha_opt$obj_opt
      FDTW_output_list_opt[[kkk]] <- mod_alpha_opt
      h_list[[kkk]] <- mod_alpha_opt$mod$h_fd
    }
    ind_min <- which.min(fit_vec)
    mod_opt <- FDTW_output_list_opt[[ind_min]]
    out <- list(
      mod_opt = mod_opt,
      x_fd_tx = x_fd_r,
      template_fd_tt = template_fd_r,
      fit_vec = fit_vec,
      end_x = end_x,
      end_h = end_h,
      h_list = h_list,
      der_x_fd_r = der_x_fd_r
    )
    return(out)
  }


# mFPCA function ---------------------------------------------------------------------
fnMatSqrt <- function(mA) {
  ei  <-  eigen(mA)
  d  <-  ei$values
  d  <-  (d + abs(d)) / 2
  d2  <-  sqrt(d)
  d2[d == 0]  <-  0
  return(ei$vectors %*% diag(d2) %*% t(ei$vectors))
}
tra_warp <- function(h_fd, type = "clog") {
  range <- h_fd$basis$rangeval
  grid_eval <- seq(range[1], range[2], length.out = 300)
  delta_g <- 1 / 300
  basis <-
    fda::create.bspline.basis(range,
                         norder = min(4, h_fd$basis$nbasis),
                         nbasis = h_fd$basis$nbasis)
  eval_fd <- fda::eval.fd(grid_eval, h_fd)
  eval_fd[eval_fd < 0] = 10 ^ -8
  if (type == "log")
    tr_eval <- log10(eval_fd)
  if (type == "clog") {
    minu <-
      if (dim(eval_fd)[2] > 1)
        matrix(rep((1 / abs(diff(
          range
        ))) * (delta_g * apply(
          log10(eval_fd), 2, sum
        )), dim(eval_fd)[1]), dim(eval_fd)[1], dim(eval_fd)[2], byrow = TRUE)
    else
      (1 / abs(diff(range))) * (delta_g * sum(log10(eval_fd)))
    tr_eval <- log10(eval_fd) - minu
  }
  fd_tr <- fda::smooth.basis(grid_eval, tr_eval, basis)$fd
  return(fd_tr)
}


#' @title Mixed Functional Principal Component Analysis (mFPCA)
#' @description This function implements the mFPCA.
#' @param x_fd  An object of class fd corresponding to the registered functions.
#' @param h_fd   An object of class fd corresponding to the warping functions.
#' @param k_weights The vector of the four constants in the inner product computation. If "equal", the  choice of Centofanti et al. (2024) is used.
#' @param ncom It is the way to select the number of principal components. If "ptv", it is selected considering the  percentage of the total variability explained.
#' If "kaiserrule", it is  selected considering the Kaiser rule. The number of principal components may be indicated directly as an integer as well.
#' @param par_ncom If \code{ncom="ptv"}, the threshold for the percentage of the total variability explained. If \code{ncom="kaiserrule"}, the threshold for the Kaiser rule. Otherwise, this parameter is not considered.
#' @references
#' Centofanti, F., A. Lepore, M. Kulahci, and M. P. Spooner (2024).
#' Real-time monitoring of functional data. \emph{Journal of Quality Technology}, 57(2):135--152,
#' doi:https://doi.org/10.1080/00224065.2024.2430978.
#'
#' @return   A list containing the following arguments:
#'
#' \code{eigfun_fd} A List of functions corresponding to the functional part of the principal components.
#'
#' \code{eigvect_sc} A matrix corresponding to the scalar part of the principal components.
#'
#' \code{scores} Scores corresponding to \code{x_fd} and \code{h_fd}.
#'
#' \code{values} Eigenvalues corresponding to the selected principal components.
#'
#' \code{varprop} Variance proportion explained by each principal component.
#'
#' \code{k_weights} The vector of the four constants in the inner product computation.
#'
#' \code{x_fd_list} A List of two elements: the list of the registered functions and the list of the centered log-ratio transformation of
#'  the first derivatives of the normalized warping functions.
#'
#' \code{sc_mat} Two column matrix corresponding to the scalar part of the observations used.
#'
#' \code{mean_fd_list} Mean functions of the  functional part.
#'
#' \code{mean_sc_mat}  Means  of the  scalar part.
#'
#' \code{sd_fd_list} The standard deviation of the functional part.
#'
#' \code{sd_sc_mat} Standard deviations  of the scalar part.
#'
#' \code{h_fd} An object of class fd corresponding to the warping functions.
#'
#' \code{x_fd} An object of class fd corresponding to the registered functions.

#' \code{ind_var} Additional parameter  used in \code{FRTM_PhaseI}.
#' @export
#' @examples
#' library(funcharts)
#'
#' data <- simulate_data_FRTM(n_obs = 100)
#' X <- sapply(1:100, function(ii)
#'   data$x_true[[ii]])
#' x_fd <-
#'   fda::smooth.basis(y = X,
#'                     argvals =  data$grid,
#'                     fda::create.bspline.basis(c(0, 1), 30))$fd
#' H <- sapply(1:100, function(ii)
#'   data$h[[ii]])
#' h_fd <-
#'   fda::smooth.basis(y = H,
#'                     argvals =  data$grid,
#'                     fda::create.bspline.basis(c(0, 1), 30))$fd
#' mod_mFPCA <- mFPCA(x_fd, h_fd, ncom = "ptv", par_ncom = 0.95)
#' plot(mod_mFPCA)
#'
mFPCA <-
  function(x_fd,
           h_fd = NULL,
           k_weights = "equal",
           ncom = "ptv",
           par_ncom = 0.9) {
    range <- x_fd$basis$rangeval
    if (!is.null(h_fd)) {
      F_0 <- fda::eval.fd(range[1], h_fd)
      F_1 <- fda::eval.fd(range[2], h_fd)
      F_0_fd <- fda::fd(F_0, fda::create.constant.basis(range))
      F_1_fd <- fda::fd(F_1, fda::create.constant.basis(range))
      diff <- (F_1 - F_0)
      diff[which(diff == 0)]  <-  1
      F_10inv_fd <- fda::fd((diff) ^ -1, fda::create.constant.basis(range))
      F_10_fd <- fda::fd((F_1 - F_0), fda::create.constant.basis(range))

      max_h  <-  max(F_1 - F_0)
      delta_xamp  <-  max(x_fd$coefs) - min(x_fd$coefs)
      h_fd_s <-  (h_fd - F_0_fd) * F_10inv_fd
      h_s_tr <- tra_warp(fda::deriv.fd(h_fd_s), type = "clog")
      sc_mat <-  cbind(t(F_0), t(log10(F_1 - F_0)))
      if (k_weights == "equal") {
        weights <- rep(1, 4)
        x_fd_list <- list(x_fd, h_s_tr)
        domains <- c(abs(diff(range)), abs(diff(range)), 1, 1)
        domain_weights <- 1 / domains
        weights  <-  domain_weights * weights
        length_var <- length(which(weights[-1] != 0))
        den <- if (length_var == 0)
          1
        else
          length_var
        weights[-1]  <-  (1 / den) * weights[-1]
        ind_var <- which(weights != 0)
        weights  <-  weights[ind_var]
      }
      else{
        weights  <-  k_weights
      }

    }
    else{
      weights <- 1
      if (all(apply(x_fd$coefs, 1, stats::sd) < 10 ^ -10))
        weights[1]  <-  0
      h_s_tr  <-  sc_mat  <-  h_fd_s  <-  F_0_fd  <-  NULL
      x_fd_list <- list(x_fd)
      domains <- c(abs(diff(range)))
      domain_weights <- 1 / domains
      weights  <-  domain_weights * weights
      ind_var  <-  1
    }
    pca <-
      fpc_hy(
        x_fd_list,
        sc_mat = sc_mat,
        weights = weights,
        ncom = ncom,
        par_ncom = par_ncom
      )
    pca$h_fd  <-  h_fd
    pca$x_fd  <-  x_fd
    pca$ind_var  <-  ind_var
    class(pca)  <-  "mFPCA"
    return(pca)

  }
fpc_hy <-
  function(x_fd_list,
           sc_mat = NULL,
           weights = NULL,
           ncom = "kaiserrule",
           par_ncom = 0.25) {
    type  <-  "std"
    if (fda::is.fd(x_fd_list))
      x_fd_list <- list(x_fd_list)
    n_fd_var <- length(x_fd_list)
    if (n_fd_var > 1)
      h_old <- x_fd_list[[2]]
    else
      h_old <- NULL
    nscalvar <- if (!is.null(sc_mat))
      dim(sc_mat)[2]
    else
      0
    nobs <- dim(x_fd_list[[1]]$coefs)[2]
    mean_fd_list <-
      lapply(1:n_fd_var, function(ii)
        fda::mean.fd(x_fd_list[[ii]]))
    sd_fd_list <-
      lapply(1:n_fd_var, function(ii)
        new_sd.fd(x_fd_list[[ii]]))
    for (ii in 1:n_fd_var)
      if (all(sd_fd_list[[ii]]$coefs < 10 ^ -5))
        sd_fd_list[[ii]]$coefs[sd_fd_list[[ii]]$coefs == 0]  <-  1
    mean_sc_mat <- if (!is.null(sc_mat))
      colMeans(sc_mat)
    else
      NULL
    sd_sc_mat <-
      if (!is.null(sc_mat))
        apply(sc_mat, 2, stats::sd)
    else
      NULL
    sd_sc_mat[sd_sc_mat < 10 ^ -5]  <-  1
    if (type != "std") {
      for (ii in 1:n_fd_var) {
        mean_fd_list[[ii]]$coef  <-  0
        sd_fd_list[[ii]]$coef  <-  1
      }
      mean_sc_mat <-
        if (!is.null(sc_mat))
          rep(0, length(mean_sc_mat))
      else
        NULL
      sd_sc_mat <-
        if (!is.null(sc_mat))
          rep(1, length(sd_sc_mat))
      else
        NULL
    }
    if (!is.null(sc_mat) &
        all(sd_sc_mat == 0))
      sd_sc_mat <- rep(1, length(sd_sc_mat))
    x_fd_list <-
      lapply(1:n_fd_var, function(ii)
        new_mul.fd(fda::center.fd(x_fd_list[[ii]]), new_sqrt.fd(sd_fd_list[[ii]], -1)))
    if (!is.null(sc_mat)) {
      sc_mat_n <- scale(sc_mat, scale = TRUE)
      if (sum(is.na(sc_mat_n)) != 0)
        sc_mat <- scale(sc_mat, scale = FALSE)
      else
        sc_mat <- sc_mat_n
    }
    weight_fun  <-  NULL
    if (!is.null(weight_fun)) {
      x_fd_list[[2]] <- new_mul.fd2(x_fd_list[[2]], weight_fun)
      if (!is.null(sc_mat)) {
        sc_mat  <-  sc_mat * matrix(c(
          fda::eval.fd(weight_fun$basis$rangeval[1], weight_fun),
          fda::eval.fd(weight_fun$basis$rangeval[2], weight_fun)
        ),
        dim(sc_mat)[1],
        dim(sc_mat)[2],
        byrow = TRUE)
      }
      weight_new <- new_sd.fd(x_fd_list[[2]])
      new_sc_mat_sd  <-  c(
        fda::eval.fd(weight_fun$basis$rangeval[1], weight_fun),
        fda::eval.fd(weight_fun$basis$rangeval[2], weight_fun)
      )
    }
    else{
      new_sc_mat_sd  <-  weight_new  <-  NULL
    }

    if (!is.null(weights)) {
      if (length(weights) != n_fd_var + nscalvar)
        stop("Wrong weight vector dimension!")
    }
    if (is.null(weights)) {
      weights  <-  rep(1, n_fd_var + nscalvar)
      for (ii in 1:n_fd_var)
        weights[ii] <- 1 / abs(diff(x_fd_list[[ii]]$basis$rangeval))
    }
    weights[weights > 10 ^ 4] <- 10 ^ 4
    c_fd <-
      t(do.call(rbind, lapply(1:n_fd_var, function(ii)
        x_fd_list[[ii]]$coefs)))
    N <- dim(c_fd)[1]
    basis_list <- lapply(1:n_fd_var, function(ii)
      x_fd_list[[ii]]$basis)
    W_list <-
      lapply(1:n_fd_var, function(ii)
        fda::eval.penalty(basis_list[[ii]]))
    nbasis_i <- sapply(1:n_fd_var, function(ii)
      dim(W_list[[ii]])[1])
    nbasis <- sum(nbasis_i)
    if (!is.null(sc_mat))
      Id <- diag(nscalvar)
    dim_aug <- if (!is.null(sc_mat))
      nbasis + nscalvar
    else
      nbasis
    we_fd <-
      unlist(lapply(1:length(nbasis_i), function(ii)
        rep(weights[ii], nbasis_i[ii])))
    we_sc <-
      if (!is.null(sc_mat))
        weights[(n_fd_var + 1):(n_fd_var + nscalvar)]
    else
      NULL
    weigth_mat <- diag(c(we_fd, we_sc))

    W_star <- matrix(0, dim_aug, dim_aug)
    for (ii in 1:n_fd_var) {
      if (ii == 1)
        W_star[1:nbasis_i[1], 1:nbasis_i[1]] <- W_list[[ii]]
      else
        W_star[(1 + sum(nbasis_i[1:(ii - 1)])):(sum(nbasis_i[1:(ii - 1)]) +
                                                  nbasis_i[ii]), (1 + sum(nbasis_i[1:(ii - 1)])):(sum(nbasis_i[1:(ii - 1)]) +
                                                                                                    nbasis_i[ii])] <- W_list[[ii]]
    }

    if (!is.null(sc_mat))
      W_star[(nbasis + 1):dim_aug, (nbasis + 1):dim_aug] <- Id
    W_star <- (W_star + t(W_star)) / 2

    W_star <- W_star %*% weigth_mat
    W_sr <- fnMatSqrt(W_star)
    W_sr <- (W_sr + t(W_sr)) / 2
    C_aug <- if (!is.null(sc_mat))
      cbind(c_fd, sc_mat)
    else
      c_fd
    mat <- N ^ (-1) * W_sr %*% t(C_aug) %*% C_aug %*% W_sr
    mat <- (mat + t(mat)) / 2
    eig <- eigen(mat)
    eigvalues <- eig$values
    eigvect <- MASS::ginv(W_sr) %*% eig$vectors
    cumsum_eig <- cumsum(eigvalues) / sum(eigvalues)
    if (ncom == "ptv") {
      K <- which(cumsum_eig <= par_ncom)[length(which(cumsum_eig <= par_ncom))]
      if (length(K) == 0)
        K  <-  1
    }
    else if (ncom == "kaiserrule") {
      K <- which(eigvalues >= par_ncom)[length(which(eigvalues >= par_ncom))]
      if (length(K) == 0)
        K <- 1
    }
    else if (is.integer(ncom)) {
      K <- ncom
    }
    varprop <- eigvalues / sum(eigvalues)
    eigvect_fd_list <- list()
    for (ii in 1:n_fd_var) {
      if (ii == 1)
        eigvect_fd_list[[ii]] <- eigvect[1:nbasis_i[1], 1:K]
      else
        eigvect_fd_list[[ii]] <-
          eigvect[(1 + sum(nbasis_i[1:(ii - 1)])):(sum(nbasis_i[1:(ii - 1)]) + nbasis_i[ii]), 1:K]
    }
    eigfun_fd_list <-
      lapply(1:n_fd_var, function(ii)
        fda::fd(eigvect_fd_list[[ii]], basis_list[[ii]]))
    eigvect_sc <-
      if (!is.null(sc_mat))
        eigvect[(nbasis + 1):dim_aug, 1:K]
    else
      NULL
    if (!is.null(eigvect_sc) & nscalvar == 1)
      eigvect_sc  <-  t(eigvect_sc)
    scores_list <- list()
    for (ii in 1:n_fd_var) {
      scores_list[[ii]] <-
        inprod2(eigfun_fd_list[[ii]], x_fd_list[[ii]]) * weights[ii]
    }
    score_fd <- t(do.call("+", scores_list))
    if (!is.null(sc_mat)) {
      if (length(we_sc) == 1)
        score_sc <- sc_mat %*% we_sc %*% eigvect_sc
      else
        score_sc <- sc_mat %*% diag(we_sc) %*% eigvect_sc
    }
    scores <- if (!is.null(sc_mat))
      score_fd + score_sc
    else
      score_fd
    fit <-
      fit_pca_hy(
        x_fd_list,
        sc_mat,
        eigfun_fd_list,
        weights,
        eigvect_sc,
        mean_fd_list,
        mean_sc_mat,
        sd_fd_list,
        sd_sc_mat,
        weight_new,
        new_sc_mat_sd
      )
    fit_list_norm  <-  fit$fit_list_norm
    sc_fit_norm  <-  fit$sc_fit_norm
    fit <- fit$fit
    out <- list(
      eigfun_fd = eigfun_fd_list,
      eigvect_sc = eigvect_sc,
      scores = scores,
      values = eigvalues[1:K],
      varprop = varprop,
      k_weights = weights,
      mean_fd_list = mean_fd_list,
      mean_sc_mat = mean_sc_mat,
      sd_fd_list = sd_fd_list,
      sd_sc_mat = sd_sc_mat,
      x_fd_list = x_fd_list,
      sc_mat = sc_mat
    )

    return(out)
  }
fit_pca_hy <-
  function(x_fd_list,
           sc_mat,
           eigfun_fd_list,
           weights,
           eigvect_sc,
           mean_fd_list = NULL,
           mean_sc_mat = NULL,
           sd_fd_list = NULL,
           sd_sc_mat = NULL,
           weight_fun = NULL,
           new_sc_mat_sd = NULL) {
    n_fd_var <- length(x_fd_list)
    nscalvar <- if (!is.null(sc_mat))
      dim(sc_mat)[2]
    else
      0
    if (is.null(mean_fd_list))
      mean_fd_list <-
      lapply(1:n_fd_var, function(ii)
        fda::mean.fd(x_fd_list[[ii]]))
    if (is.null(mean_sc_mat))
      if (!is.null(sc_mat))
        mean_sc_mat <- colMeans(sc_mat)
    scores_list <- list()
    for (ii in 1:n_fd_var) {
      scores_list[[ii]] <-
        inprod2(eigfun_fd_list[[ii]], x_fd_list[[ii]]) * weights[ii]
    }
    score_fd <- t(do.call("+", scores_list))
    we_sc <-
      if (!is.null(sc_mat))
        weights[(n_fd_var + 1):(n_fd_var + nscalvar)]
    else
      NULL
    if (!is.null(sc_mat)) {
      if (length(we_sc) == 1)
        score_sc <- sc_mat %*% we_sc %*% eigvect_sc
      else
        score_sc <- sc_mat %*% diag(we_sc) %*% eigvect_sc
    }
    scores <- if (!is.null(sc_mat))
      score_fd + score_sc
    else
      score_fd
    fit_list_norm <-
      lapply(1:n_fd_var, function(ii)
        fda::fd(eigfun_fd_list[[ii]]$coefs %*% t(scores), eigfun_fd_list[[ii]]$basis))
    fit_list = fit_list_norm
    if (!is.null(sc_mat)) {
      sc_fit_norm <-
        if (is.null(dim(eigvect_sc)))
          scores %*% eigvect_sc
      else
        scores %*% t(eigvect_sc)
      sc_fit  <-  sc_fit_norm
    }
    else{
      sc_fit_norm  <-  NULL
    }
    if (!is.null(weight_fun)) {
      if (!all(fit_list[[2]]$coefs == 0)) {
        fit_list[[2]] <- new_mul.fd3(fit_list[[2]], weight_fun)
        if (!is.null(sc_mat)) {
          if (new_sc_mat_sd[1] < 10 ^ -2)
            sc_fit[, 1]  <-  0
          else
            sc_fit[, 1]  <-  sc_fit[, 1] / new_sc_mat_sd[1]
          if (new_sc_mat_sd[2] < 10 ^ -2)
            sc_fit[, 2]  <-  0
          else
            sc_fit[, 2]  <-  sc_fit[, 2] / new_sc_mat_sd[2]
        }
      }
    }
    for (ii in 1:n_fd_var) {
      fit_list[[ii]] <-
        fda::fd(matrix(rep(mean_fd_list[[ii]]$coefs, dim(scores)[1]), ncol = dim(scores)[1]), mean_fd_list[[ii]]$basis) +
        new_mul.fd(fit_list[[ii]], sd_fd_list[[ii]])
    }
    if (!is.null(sc_mat)) {
      sc_fit <-
        if (is.null(dim(eigvect_sc)))
          sc_fit * matrix(sd_sc_mat, dim(scores)[1], nscalvar, byrow = TRUE) + matrix(mean_sc_mat, dim(scores)[1], nscalvar, byrow =
                                                                                        TRUE)
      else
        sc_fit * matrix(sd_sc_mat, dim(scores)[1], nscalvar, byrow = TRUE) + matrix(mean_sc_mat, dim(scores)[1], nscalvar, byrow =
                                                                                      TRUE)
    }
    if (!is.null(sc_mat))
      scores  <-  list(scores_fd = scores_list, scores_sc = sc_mat)
    else
      scores = scores_list
    if (!is.null(sc_mat))
      fit  <-  list(fit_fd = fit_list, fit_sc = sc_fit)
    else
      fit  <-  fit_list
    out <- list(
      scores = scores,
      fit = fit,
      fit_list_norm = fit_list_norm,
      sc_fit_norm = sc_fit_norm
    )
    return(out)
  }
# Control charts functions ------------------------------------------------

get_scores_pcahy <- function(x_fd,
                             h_fd = NULL,
                             mod_pca,
                             type = "std") {
  if (!is.null(h_fd)) {
    range <- h_fd$basis$rangeval
    eval_seq <- seq(range[1], range[2], length.out = 200)
    F_0 <- fda::eval.fd(range[1], h_fd)
    F_1 <- fda::eval.fd(range[2], h_fd)
    F_0_fd <- fda::fd(F_0, fda::create.constant.basis(range))
    F_1_fd <- fda::fd(F_1, fda::create.constant.basis(range))
    F_10inv_fd <- fda::fd((F_1 - F_0) ^ -1, fda::create.constant.basis(range))
    F_10_fd <- fda::fd((F_1 - F_0), fda::create.constant.basis(range))
    h_fd_s <-  (h_fd - F_0_fd) * F_10inv_fd
    h_s_tr <- tra_warp(fda::deriv.fd(h_fd_s), type = "clog")
    sc_mat <- cbind(t(F_0), t(log10(F_1 - F_0)))
    x_fd_list <- list(x_fd, h_s_tr)
  }
  else{
    x_fd_list <- list(x_fd)
    sc_mat <- h_s_tr <- NULL
  }
  eigfun_fd_list <- mod_pca$eigfun_fd
  weights <- mod_pca$k_weights
  eigvect_sc <- mod_pca$eigvect_sc
  ind_var <- mod_pca$ind_var
  ind_var_fd <- ind_var[which(ind_var <= 2)]
  x_fd_list <- x_fd_list[ind_var_fd]
  ind_var_sc <- ind_var[which(ind_var > 2)] - 2
  sc_mat <- if (length(ind_var_sc) > 0)
    t(sc_mat[, ind_var_sc])
  else
    NULL
  # weight_new=mod_pca$weight_new
  new_sc_mat_sd <- mod_pca$new_sc_mat_sd
  mean_fd_list <- mod_pca$mean_fd_list
  sd_fd_list <- mod_pca$sd_fd_list
  mean_sc_mat <- mod_pca$mean_sc_mat
  sd_sc_mat <- mod_pca$sd_sc_mat
  weight_fun <- mod_pca$weight_fun
  n_fd_var <- length(mod_pca$x_fd_list)
  nscalvar <- if (!is.null(sc_mat))
    dim(mod_pca$sc_mat)[2]
  else
    0
  x_fd_list_cen <-
    lapply(1:n_fd_var, function(ii)
      new_mul.fd(
        center_fd(x_fd_list[[ii]], mean_fd_list[[ii]]),
        new_sqrt.fd(sd_fd_list[[ii]], -1)
      ))
  sc_mat_cen <-
    if (!is.null(sc_mat))
      (sc_mat - matrix(mean_sc_mat, nrow(sc_mat), dim(sc_mat)[2], byrow = TRUE))/matrix(sd_sc_mat,nrow(sc_mat), dim(sc_mat)[2],byrow=TRUE) else NULL
  if(!is.null(weight_fun)){
    x_fd_list_cen[[2]]<-new_mul.fd2(x_fd_list_cen[[2]],weight_fun)
    if(!is.null(sc_mat)){
      sc_mat_cen=sc_mat_cen*matrix(c(fda::eval.fd(weight_fun$basis$rangeval[1],weight_fun),fda::eval.fd(weight_fun$basis$rangeval[2],weight_fun)),dim(sc_mat)[1],dim(sc_mat)[2],byrow=TRUE)
    }
  }
  scores_list<-list()
  for(ii in 1:n_fd_var){
    scores_list[[ii]]<-inprod2(eigfun_fd_list[[ii]],x_fd_list_cen[[ii]])*weights[ii]
  }
  score_fd<-t(do.call("+",scores_list))
  we_sc<-if(!is.null(sc_mat))  weights[(n_fd_var+1):(n_fd_var+nscalvar)] else NULL
  if(!is.null(sc_mat)){
    if(length(we_sc)==1)
      score_sc<-sc_mat_cen%*%we_sc%*%eigvect_sc
    else
      score_sc<-sc_mat_cen%*%diag(we_sc)%*%eigvect_sc
  }
  scores<-if(!is.null(sc_mat))  score_fd+score_sc else score_fd
  fit_list_norm<-lapply(1:n_fd_var, function(ii) {
    fda::fd(eigfun_fd_list[[ii]]$coefs%*%t(scores),eigfun_fd_list[[ii]]$basis)
  })
  fit_list=fit_list_norm
  if(!is.null(sc_mat)){
    sc_fit_norm<- if(is.null(dim(eigvect_sc)))scores%*%eigvect_sc else scores%*%t(eigvect_sc)
    sc_fit=sc_fit_norm

  }
  else{
    sc_fit_norm=NULL
  }
  if(!is.null(weight_fun)){
    if(!all(fit_list[[2]]$coefs==0)){
      fit_list[[2]]<-new_mul.fd3(fit_list[[2]],weight_fun)
      if(!is.null(sc_mat)){
        if(new_sc_mat_sd[1]<10^-2)sc_fit[,1]=0 else sc_fit[,1]=sc_fit[,1]/new_sc_mat_sd[1]
        if(new_sc_mat_sd[2]<10^-2)sc_fit[,2]=0 else sc_fit[,2]=sc_fit[,2]/new_sc_mat_sd[2]


      }
    }

  }

  for(ii in 1:n_fd_var){
    fit_list[[ii]]<-fda::fd(matrix(rep(mean_fd_list[[ii]]$coefs,dim(scores)[1]),ncol=dim(scores)[1]),mean_fd_list[[ii]]$basis)+new_mul.fd(fit_list[[ii]],sd_fd_list[[ii]])

  }
  if(!is.null(sc_mat)){

    sc_fit<- if(is.null(dim(eigvect_sc)))sc_fit*matrix(sd_sc_mat,dim(scores)[1],nscalvar,byrow=TRUE)+matrix(mean_sc_mat,dim(scores)[1],nscalvar,byrow=TRUE) else sc_fit*matrix(sd_sc_mat,dim(scores)[1],nscalvar,byrow=TRUE)+matrix(mean_sc_mat,dim(scores)[1],nscalvar,byrow=TRUE)
  }
  if(!is.null(sc_mat)) fit=list(fit_fd=fit_list,fit_sc=sc_fit) else fit=list(fit_fd=fit_list)
  if(!is.null(sc_mat)) fit=list(fit_fd=fit_list,fit_sc=sc_fit) else fit=list(fit_fd=fit_list)
  out<-list(scores=scores,
            fit=fit,
            sc_mat=sc_mat,
            h_s_tr=h_s_tr,
            k_weights=weights,
            x_fd_list_std=x_fd_list_cen,
            sc_mat_std=sc_mat_cen,
            fit_list_norm=fit_list_norm,
            sc_fit_norm=sc_fit_norm)
  return(out)
}
get_T2SPE <- function(x_fd, h_fd = NULL, mod_pca, ...) {
  weights <- mod_pca$k_weights
  if (length(weights) == 1)
    h_fd <- NULL
  scfit <- get_scores_pcahy(x_fd, h_fd, mod_pca, ...)
  sc_mat <- scfit$sc_mat
  scores <- scfit$scores
  fit <- scfit$fit
  # weight_fun=mod_pca$weight_fun
  fit_fd_list_std <- scfit$fit_list_norm
  fit_sc_mat_std <- scfit$sc_fit_norm
  values <- mod_pca$values
  h_fd_tr <- scfit$h_s_tr
  T_2 <-
    if (length(scores) == 1)
      scores * (values) ^ -1 * scores
  else
    (diag(scores %*% solve(diag(values)) %*% t(scores)))
  if (!is.null(h_fd)) {
    x_fd_diff <-
      lapply(1:length(scfit$x_fd_list_std), function(ii)
        scfit$x_fd_list_std[[ii]] - fit_fd_list_std[[ii]])
    sc_mat_diff <- scfit$sc_mat_std - fit_sc_mat_std
    if (length(sc_mat_diff) == 0)
      sc_mat_diff <- NULL
  }
  else{
    x_fd_diff <- list(scfit$x_fd_list_std[[1]] - fit_fd_list_std[[1]])
    sc_mat_diff <- NULL
  }
  SPE <-
    diag(inprod_ext(x_fd_diff, sc_mat_diff, x_fd_diff, sc_mat_diff, weights =
                      weights))
  out <- list(
    T_2 = T_2,
    SPE = SPE,
    mod_pca = mod_pca,
    scores = scores
  )
  return(out)

}
CL_T2SPE <- function(mat, alpha, type = "pointwise") {
  if (type == "pointwise") {
    CL <- numeric()
    for (ii in 1:dim(mat)[2]) {
      mat_i <- mat[, ii]
      mat_i <- mat_i[!is.na(mat_i)]
      if (length(mat_i) > 1) {
        den <- stats::density(mat[, ii],
                              na.rm = TRUE,
                              bw = "SJ",
                              adjust = 0.25)

        CL[ii] <- spatstat.univar::quantile.density(den, 1 - alpha)
      }
      else if (length(mat_i) == 1) {
        CL[ii] <- mat_i
      }
      else{
        CL[ii] <- NA
      }
    }
  }
  if (type == "Fisher") {
    CL <- numeric()
    for (ii in 1:dim(mat)[2]) {
      mat_i <- mat[, ii]
      mat_i <- mat_i[!is.na(mat_i)]
      if (length(mat_i) > 1) {
        den <- stats::density(mat[, ii],
                              na.rm = TRUE,
                              bw = "SJ",
                              adjust = 0.25)

        CL[ii] <- spatstat.univar::quantile.density(den, 1 - alpha)
      }
      else if (length(mat_i) == 1) {
        CL[ii] <- mat_i
      }
      else{
        CL[ii] <- NA
      }
    }
  }
  if (type == "overall") {
    alpha_i <- c(seq(0.00001, 0.2, length.out = 100))
    fer <- numeric()
    CL_i <- list()
    for (ii in 1:length(alpha_i)) {
      CL_i[[ii]] <- CL_T2SPE(mat, alpha_i[ii], type = "pointwise")
      out_jj <- list()
      for (jj in 1:dim(mat)[2]) {
        out_jj[[jj]] <- which(mat[, jj] >  CL_i[[ii]][jj])
      }
      length(unique(unlist(out_jj))) / dim(mat)[1]
      fer[ii] <- length(unique(unlist(out_jj))) / dim(mat)[1]
    }
    ind <- which(fer < (alpha))[length(which(fer < (alpha)))]
    CL <- CL_i[[ind]]
  }
  return(CL)
}
CL_T2SPE_fd <- function(fd, alpha, type = "pointwise", seq_x) {
  n_obs <- length(fd)
  grid_eval <- seq(seq_x[1], max(seq_x), length.out = 500)
  ind_list <-
    lapply(1:n_obs, function(ii)
      which(
        grid_eval >= fd[[ii]]$basis$rangeval[1] &
          grid_eval <= fd[[ii]]$basis$rangeval[2]
      ))
  grid_list <- lapply(1:n_obs, function(ii)
    grid_eval[ind_list[[ii]]])
  eval_list <-
    lapply(1:n_obs, function(ii)
      fda::eval.fd(grid_list[[ii]], fd[[ii]]))
  mat <- matrix(NA, n_obs, length(grid_eval))
  for (ii in 1:n_obs) {
    mat[ii, ind_list[[ii]]] <- eval_list[[ii]]
  }
  if (type == "pointwise") {
    CL <- numeric()
    for (ii in 1:dim(mat)[2]) {
      mat_i <- mat[, ii]
      mat_i <- mat_i[!is.na(mat_i)]
      if (length(mat_i) > 1) {
        den <-
          try(stats::density(mat_i,
                             na.rm = TRUE,
                             bw = "SJ",
                             adjust = 0.25),
              outFile = "err")
        ;
        if (methods::is(den, "try-error"))
          den <- stats::density(mat_i, na.rm = TRUE)
        CL[ii] <- spatstat.univar::quantile.density(den, 1 - alpha)
      }
      else if (length(mat_i) == 1) {
        CL[ii] <- mat_i
      }
      else{
        CL[ii] <- NA
      }
    }
  }
  if (type == "Fisher") {
    CL <- numeric()
    for (ii in 1:dim(mat)[2]) {
      mat_i <- mat[, ii]
      mat_i <- mat_i[!is.na(mat_i)]
      if (length(mat_i) > 1) {
        den <- stats::density(mat[, ii],
                              na.rm = TRUE,
                              bw = "SJ",
                              adjust = 0.25)

        CL[ii] <- spatstat.univar::quantile.density(den, 1 - alpha)
      }
      else if (length(mat_i) == 1) {
        CL[ii] <- mat_i
      }
      else{
        CL[ii] <- NA
      }
    }
  }
  if (type == "overall") {
    alpha_i <- c(seq(0.00001, 0.2, length.out = 100))
    fer <- numeric()
    CL_i <- list()
    for (ii in 1:length(alpha_i)) {
      CL_i[[ii]] <- CL_T2SPE(mat, alpha_i[ii], type = "pointwise")
      out_jj <- list()
      for (jj in 1:dim(mat)[2]) {
        out_jj[[jj]] <- which(mat[, jj] >  CL_i[[ii]][jj])
      }
      length(unique(unlist(out_jj))) / dim(mat)[1]
      fer[ii] <- length(unique(unlist(out_jj))) / dim(mat)[1]
    }
    ind <- which(fer < (alpha))[length(which(fer < (alpha)))]
    CL <- CL_i[[ind]]
  }
  values <- CL
  ind <- !is.na(values)
  values <- values[ind]
  seq_i <- grid_eval[ind]
  basis <-
    fda::create.bspline.basis(range(seq_i), breaks = seq_i, norder = 2)
  CL_fd <- fda::smooth.basis(seq_i, values, basis)$fd
  return(CL_fd)
}
get_T2SPE_fd <- function(T_2_mat, SPE_2_mat, seq_x) {
  n_obs <- dim(T_2_mat)[1]
  T2_fd <- SPE_fd <- list()
  for (ii in 1:n_obs) {
    values <- T_2_mat[ii, ]
    values_spe <- SPE_2_mat[ii, ]
    ind <- !is.na(values)
    values <- (values)[ind]
    values_spe <- (values_spe)[ind]
    seq_i <- seq_x[ind]
    if (length(seq_i) > 1) {
      basis <- fda::create.bspline.basis(range(seq_i), breaks = seq_i, norder = 2)
      T2_fd[[ii]] <- fda::fd(values, basis)
      SPE_fd[[ii]] <- fda::fd(values_spe, basis)
    }
    else{
      print("seq_x too short!!")
      T2_fd[[ii]] <- SPE_fd[[ii]] <- NULL
      # SPE_fd[[ii]]<-fda::fd(values_spe,basis)
    }

  }
  out <- list(T2_fd = T2_fd,
              SPE_fd = SPE_fd)
  return(out)
}

# Phase I -----------------------------------------------------------------
#' @title Setting open-end/open-begin functional dynamic time warping (OEB-FDTW) defaults
#' @description This is an internal function of package \code{FRTM} which allows controlling the parameters to implement the OEB-FDTW in the FRTM method.
#' @param N  The number \eqn{N_{t}} of evenly spaced values along the template domain \eqn{\mathcal{D}_{Y}}.
#' @param M  The number \eqn{M_{x}} of evenly spaced values along the functional observation domain \eqn{\mathcal{D}_{X_i}}.
#' @param smin  The minimum  values allowed for the first derivative of the warping function \eqn{h_i}. If NULL, in \code{FRTM_PhaseI}, it is set as  0.001 multiplied by the ratio between the  size
#'  of the monitoring and template domains.
#' @param smax  The maximum  values allowed for the first derivative of the warping function \eqn{h_i}. If NULL, in \code{FRTM_PhaseI}, it is set as  100 multiplied by the ratio between the  size
#'  of the monitoring and template domains.
#' @param alpha_vec Grid of values to find the optimal value of \eqn{\alpha_i}.
#' @param frac_oeob  Fraction of \eqn{\mathcal{D}_{Y}} and \eqn{\mathcal{D}_{X_i}} to obtain \eqn{\delta_{t,s}}, \eqn{\delta_{t,e}},\eqn{\delta_{x,s}} and \eqn{\delta_{x,e}}.
#' @param eta Fraction \eqn{\eta} for updating the constraint bounds  to reduce the error associated to the discretization (Deriso and Boyd, 2022).
#' @param iter  Number of iteration in the  iterative refinement to reduce the error associated to the discretization (Deriso and Boyd, 2022).
#' @param template  If "Procrustes", the Procrustes fitting process is used to select the template function. If \code{numeric}, the discrete observations of the template function.
#' @param grid_tem  If \code{template} is \code{numeric}, a vector of time points where the discrete observations of the template function are sampled.
#' @param index_tem  If NULL and \code{template="Procrustes"}, the function in the training set, whose domain length is nearest the median domain length, is chosen as initial estimate of the template function.
#' If an \code{integer}  and \code{template="Procrustes"}, the  \code{index_tem} function in the training set is chosen as initial estimate of the template function.
#' If \code{template} is \code{numeric}, this parameter is not used.
#' @param iter_tem  Number of iterations in the Procrustes fitting process.
#' @param lambda  Grid of smoothing parameters to evaluate the average curve distance (ACD).
#' @param threshold The fraction \eqn{\delta} of the difference between the maximum and the minimum distance in the selection of the smoothing parameter via the ACD.
#' @param seq_t Discretized sequence in the template domain \eqn{\mathcal{D}_{Y}}.
#' @seealso \code{\link{FRTM_PhaseI}}
#' @export
#' @examples
#' library(funcharts)
#' par.FDTW()
#' @references
#' Deriso, D. and S. Boyd (2022).
#'  A general optimization framework for dynamic time warping.
#'  \emph{Optimization and Engineering, 1-22}.
#'
par.FDTW <-
  function(N = 100,
           M = 50,
           smin = NULL,
           smax = NULL,
           alpha_vec = c(0, 0.5, 1),
           frac_oeob = 0.1,
           eta = 0.5,
           iter = 3,
           template = "Procrustes",
           grid_tem = NULL,
           index_tem = NULL,
           iter_tem = 2,
           lambda = c(0, 10 ^ seq(-8, -2, by = 0.25), 10 ^ 5),
           threshold = 0.01,
           seq_t = seq(0.01, 1, length.out = 100)) {
    list(
      N = N,
      M = M,
      smin = smin,
      smax = smax,
      alpha_vec = alpha_vec,
      frac_oeob = frac_oeob,
      eta = eta,
      iter = iter,
      template = template,
      grid_tem = grid_tem,
      index_tem = index_tem,
      iter_tem = iter_tem,
      lambda = lambda,
      threshold = threshold,
      seq_t = seq_t
    )
  }
#' @title Setting real-time registration step defaults
#' @description This is an internal function of package \code{FRTM} which allows controlling the parameters to implement real-time registration step in the FRTM method.
#' @param seq_x  Discretized sequence in the monitoring domain \eqn{\mathcal{D}_{m}}.
#' @param delta_d  The parameter \eqn{\delta_{d}} in the adaptive band constraint calculation.
#' @param delta_v  The parameter \eqn{\delta_{v}} in the adaptive band constraint calculation.
#' @param delta_c  The parameter \eqn{\delta_{c}} in the adaptive band constraint calculation.
#' @param Delta The parameter \eqn{\Delta} in the adaptive band constraint calculation.
#' @param length_grid_window  Number of points to be explored in the interval of the band constraint for each point in \eqn{\mathcal{D}_{m}} when the adaptive band constraint is considered.
#' @param length_grid_owindow Number of points to be explored in the interval of the band constraint for each point in \eqn{\mathcal{D}_{m}} when the original band constraint is considered.
#' @param eval_seq_der   If multiplied by the template domain range, the distances from the domain right boundaries  over which are calculated the first derivative
#' to mitigate the effects of possible estimation errors in the adaptive band constraint calculation.
#' @param perc_basis_x_reg  Multiplied by the number of observed discrete points, it is the number of basis functions used in the real-time registration step for each time point.
#' This latter number cannot be grater than \code{n_basis_xall}.
#' @seealso \code{\link{FRTM_PhaseI}}
#' @export
#' @examples
#' library(funcharts)
#' par.rtr()
#'
par.rtr <-
  function(seq_x = NULL,
           delta_d = 0.05,
           delta_v = 0.03,
           delta_c = 0.04,
           Delta = 0.1,
           length_grid_window = 10,
           length_grid_owindow = 20,
           eval_seq_der = seq(0.02, 0.1, length.out = 10),
           perc_basis_x_reg = 0.3) {
    list(
      seq_x = seq_x,
      delta_d = delta_d,
      delta_v = delta_v,
      delta_c = delta_c,
      Delta = Delta,
      length_grid_window = length_grid_window,
      length_grid_owindow = length_grid_owindow,
      eval_seq_der = eval_seq_der,
      perc_basis_x_reg = perc_basis_x_reg
    )
  }
#' @title Setting mixed functional principal component analysis (mFPCA) defaults
#' @description This is an internal function of package \code{FRTM} which allows controlling the parameters used in the mFPCA in the FRTM method.
#' @param perc_pca  Percentage of the total variability used to select the number \eqn{L} of principal components.
#' @param perc_basis_x_pca  Multiplied by the maximum number of basis of the registered functions for each time point in \eqn{\mathcal{D}_{Y}}, it is the number of basis functions of the registered functions in the mFPCA.
#' @param perc_basis_h  Multiplied by the mean number of basis of the warping functions for each time point in \eqn{\mathcal{D}_{Y}}, it is the number of basis functions of the warping functions in the mFPCA.
#' @seealso \code{\link{FRTM_PhaseI}}
#' @export
#' @examples
#' library(funcharts)
#' par.mFPCA()
par.mFPCA <-
  function(perc_pca = 0.9,
           perc_basis_x_pca = 1,
           perc_basis_h = 0.2) {
    list(
      perc_pca = perc_pca,
      perc_basis_x_pca = perc_basis_x_pca,
      perc_basis_h = perc_basis_h
    )
  }
#' @title Phase I of the FRTM method.
#' @description This function implements the design phase (Phase I) of FRTM method.
#' @param data_tra  A list containing the following arguments: \code{x_err} a list containing the discrete observations for each curve of the training set; \code{grid_i} a list of vector of time points where the curves of the training set are sampled.
#' @param data_tun  A list containing the following arguments: \code{grid_i} a list containing the discrete observations for each curve of the tuning set;
#' \code{grid_i} a list of vector of time points where the curves of the tuning set are sampled. If NULL, the tuning set is not used.
#' @param alpha Overall type I error probability to obtain the  control chart limits.
#' @param n_basis_xall Number of basis to obtain the functional observation via  the spline smoothing approach based on cubic B-splines and a roughness penalty on the second derivative.
#' @param control.FDTW A list of  control parameters for the  open-end/open-begin functional dynamic time warping to replace defaults returned by par.FDTW. Values not set assume default values.
#' @param control.rtr A list of  control parameters for the  real-time registration step to replace defaults returned by par.rtr. Values not set assume default values.
#' @param control.mFPCA A list of  control parameters for the   mixed functional principal component analysis to replace defaults returned by par.mFPCA. Values not set assume default values.
#' @param ncores If \code{ncores}>1, then parallel computing is used, with \code{ncores} cores. Default is 1.
#' @param print If TRUE, some information are printed. Default is TRUE.
#' @seealso \code{\link{FRTM_PhaseI}}
#'
#' @references
#' Centofanti, F., A. Lepore, M. Kulahci, and M. P. Spooner (2024).
#' Real-time monitoring of functional data. \emph{Journal of Quality Technology}, 57(2):135--152,
#' doi:https://doi.org/10.1080/00224065.2024.2430978.
#'
#' @return   A list containing the following arguments:
#'
#' \code{T2_fd} List of \eqn{T^{2}} functions for each observation in the tuning set.
#'
#' \code{SPE_fd} List of SPE functions for each observation in the tuning set.
#'
#' \code{CL_T2} Control limit of  the Hotelling's \eqn{T^{2}} control chart.
#'
#' \code{CL_SPE} Control limit of  the SPE control chart.
#'
#' \code{template_fd} Template function used in the registration.
#'
#' \code{der_template_fd} First derivative of the template function.
#'
#' \code{u_fd} Upper extreme of the  band constraint.
#'
#' \code{l_fd} Lower extreme of the  band constraint.
#'
#' \code{x_list_tun} List, for each observation in the tuning set, of partial registered functions.
#'
#' \code{h_list_tun} List, for each observation in the tuning set, of partial  warping  functions.
#'
#' \code{x_list} List, for each observation in the training set, of partial registered functions.
#'
#' \code{h_list} List, for each observation in the training set, of partial  warping  functions.
#'
#' \code{x_err} A list containing the discrete observations for each curve of the training set.
#'
#' \code{grid_i} A list of vector of time points where the curves of the training set are sampled.
#'
#' \code{x_list_smooth} Smooth curves from the training set.
#'
#' \code{lambda} Lambda identified through the average curve distance to obtain the OEB-FDTW solution.
#'
#' \code{par_reg} Additional parameters to be used in the monitoring phase (Phase II).
#' @export
#' @examples
#' library(funcharts)
#' data <- simulate_data_FRTM(n_obs = 20)
#'
#' data_oc <-
#'   simulate_data_FRTM(
#'     n_obs = 2,
#'     scenario = "1",
#'     shift = "OC_h",
#'     severity = 0.5
#'   )
#'
#' lambda <- 10 ^ -5
#' max_x <- max(unlist(data$grid_i))
#' seq_t_tot <- seq(0, 1, length.out = 30)[-1]
#' seq_x <- seq(0.1, max_x, length.out = 10)
#'
#'
#' \donttest{
#'   mod_phaseI_FRTM <- FRTM_PhaseI(
#'     data_tra =  data,
#'     control.FDTW = list(
#'       M = 30,
#'       N = 30,
#'       lambda = lambda,
#'       seq_t = seq_t_tot,
#'       iter_tem = 1,
#'       iter = 1
#'     ),
#'     control.rtr = list(seq_x = seq_x)
#'   )
#'   mod_phaseII_FRTM <- FRTM_PhaseII(data_oc = data_oc , mod_phaseI = mod_phaseI_FRTM)
#'
#'   plot(mod_phaseI_FRTM)
#'   plot(mod_phaseII_FRTM)
#' }
#'
FRTM_PhaseI <-
  function(data_tra,
           data_tun = NULL,
           alpha = 0.05,
           n_basis_xall = 30,
           control.FDTW = list(),
           control.mFPCA = list(),
           control.rtr = list(),
           ncores = 1,
           print = TRUE) {


    par.FDTW <- do.call("par.FDTW", control.FDTW)
    par.mFPCA <- do.call("par.mFPCA", control.mFPCA)
    par.rtr <- do.call("par.rtr", control.rtr)
    if (is.null(par.rtr$seq_x))
      par.rtr$seq_x <- seq(0.1, max(unlist(data_tra$grid_i)), length.out = 50)
    template <- par.FDTW$template
    grid_tem <- par.FDTW$grid_tem
    frac_oeob <- par.FDTW$frac_oeob
    index_tem <- par.FDTW$index_tem
    if (is.null(par.FDTW$smin))
      par.FDTW$smin <- abs(diff(range(par.rtr$seq_x))) / abs(diff(range(par.FDTW$seq_t))) *
      0.001
    if (is.null(par.FDTW$smax))
      par.FDTW$smax <- abs(diff(range(par.rtr$seq_x))) / abs(diff(range(par.FDTW$seq_t))) *
      100
    smin <- par.FDTW$smin
    smax <- par.FDTW$smax
    iter <- par.FDTW$iter
    threshold <- par.FDTW$threshold
    N <- par.FDTW$N
    M <- par.FDTW$M
    seq_t <- par.FDTW$seq_t
    seq_x <- par.rtr$seq_x
    Delta <- par.rtr$Delta
    par.rtr$iter_der_min <- round(Delta * max(seq_x) / (seq_x[2] - seq_x[1]))
    alpha_vec <- par.FDTW$alpha_vec
    perc_basis_x_reg <- par.rtr$perc_basis_x_reg
    eta <- par.FDTW$eta
    lambda <- par.FDTW$lambda
    if (is.null(lambda))
      lambda <- c(0, 10 ^ seq(-8, -2, by = 0.25), 10 ^ 5)
    iter_tem  <-  par.FDTW$iter_tem
    perc_pca  <-  par.mFPCA$perc_pca
    perc_basis_x_pca  <-  par.mFPCA$perc_basis_x_pca
    perc_basis_h  <-  par.mFPCA$perc_basis_h

    x_list_s  <-  data_tra$x_err
    grid_x  <-  data_tra$grid_i

    n_obs <- length(x_list_s)
    delta_t <- seq_t[2] - seq_t[1]


    if (!methods::is(x_list_s[[1]], "fd")) {
      x_fd_list <-
        lapply(1:n_obs, function(ii)
          get_fd_smooth(
            x_value_i = x_list_s[[ii]],
            grid = grid_x[[ii]],
            nbasis_max = n_basis_xall
          ))
      der_x_fd_list <-
        lapply(1:n_obs, function(ii)
          fda::deriv.fd(x_fd_list[[ii]], 1))
      x_fd_list_smoothing  <-  x_fd_list
    }
    # Real-time registration --------------------------------------------------
    if (is.numeric(template)) {
      template_fd <-
        get_fd_smooth(x_value_i = template,
                      grid = grid_tem,
                      nbasis_max = n_basis_xall)
      graphics::lines(template_fd, lwd = 3)
      range_xx <- template_fd$basis$rangeval
      template_fd$basis$params = (template_fd$basis$params - range_xx[1]) /
        (range_xx[2] - range_xx[1])
      template_fd$basis$rangeval <- c(0, 1)##convention
      der_template_fd <- fda::deriv.fd(template_fd)
      n_obs <- length(x_fd_list)
      duration <-
        sapply(1:n_obs, function(ii)
          abs(base::diff(range(
            x_fd_list[[ii]]$basis$rangeval
          ))))
      median_duration <- stats::median(duration)
      out_lambda1 <-
        get_lambda_new(
          x_fd_list,
          der_x_fd_list,
          template_fd,
          der_template_fd,
          seq_t = seq_t,
          N = N,
          M = M,
          alpha_vec = alpha_vec,
          smin = smin,
          smax = smax,
          frac_oeob = frac_oeob,
          iter = iter,
          eta = eta,
          iter_tem = 1,
          ncores = ncores,
          lambda = lambda,
          threshold = threshold
        )
      lambda_opt = out_lambda1$lambda_opt
      align_output_IC <-
        FDTW_group(
          x_fd_list,
          template_fd,
          der_x_fd_list,
          der_template_fd,
          seq_t = seq_t ,
          N = N,
          M = M,
          iter = iter,
          eta = eta,
          lambda = lambda_opt,
          alpha_vec = alpha_vec,
          smin = smin,
          smax = smax,
          PLOT = FALSE,
          frac_oeob =  frac_oeob,
          fit_c = FALSE,
          n_basis_x = n_basis_xall,
          get_fd = "x_reg"
        )
      complete_fd <-
        completion_fd(align_output_IC$x_reg_fd,
                      align_output_IC$h_fd,
                      template_fd,
                      seq_t = seq_t)

    }
    else if (template == "Procrustes") {
      if (print)
        print("Procrustes")
      out_tem <-
        get_template(
          x_fd_list = x_fd_list,
          der_x_fd_list = der_x_fd_list,
          seq_t = seq_t,
          N = N,
          M = M,
          alpha_vec = alpha_vec,
          smin = smin,
          smax = smax,
          frac_oeob = frac_oeob,
          iter = iter,
          eta = eta,
          iter_tem = iter_tem,
          print = print,
          lambda = lambda,
          threshold = threshold,
          index_tem = index_tem,
          ncores = ncores
        )
      template_fd = out_tem$template_fd
      der_template_fd = out_tem$der_template_fd
      complete_fd = out_tem$complete_fd
      lambda_opt = out_tem$lambda_opt
      median_duration = out_tem$median_duration
      rm(out_tem)
    }


    x_fd_com <- complete_fd$x_fd_com
    h_fd_com <- complete_fd$h_fd_com
    point_pen <- get_point_pen(h_fd_com)
    ul_fd <- get_ul(h_fd_com, perc = 0.01)

    u_fd <- ul_fd$u_fd
    l_fd <- ul_fd$l_fd
    frac_end <- frac_oeob
    delta_xs <- fda::eval.fd(ul_fd$u_fd$basis$rangeval[1], ul_fd$u_fd)
    delta_ys <- ul_fd$l_fd$basis$rangeval[1]



    if (print)
      print("Real-time registration")
    os <- .Platform$OS.type
    if (os == "unix") {
      rt_list <- parallel::mclapply(1:n_obs, function(ii)
        Phase_I_OEBFDTW_rt(
          x_i = list(x_list_s[[ii]], grid_x[[ii]]),
          template_fd,
          seq_x = seq_x,
          u_fd = u_fd,
          l_fd = l_fd,
          seq_t = seq_t,
          alpha_vec = alpha_vec,
          M = M,
          N = N,
          smin = smin,
          smax = smax,
          eta = eta,
          iter = iter,
          lambda = lambda_opt,
          delta_xs = delta_xs,
          frac_end = frac_end,
          delta_ys = delta_ys,
          par.rtr = par.rtr,
          n_basis_x = n_basis_xall,
          perc_basis_x = perc_basis_x_reg,
          point_pen = point_pen
        ),
        mc.cores = ncores)
    }
    else{
      cl <- parallel::makeCluster(ncores)
      parallel::clusterExport(
        cl,
        list(
          "x_fd_list",
          "x_list_s",
          "grid_x",
          "template_fd",
          "seq_x",
          "u_fd",
          "l_fd",
          "seq_t",
          "delta_xs",
          "delta_ys",
          "lambda_opt",
          "N",
          "M",
          "alpha_vec",
          "smin",
          "smax",
          "n_basis_xall"
        ),
        envir = environment()
      )
      parallel::clusterEvalQ(cl,  library(funcharts))

      rt_list <- parallel::parLapply(cl, 1:n_obs, function(ii)
        Phase_I_OEBFDTW_rt(
          x_i = list(x_list_s[[ii]], grid_x[[ii]]),
          template_fd,
          seq_x = seq_x,
          u_fd = u_fd,
          l_fd = l_fd,
          seq_t = seq_t,
          alpha_vec = alpha_vec,
          M = M,
          N = N,
          smin = smin,
          smax = smax,
          eta = eta,
          iter = iter,
          lambda = lambda_opt,
          delta_xs = delta_xs,
          frac_end = frac_end,
          delta_ys = delta_ys,
          par.rtr = par.rtr,
          n_basis_x =
            n_basis_xall,
          perc_basis_x = perc_basis_x_reg,
          point_pen = point_pen
        ))

      parallel::stopCluster(cl)

    }

    x_list <- lapply(1:n_obs, function(ii)
      rt_list[[ii]]$x_reg_list)
    h_list <- lapply(1:n_obs, function(ii)
      rt_list[[ii]]$h_list)
    rm(rt_list)

    end_i <- matrix(0, n_obs, length(seq_x))
    for (kk in 1:n_obs) {
      for (ll in 1:length(seq_x)) {
        end_i[kk, ll] <-
          if (h_list[[kk]][[ll]][1][[1]][1] == "overlimit" |
              h_list[[kk]][[ll]][1][[1]][1] == "underlimit")
            NA
        else
          h_list[[kk]][[ll]]$basis$rangeval[2]

      }
    }
    ind_end <- 0
    x_list_t <- h_list_t <- obs_list <- list()
    for (ii in 1:length(seq_t)) {
      x_list_t[[ii]] <- h_list_t[[ii]] <- list()
      obs_list[[ii]] <- numeric()
      for (kk in 1:n_obs) {
        ind_ke <- unique(which(abs(end_i[kk, ] - seq_t[ii]) < 10 ^ -6))
        ind_kg <-
          unique(which(end_i[kk,] > seq_t[ii] &
                         abs(end_i[kk,] - seq_t[ii]) < 10 ^ -1))
        ind_k <- unique(c(ind_ke, ind_kg))
        if (length(ind_k) != 0) {
          leng_ke <- length(ind_ke)
          if (leng_ke == 0)
            indeces <- if (ind_end == 0)
              1
          else
            1:ind_end
          else
            indeces <- 1:(leng_ke + ind_end)

          ind_ke <- ind_k[indeces[1]]
          x_list_t[[ii]] <- c(x_list_t[[ii]], x_list[[kk]][ind_ke])
          h_list_t[[ii]] <- c(h_list_t[[ii]], h_list[[kk]][ind_ke])
          obs_list[[ii]] <- c(obs_list[[ii]], kk)
        }
        else{
          x_list_t[[ii]] <- c(x_list_t[[ii]], NULL)
          h_list_t[[ii]] <- c(h_list_t[[ii]], NULL)
        }
      }
    }
    weights_f <- NULL
    ind_end <- 0
    x_list_t <- h_list_t <- obs_list <- list()
    for (ii in 1:length(seq_t)) {
      x_list_t[[ii]] <- h_list_t[[ii]] <- list()
      obs_list[[ii]] <- numeric()
      for (kk in 1:n_obs) {
        ind_ke <- unique(which(abs(end_i[kk,] - seq_t[ii]) < 10 ^ -6))
        ind_kg <-
          unique(which(end_i[kk,] > seq_t[ii] &
                         abs(end_i[kk,] - seq_t[ii]) < 10 ^ -1))
        ind_k <- unique(c(ind_ke, ind_kg))
        if (length(ind_k) != 0) {
          leng_ke <- length(ind_ke)
          if (leng_ke == 0)
            indeces <- if (ind_end == 0)
              1
          else
            1:ind_end
          else
            indeces <- 1:(leng_ke + ind_end)
          ind_ke <- ind_k[indeces]
          x_list_t[[ii]] <- c(x_list_t[[ii]], x_list[[kk]][ind_ke])
          h_list_t[[ii]] <- c(h_list_t[[ii]], h_list[[kk]][ind_ke])
          obs_list[[ii]] <- c(obs_list[[ii]], kk)
        }
        else{
          x_list_t[[ii]] <- c(x_list_t[[ii]], NULL)
          h_list_t[[ii]] <- c(h_list_t[[ii]], NULL)
        }
      }
    }

  # mFPCA -------------------------------------------------------------------
  if (print)
    print("mFPCA")
  range_t <- template_fd$basis$rangeval

  parr_pca<-function(ii){

    if(length(x_list_t[[ii]]) <= 10){
      out <- "no variability"
    }
    else{
      t_t <- seq_t[ii]
      eval_seq_h <- c(range_t[1], seq_t[seq_t <= t_t])
      eval_seq_x <- seq(range_t[1], t_t, length.out = 200)
      eval_x <- matrix(0, length(eval_seq_x), length(x_list_t[[ii]]))
      eval_h <- matrix(0, length(eval_seq_h), length(x_list_t[[ii]]))
      nbasish <- nbasisx <- numeric()
      for (jj in 1:length(x_list_t[[ii]])) {
        h_fd_i <- h_list_t[[ii]][[jj]]
        x_fd_i <- x_list_t[[ii]][[jj]]
        end_h <- h_fd_i$basis$rangeval[2]
        eval_grid_h <-
          unique(
            c(
              range_t[1],
              seq_t[seq_t < h_fd_i$basis$rangeval[1]],
              h_fd_i$basis$rangeval[1],
              h_fd_i$basis$params,
              h_fd_i$basis$rangeval[2]
            )
          )
        eval_grid_x <-
          seq(range_t[1], x_fd_i$basis$rangeval[2], length.out = 200)
        h_fd_i <- complete_h(h_fd_i, eval_seq = eval_grid_h, inv = FALSE)$h_fd_com
        x_fd_i <-
          complete_x(x_fd_i, template_fd, eval_seq = eval_grid_x)$x_fd_com
        h_fd_i <-
          cut_fd_ht(h_fd_i , t_t, monotone = FALSE, eval_grid = eval_seq_h)
        x_fd_i <- cut_fd_xt(x_fd_i , t_t, eval_grid = eval_seq_x)
        nbasish[jj] <- h_fd_i$basis$nbasis
        nbasisx[jj] <- x_fd_i$basis$nbasis
        eval_x[, jj] <- fda::eval.fd(eval_seq_x, x_fd_i)
        eval_h[, jj] <- fda::eval.fd(eval_seq_h, h_fd_i)
      }
      basis_x_r <-
        fda::create.bspline.basis(c(range_t[1], t_t),
                             nbasis = max(2, round(perc_basis_x_pca * base::max(nbasisx))),
                             norder = min(max(1, round(
                               perc_basis_x_pca * base::mean(nbasisx)
                             )), 4))
      n_basis_h <-
        if (base::mean(nbasish) <= 10)
          max(2, floor(perc_basis_h * base::mean(nbasish)))
      else
        max(2, floor(perc_basis_h * base::mean(nbasish)))
      norder_h <- max(2, min(n_basis_h - 2, 4))
      basis_h_r <-
        fda::create.bspline.basis(c(range_t[1], t_t),
                             nbasis = max(2, min(
                               round(perc_basis_h * length(seq_t)), max(2, n_basis_h)
                             )),
                             norder = norder_h)
      x_fd_r <- fda::smooth.basis(eval_seq_x, eval_x, basis_x_r)$fd
      grid_new <-
        seq(min(eval_seq_h), max(eval_seq_h), length.out = 200)
      data_new <- data.frame(eval_seq_h = grid_new)
      predict <- matrix(0, length(grid_new), dim(eval_x)[2])
      npoints <- length(eval_seq_h)
      if(basis_h_r$nbasis>2){
        gradtol <- 10 ^ -7
        for (jj in 1:dim(eval_x)[2]) {
          fit <- scam2(
            eval_h[, jj] ~ s(
              eval_seq_h,
              k = basis_h_r$nbasis,
              bs = "mpi",
              m = max(1, min(norder_h - 1, 3))
            ),
            family = stats::gaussian(link = "identity"),
            control = list(trace = FALSE, bfgs = list(gradtol.bfgs = gradtol))
          )
          predict[, jj] <- scam::predict.scam(fit, newdata = data_new)
        }

        h_fd_r <- fda::smooth.basis(grid_new, predict, basis_h_r)$fd
      }
      else{
        h_fd_r <- fda::smooth.basis(eval_seq_h, eval_h, basis_h_r)$fd
      }

      out <- mFPCA(x_fd_r, h_fd_r, ncom = "ptv", par_ncom = perc_pca)
    }

    return(out)
  }

  if(.Platform$OS.type=="unix")
    pca_list <-
    parallel::mclapply(1:length(seq_t), function(ii)
      parr_pca(ii), mc.cores = ncores)
  else{
    cl <- parallel::makeCluster(ncores)
    parallel::clusterExport(
      cl,
      list(
        "range_t",
        "seq_t",
        "x_list_t",
        "h_list_t",
        "template_fd",
        "seq_x",
        "u_fd",
        "l_fd",
        "seq_t",
        "frac_oeob",
        "delta_xs",
        "delta_ys",
        "lambda_opt",
        "N",
        "M",
        "n_basis_xall",
        "parr_pca"
      ),
      envir = environment()
    )
    parallel::clusterEvalQ(cl,  library(funcharts))
    pca_list <- parallel::parLapply(cl, 1:length(seq_t), function(ii) parr_pca(ii))
    parallel::stopCluster(cl)
  }


  # Real-time registration tuning -------------------------------------------------------------------
  if(print) print("Real-time registration tuning")
  if(!is.null(data_tun)){
    x_list_s_tun <- data_tun$x_err
    grid_x_tun <- data_tun$grid_i
    n_obs_tun <- length(x_list_s_tun)
    if(os=="unix"){

      rt_list_tun <-
        parallel::mclapply(1:n_obs_tun, function(ii)
          Phase_I_OEBFDTW_rt(
            x_i = list(x_list_s_tun[[ii]], grid_x_tun[[ii]]),
            template_fd,
            seq_x = seq_x,
            u_fd = u_fd,
            l_fd = l_fd,
            seq_t = seq_t,
            alpha_vec = alpha_vec,
            M =
              M,
            N = N,
            smin = smin,
            smax = smax,
            eta = eta,
            iter = iter,
            lambda = lambda_opt,
            delta_xs = delta_xs,
            frac_end = frac_end,
            delta_ys = delta_ys,
            par.rtr = par.rtr,
            n_basis_x =
              n_basis_xall,
            perc_basis_x = perc_basis_x_reg,
            point_pen = point_pen
          ),
          mc.cores = ncores)
    }
    else{
      cl <- parallel::makeCluster(ncores)
      parallel::clusterExport(
        cl,
        list(
          "x_fd_list",
          "x_list_s_tun",
          "grid_x_tun",
          "template_fd",
          "seq_x",
          "u_fd",
          "l_fd",
          "seq_t",
          "delta_xs",
          "delta_ys",
          "lambda_opt",
          "N",
          "M",
          "alpha_vec",
          "smin",
          "smax",
          "n_basis_xall"
        ),
        envir = environment()
      )

      parallel::clusterEvalQ(cl,  library(funcharts))
      rt_list_tun <-
        parallel::parLapply(cl, 1:n_obs, function(ii)
          Phase_I_OEBFDTW_rt(
            x_i = list(x_list_s_tun[[ii]], grid_x_tun[[ii]]),
            template_fd,
            seq_x = seq_x,
            u_fd = u_fd,
            l_fd = l_fd,
            seq_t = seq_t,
            alpha_vec = alpha_vec,
            M =
              M,
            N = N,
            smin = smin,
            smax = smax,
            eta = eta,
            iter = iter,
            lambda = lambda_opt,
            delta_xs = delta_xs,
            frac_end = frac_end,
            delta_ys = delta_ys,
            par.rtr = par.rtr,
            n_basis_x =
              n_basis_xall,
            perc_basis_x = perc_basis_x_reg,
            point_pen = point_pen
          ))
      parallel::stopCluster(cl)
    }

    x_list_tun <-
      lapply(1:n_obs_tun, function(ii)
        rt_list_tun[[ii]]$x_reg_list)
    h_list_tun <-
      lapply(1:n_obs_tun, function(ii)
        rt_list_tun[[ii]]$h_list)

  }
  else{
    data_tun <- data_tra
    x_list_tun <- x_list
    h_list_tun <- h_list
  }
  # Control limits -------------------------------------------------------------------
  if(print) print("Control Limits")
  get_T2SPE_tot <-
    function(x_list,
             h_list,
             seq_t,
             seq_x,
             range_t,
             pca_list,
             ma = 1) {

      delta_t <- seq_t[2] - seq_t[1]
      nobs <- length(x_list)
      parr_fun <- function(ii) {
        T_list <- SPE_list <- numeric()
        for (kk in 1:nobs) {
          if (!is.null(x_list[[kk]][[ii]])) {
            if ((x_list[[kk]][[ii]] != "overlimit" &
                 x_list[[kk]][[ii]] != "underlimit")[1]) {
              x_fd_i <- x_list[[kk]][[ii]]
              h_fd_i <- h_list[[kk]][[ii]]
              end_h <- h_fd_i$basis$rangeval[2]
              ind_i <- max(which(seq_t <= end_h))
              mod_pca <- pca_list[[ind_i]]
              if((mod_pca != "no variability")[1]){
                eval_seq_h <- c(range_t[1], seq_t[seq_t <= seq_t[ind_i]])
                eval_seq_x <-
                  seq(range_t[1], seq_t[ind_i], length.out = 200)
                eval_grid_h <-
                  unique(
                    c(
                      range_t[1],
                      seq_t[seq_t < h_fd_i$basis$rangeval[1]],
                      h_fd_i$basis$rangeval[1],
                      h_fd_i$basis$params,
                      h_fd_i$basis$rangeval[2]
                    )
                  )
                eval_grid_x <-
                  seq(range_t[1], x_fd_i$basis$rangeval[2], length.out = 200)
                h_fd_i <-
                  complete_h(h_fd_i, eval_seq = eval_grid_h, inv = FALSE)$h_fd_com
                x_fd_i <-
                  complete_x(x_fd_i, template_fd, eval_seq = eval_grid_x)$x_fd_com
                h_fd_i <-
                  cut_fd_ht(h_fd_i , seq_t[ind_i], eval_grid = eval_seq_h, monotone = FALSE)
                x_fd_i <-
                  cut_fd_xt(x_fd_i , seq_t[ind_i], eval_grid = eval_seq_x)
                eval_x <- fda::eval.fd(eval_seq_x, x_fd_i)
                eval_h <- fda::eval.fd(eval_seq_h, h_fd_i)
                basis_x_r <-
                  fda::create.bspline.basis(
                    c(range_t[1], seq_t[ind_i]),
                    nbasis = mod_pca$x_fd$basis$nbasis,
                    norder = min(mod_pca$x_fd$basis$nbasis, 4)
                  )
                norder_h <- max(2, min(mod_pca$h_fd$basis$nbasis - 2, 4))
                basis_h_r <-
                  fda::create.bspline.basis(
                    c(range_t[1], seq_t[ind_i]),
                    nbasis = max(2, mod_pca$h_fd$basis$nbasis),
                    norder = min(max(2, mod_pca$h_fd$basis$nbasis), norder_h)
                  )
                x_com <- fda::smooth.basis(eval_seq_x, eval_x, basis_x_r)$fd
                grid_new <-
                  seq(min(eval_seq_h), max(eval_seq_h), length.out = 200)
                data_new <- data.frame(eval_seq_h = grid_new)
                npoints <- length(eval_seq_h)
                if(basis_h_r$nbasis > 2){
                  gradtol <- 10 ^ -7
                  fit <-
                    scam2(
                      eval_h ~ s(
                        eval_seq_h,
                        k = basis_h_r$nbasis,
                        bs = "mpi",
                        m = max(1, min(norder_h - 1, 3))
                      ),
                      family = stats::gaussian(link = "identity"),
                      control = list(trace = FALSE, bfgs = list(gradtol.bfgs = gradtol))
                    )
                  predict <- scam::predict.scam(fit, newdata = data_new)
                  h_com <-
                    fda::smooth.basis(grid_new, as.numeric(predict), basis_h_r)$fd
                }
                else{
                  h_com <- fda::smooth.basis(eval_seq_h, eval_h, basis_h_r)$fd
                }
                out_rt <- get_T2SPE(x_com, h_com, mod_pca)
                T_list[kk] <- out_rt$T_2
                SPE_list[kk] <- out_rt$SPE
              }
              else{
                T_list[kk] <- NA
                SPE_list[kk] <- NA
              }
            }
            else{
              T_list[kk] <- NA
              SPE_list[kk] <- NA
            }
          }
          else{
            T_list[kk] <- NA
            SPE_list[kk] <- NA
          }
        }
        out <- list(T_list = T_list,
                    SPE_list = SPE_list)
        return(out)
      }
      if(.Platform$OS.type=="unix")
        out_list<-parallel::mclapply(1:length(seq_x), function(ii)
          parr_fun(ii), mc.cores = ncores)
      else{
        cl <- parallel::makeCluster(ncores)
        parallel::clusterExport(
          cl,
          list(
            "range_t",
            "seq_t",
            "x_list",
            "h_list",
            "template_fd",
            "seq_x",
            "u_fd",
            "l_fd",
            "seq_t",
            "frac_oeob",
            "delta_xs",
            "delta_ys",
            "lambda_opt",
            "N",
            "M",
            "n_basis_xall",
            "parr_fun",
            "pca_list",
            "nobs",
            "delta_t"
          ),
          envir = environment()
        )
        parallel::clusterEvalQ(cl,  library(funcharts))
        out_list <-
          parallel::parLapply(cl, 1:length(seq_x), function(ii)
            parr_fun(ii))#,mc.cores = 12)
        parallel::stopCluster(cl)
      }

      T_2_mat <- sapply(out_list, "[[", 1)
      SPE_2_mat <- sapply(out_list, "[[", 2)
      for (ii in 1:nobs) {
        ind_aa <- which(!is.na(T_2_mat[ii, ]))
        aa <- (T_2_mat[ii, ])[ind_aa]
        filter_aa <-
          stats::filter(aa, rep(1 / ma, ma), sides = 1, method = "convolution")
        aa <- c(aa[(1:(ma - 1))], filter_aa[-(1:(ma - 1))])
        T_2_mat[ii, ind_aa] = aa
        ind_aa <- which(!is.na(SPE_2_mat[ii, ]))
        aa <- (SPE_2_mat[ii, ])[ind_aa]
        filter_aa <-
          stats::filter(aa, rep(1 / ma, ma), sides = 1, method = "convolution")
        aa <- c(aa[(1:(ma - 1))], filter_aa[-(1:(ma - 1))])
        SPE_2_mat[ii, ind_aa] = aa
      }
      out <- list(T_2_mat = T_2_mat,
                  SPE_2_mat = SPE_2_mat)
      return(out)
    }
  range_t <- template_fd$basis$rangeval
  mod <-
    get_T2SPE_tot(
      x_list = x_list_tun,
      h_list = h_list_tun,
      seq_t,
      seq_x,
      range_t,
      pca_list = pca_list,
      ma = 1
    )
  T_2_mat <- mod$T_2_mat
  SPE_2_mat <- mod$SPE_2_mat
  T2SPE_fd <- get_T2SPE_fd(T_2_mat, SPE_2_mat, seq_x)
  T2_fd <- T2SPE_fd$T2_fd
  SPE_fd <- T2SPE_fd$SPE_fd
  alpha_sid <- 1 - sqrt(1 - alpha)
  CL_T2 <- CL_T2SPE_fd(T2_fd, alpha = alpha_sid, seq_x = seq_x)
  CL_SPE <- CL_T2SPE_fd(SPE_fd, alpha = alpha_sid, seq_x = seq_x)

  out <- list(
    T2_fd = T2_fd,
    SPE_fd = SPE_fd,
    CL_SPE = CL_SPE,
    CL_T2 = CL_T2,
    pca_list = pca_list,
    template_fd = template_fd,
    der_template_fd = der_template_fd,
    u_fd = u_fd,
    l_fd = l_fd,
    h_list_tun = h_list_tun,
    x_list_tun = x_list_tun,
    h_list = h_list,
    x_list = x_list,
    x_err = x_list_s,
    grid_i = grid_x,
    x_list_smooth = x_fd_list_smoothing,
    par_reg = list(
      lambda = lambda_opt,
      delta_xs = delta_xs,
      delta_ys = delta_ys,
      frac_end = frac_end,
      point_pen = point_pen,
      n_basis_xall = n_basis_xall,
      par.rtr = par.rtr,
      par.FDTW = par.FDTW,
      par.mFPCA = par.mFPCA
    )
  )

  class(out) <- "FRTM_PhaseI"
  return(out)

}



#' @title Phase II of the FRTM method.
#' @description This function implements the monitoring phase (Phase II) of FRTM method.
#' @param data_oc  A list containing the following arguments: \code{x_err} a list containing the discrete observations for each curve to be monitored; \code{grid_i} a list of vector of time points where the curves to be monitored are sampled.
#' @param mod_phaseI  An object of class \code{mod_phaseI_FRTM} obtained as output of the function  \code{FRTM_PhaseI}.
#' @param ncores If \code{ncores}>1, then parallel computing is used, with \code{ncores} cores. Default is 1.
#' @references
#' Centofanti, F., A. Lepore, M. Kulahci, and M. P. Spooner (2024).
#' Real-time monitoring of functional data. \emph{Journal of Quality Technology}, 57(2):135--152,
#' doi:https://doi.org/10.1080/00224065.2024.2430978.
#' @return   A list containing the following arguments:
#'
#' \code{T2_fd} List of \eqn{T^{2}} functions for each observation.
#'
#' \code{SPE_fd} List of SPE functions for each observation.
#'
#' \code{CL_T2} Control limit of  the Hotelling's \eqn{T^{2}} control chart.
#'
#' \code{CL_SPE} Control limit of  the SPE control chart.
#'
#' \code{x_err} A list containing the discrete observations for each curve.
#'
#' \code{grid_i} A list of vector of time points where the curves  are sampled.
#'
#' \code{x_list_smooth} Smooth curves.
#'
#' \code{mod_phaseI} An object of class \code{mod_phaseI_FRTM} obtained as output of the function  \code{FRTM_PhaseI}.
#'
#' @export
#' @inherit FRTM_PhaseI return examples
#'
FRTM_PhaseII<-function(data_oc,mod_phaseI,ncores=1){

  if(!methods::is(mod_phaseI, "FRTM_PhaseI"))
    stop("mod_phaseI is not an object of class FRTM_PhaseI!")
  x_list_o <- data_oc$x_err
  grid_x_o  <-  data_oc$grid_i
  template_fd <- mod_phaseI$template_fd
  der_template_fd <- mod_phaseI$der_template_fd
  pca_list <- mod_phaseI$pca_list
  CL_T2 <- mod_phaseI$CL_T2
  CL_SPE <- mod_phaseI$CL_SPE
  u_fd <- mod_phaseI$u_fd
  l_fd <- mod_phaseI$l_fd
  smin <- mod_phaseI$par_reg$par.FDTW$smin
  smax <- mod_phaseI$par_reg$par.FDTW$smax
  eta <- mod_phaseI$par_reg$par.FDTW$eta
  iter <- mod_phaseI$par_reg$par.FDTW$iter
  N <- mod_phaseI$par_reg$par.FDTW$N
  M <- mod_phaseI$par_reg$par.FDTW$M
  delta_xs <- mod_phaseI$par_reg$delta_xs
  delta_ys <- mod_phaseI$par_reg$delta_ys
  frac_end <- mod_phaseI$par_reg$frac_end
  delta_ye <- mod_phaseI$par_reg$delta_ye
  lambda <- mod_phaseI$par_reg$lambda
  lambda_init <- mod_phaseI$par_reg$lambda_init
  thres <- mod_phaseI$par_reg$thres
  alpha_vec <- mod_phaseI$par_reg$par.FDTW$alpha_vec
  point_pen <- mod_phaseI$par_reg$point_pen
  n_basis_xall <- mod_phaseI$par_reg$n_basis_xall
  perc_basis_x <- mod_phaseI$par_reg$par.mFPCA$perc_basis_x
  perc_basis_h <- mod_phaseI$par_reg$par.mFPCA$perc_basis_h
  par.rtr <- mod_phaseI$par_reg$par.rtr


  seq_t <- mod_phaseI$par_reg$par.FDTW$seq_t
  seq_x <- par.rtr$seq_x

  n_obs_II <- length(x_list_o)
  delta_t <- seq_t[2] - seq_t[1]
  range_t <- template_fd$basis$rangeval
  x_fd_list <-
    lapply(1:n_obs_II, function(ii)
      get_fd_smooth(
        x_value_i = x_list_o[[ii]],
        grid = grid_x_o[[ii]],
        nbasis_max = n_basis_xall
      ))
  x_fd_list_smoothing <- x_fd_list
  par_funII <- function(ii) {
    mod_iii <-
      Phase_I_OEBFDTW_rt(
        x_i = list(x_list_o[[ii]], grid_x_o[[ii]]),
        template_fd = template_fd,
        seq_x = seq_x,
        u_fd = u_fd,
        l_fd = l_fd,
        seq_t = seq_t,
        alpha_vec =  alpha_vec,
        M = M,
        N = N,
        smin = smin,
        smax = smax,
        eta = eta,
        iter = iter,
        lambda = lambda,
        frac_end = frac_end,
        delta_xs = delta_xs,
        delta_ys = delta_ys,
        par.rtr = par.rtr,
        n_basis_x = n_basis_xall,
        perc_basis_x = perc_basis_x,
        point_pen = point_pen,
        PLOTRT = FALSE
      )
    x_list <- mod_iii$x_reg_list
    h_list <- mod_iii$h_list
    rm(mod_iii)
    mod_iii = NULL
    if (!is.null(lambda_init)) {
      mod_iii2 <-
        Phase_I_OEBFDTW_rt(
          x_i = list(x_list_o[[ii]], grid_x_o[[ii]]),
          template_fd = template_fd,
          seq_x = seq_x[1:thres],
          u_fd = u_fd,
          l_fd = l_fd,
          seq_t = seq_t,
          alpha_vec =  alpha_vec,
          M = M,
          N = N,
          smin = smin,
          smax = smax,
          eta = 0.5,
          iter = 3,
          lambda = lambda_init,
          frac_end = frac_end,
          delta_xs = delta_xs,
          delta_ys = delta_ys,
          par.rtr = list(
            delta_der = 0.3,
            iter_der_min = round(0.5 / (seq_x[2] - seq_x[1])),
            window = 0.04,
            length_grid_window = 5,
            length_grid_owindow = 10,
            seq_der = seq(0.01, 0.1, length.out = 10)
          ),
          PLOTRT = FALSE
        )

      h_listm <- c(mod_iii2$h_list, h_list[thres:length(h_list)])
      x_listm <- c(mod_iii2$x_reg_list, x_list[thres:length(x_list)])
      x_list  <-  x_listm
      h_list  <-  h_listm
    }

    T_vec <- SPE_vec <- numeric()

    for (kk in 1:length(seq_x)) {
      if (!is.null(x_list[[kk]])) {
        if ((x_list[[kk]] != "overlimit" & x_list[[kk]] != "underlimit")[1]) {
          x_fd_i = x_list[[kk]]
          h_fd_i = h_list[[kk]]
          end_h <- h_fd_i$basis$rangeval[2]
          ind_i <- max(which(seq_t <= end_h))
          mod_pca <- pca_list[[ind_i]]
          if ((mod_pca != "no variability")[1]) {
            eval_seq_h <- c(range_t[1], seq_t[seq_t <= seq_t[ind_i]])
            eval_seq_x <-
              seq(range_t[1], seq_t[ind_i], length.out = 200)
            eval_grid_h <-
              unique(
                c(
                  range_t[1],
                  seq_t[seq_t < h_fd_i$basis$rangeval[1]],
                  h_fd_i$basis$rangeval[1],
                  h_fd_i$basis$params,
                  h_fd_i$basis$rangeval[2]
                )
              )
            eval_grid_x <-
              seq(range_t[1], x_fd_i$basis$rangeval[2], length.out = 200)
            h_fd_i <-
              complete_h(h_fd_i, eval_seq = eval_grid_h, inv = FALSE)$h_fd_com
            x_fd_i <-
              complete_x(x_fd_i, template_fd, eval_seq = eval_grid_x)$x_fd_com
            h_fd_i <-
              cut_fd_ht(h_fd_i ,
                        seq_t[ind_i],
                        eval_grid = eval_seq_h,
                        monotone = FALSE)
            x_fd_i <-
              cut_fd_xt(x_fd_i , seq_t[ind_i], eval_grid = eval_seq_x)
            eval_x <- fda::eval.fd(eval_seq_x, x_fd_i)
            eval_h <- fda::eval.fd(eval_seq_h, h_fd_i)
            basis_x_r <-
              fda::create.bspline.basis(
                c(range_t[1], seq_t[ind_i]),
                nbasis = mod_pca$x_fd$basis$nbasis,
                norder = min(mod_pca$x_fd$basis$nbasis, 4)
              )
            norder_h <- max(2, min(mod_pca$h_fd$basis$nbasis - 2, 4))
            basis_h_r <-
              fda::create.bspline.basis(
                c(range_t[1], seq_t[ind_i]),
                nbasis = max(2, mod_pca$h_fd$basis$nbasis),
                norder = norder_h
              )
            x_com <- fda::smooth.basis(eval_seq_x, eval_x, basis_x_r)$fd
            grid_new <-
              seq(min(eval_seq_h), max(eval_seq_h), length.out = 200)
            data_new <- data.frame(eval_seq_h = grid_new)
            npoints <- length(eval_seq_h)
            if (basis_h_r$nbasis > 2) {
              gradtol <- 10 ^ -7
              fit <- scam2(
                eval_h ~ s(
                  eval_seq_h,
                  k = basis_h_r$nbasis,
                  bs = "mpi",
                  m = max(1, min(norder_h - 1, 3))
                ),
                family = stats::gaussian(link = "identity"),
                control = list(
                  trace = FALSE,
                  bfgs = list(gradtol.bfgs = gradtol)
                )
              )
              predict <- scam::predict.scam(fit, newdata = data_new)
              h_com <-
                fda::smooth.basis(grid_new, as.numeric(predict), basis_h_r)$fd
            }
            else{
              h_com <- fda::smooth.basis(eval_seq_h, eval_h, basis_h_r)$fd
            }
            out_rt <- get_T2SPE(x_com, h_com, mod_pca)
            T_vec[kk] <- out_rt$T_2
            SPE_vec[kk] <- out_rt$SPE
          }
          else{
            T_vec[kk] <- NA
            SPE_vec[kk] <- NA
          }
        }
        else{
          T_vec[kk] <- NA
          SPE_vec[kk] <- NA
        }
      }
      else{
        T_vec[kk] <- NA
        SPE_vec[kk] <- NA
      }
    }
    out <- list(
      T_vec = T_vec,
      SPE_vec = SPE_vec,
      mod_iii = mod_iii,
      seq_x = seq_x
    )
    return(out)
  }
  if (.Platform$OS.type == "unix")
    out_list <-
    parallel::mclapply(1:n_obs_II, function(ii)
      par_funII(ii), mc.cores = ncores)
  else{
    cl <- parallel::makeCluster(ncores)
    parallel::clusterExport(
      cl,
      list(
        "x_list_o",
        "grid_x_o",
        "template_fd",
        "seq_x",
        "u_fd",
        "l_fd",
        "seq_t",
        "delta_xs",
        "delta_ys",
        "N",
        "M",
        "alpha_vec",
        "smin",
        "smax",
        "n_basis_xall",
        "eta",
        "iter",
        "lambda",
        "frac_end",
        "delta_xs",
        "delta_ys",
        "par.rtr",
        "perc_basis_x",
        "point_pen",
        "par_funII",
        "pca_list",
        "delta_t"
      ),
      envir = environment()
    )
    parallel::clusterEvalQ(cl,  library(funcharts))

    out_list <-
      parallel::parLapply(cl, 1:n_obs_II, function(ii)
        par_funII(ii))

    parallel::stopCluster(cl)
  }
  T_2_mat <- t(sapply(out_list, "[[", 1))
  SPE_2_mat <- t(sapply(out_list, "[[", 2))
  ma <- 1
  for (ii in 1:n_obs_II) {
    ind_aa <- which(!is.na(T_2_mat[ii, ]))
    aa <- (T_2_mat[ii, ])[ind_aa]
    filter_aa <-
      stats::filter(aa, rep(1 / ma, ma), sides = 1, method = "convolution")
    aa <- c(aa[(1:(ma - 1))], filter_aa[-(1:(ma - 1))])
    T_2_mat[ii, ind_aa] = aa
    ind_aa <- which(!is.na(SPE_2_mat[ii, ]))
    aa <- (SPE_2_mat[ii, ])[ind_aa]
    filter_aa <-
      stats::filter(aa, rep(1 / ma, ma), sides = 1, method = "convolution")
    aa <- c(aa[(1:(ma - 1))], filter_aa[-(1:(ma - 1))])
    SPE_2_mat[ii, ind_aa] = aa
  }
  T2SPE_fd <- get_T2SPE_fd(T_2_mat, SPE_2_mat, seq_x)
  T2_fd <- T2SPE_fd$T2_fd
  SPE_fd <- T2SPE_fd$SPE_fd
  alpha_sid <- 1 - sqrt(1 - 0.05)
  CL_T2_II <- CL_T2SPE_fd(T2_fd, alpha = alpha_sid, seq_x = seq_x)
  CL_SPE_II <- CL_T2SPE_fd(SPE_fd, alpha = alpha_sid, seq_x = seq_x)


  out <- list(
    T2_fd = T2_fd,
    SPE_fd = SPE_fd,
    CL_T2 = CL_T2,
    CL_SPE = CL_SPE,
    x_err = x_list_o,
    grid_i = grid_x_o,
    x_list_smooth = x_fd_list_smoothing,
    mod_phaseI = mod_phaseI
  )
  class(out) <- "FRTM_PhaseII"
  return(out)
}




