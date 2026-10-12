## Plots
## 12/16/19
##


#' Plot the surface and observations
#'
#' @param est_list output from \code{estimate_params}
#' @param GIA_fn function to calculate GIA
#' @param fn_list additional arguments to pass to GIA fn
#' @param xlab to pass to ggplot
#' @param ylab to pass to ggplot
#' @param title to pass to ggplot
#' @param subtitle to pass to ggplot
#' @param base_size to pass to ggplot
#' @return ggplot object
#' @examples
#' df <- loewesadditivity::cyrpa_ripr
#' df$dose_A <- df$CyRPA
#' df$dose_B <- df$RIPR
#' data <- fortify_gia_data(df)
#' model_params <- c("beta_A" = .5, "beta_B" = .5,
#'                  "gamma_A" = .5, "gamma_B" = .5,
#'                  "tau_1" = 0, "tau_2" = 0)
#' n_boot <- 10
#' GIA_fn <- base_GIA
#' S_fn <- calc_S_base
#' fn_list <- NULL
#' alpha <- .05
#' verbose <- FALSE
#' out <- estimate_params(data = data,
#' init_params = model_params,
#' n_boot = n_boot,
#' GIA_fn = GIA_fn,
#' S_fn = S_fn,
#' fn_list = fn_list,
#' alpha = alpha,
#' verbose = verbose)
#' plot_surface(out)
#' @export
plot_surface <- function(est_list,
                         GIA_fn = base_GIA,
                         fn_list = NULL,
                         xlab = "Dose A",
                         ylab = "Dose B",
                         title = "Surface Plot of Doses",
                         subtitle = "",
                         base_size = 14){
  if(!("loewes_list" %in% class(est_list))){
    stop("We need the output from estimate_params()")
  }
#  browser()

  df <- est_list$GIA_est
  par <- est_list$params_est$mean
  names(par) <- as.character(est_list$params_est$param)

  beta_A <- est_list$params_est$mean[which(est_list$params_est$param == "beta_A")]
  beta_B <- est_list$params_est$mean[which(est_list$params_est$param == "beta_B")]

  Amax <-  max(df$dose_A / beta_A)
  Bmax <- max(df$dose_B / beta_B)
  #browser()
  df_grid <- make_grid(n = 40, par = par,
                       Amax = Amax,
                       Bmax = Bmax,
                       n_reps = 1)

  df_grid$GIA <- GIA_fn(model_params = par,
                         dose_A = df_grid$dose_A,
                         dose_B = df_grid$dose_B)



  g <-  ggplot2::ggplot() +
    ggplot2::geom_tile(data = df_grid,
                        ggplot2::aes(x = .data$dose_A / beta_A ,
                            y = .data$dose_B / beta_B,
                            fill = .data$GIA)) +
    viridis::scale_fill_viridis(option = "magma",
                                limits = c(-10, 120),
                                name = "GIA %") +
    ggplot2::labs(x = paste(xlab,
                   "scaled by ED50"),
         y = paste(ylab,
                   "scaled by ED50"),
         title = title,
         subtitle = subtitle) +
    ggplot2::theme_bw(base_size = base_size) +
    ggplot2::geom_contour(data = df_grid,
                 ggplot2::aes( x = .data$dose_A / beta_A,
                      y = .data$dose_B / beta_B,
                      z = .data$GIA),  col = "white") +
    metR::geom_text_contour(data = df_grid,
                            ggplot2::aes(x = .data$dose_A / beta_A,
                                         y = .data$dose_B / beta_B,
                                         z = .data$GIA))


  g <- g +  ggplot2::geom_point(data = est_list$GIA_est,
                           ggplot2::aes(x = .data$dose_A / beta_A,
                                        y = .data$dose_B / beta_B), size = 5,
                           col = "black") +
    ggplot2::geom_point(data = est_list$GIA_est,
                        ggplot2::aes(x = .data$dose_A / beta_A,
                                     y = .data$dose_B / beta_B,
                                     col = .data$GIA), size = 3) +
    viridis::scale_color_viridis(option = "magma",
                                 limits = c(-10,120),
                                 name = "GIA %")



  print(g)
  return(g)

}


#' Plot the surface and observations
#'
#' @param est_list output from \code{estimate_params}
#' @param dose_A to pass to ggplot
#' @param dose_B to pass to ggplot
#' @param title to pass to ggplot
#' @param subtitle to pass to ggplot
#' @param base_size to pass to ggplot
#' @return ggplot object
#' @examples
#' df <- loewesadditivity::cyrpa_ripr
#' df$dose_A <- df$CyRPA
#' df$dose_B <- df$RIPR
#' data <- fortify_gia_data(df)
#' model_params <- c("beta_A" = .5, "beta_B" = .5,
#'                  "gamma_A" = .5, "gamma_B" = .5,
#'                  "tau_1" = 0, "tau_2" = 0)
#' n_boot <- 10
#' GIA_fn <- base_GIA
#' S_fn <- calc_S_base
#' fn_list <- NULL
#' alpha <- .05
#' verbose <- FALSE
#' out <- estimate_params(data = data,
#' init_params = model_params,
#' n_boot = n_boot,
#' GIA_fn = GIA_fn,
#' S_fn = S_fn,
#' fn_list = fn_list,
#' alpha = alpha,
#' verbose = verbose)
#' plots <- plot_curves(out, dose_A = "CyRPA",
#' dose_B = "RIPR")
#'
#' @export
plot_curves <- function(est_list,
                         dose_A = "Dose A",
                         dose_B = "Dose B",
                         title = "Curves of Dose Combos",
                         subtitle = "",
                         base_size = 14){
  if(!("loewes_list" %in% class(est_list))){
    stop("We need the output from estimate_params()")
  }
  #  browser()

  gg_df <- est_list$GIA_est
  par <- est_list$params_est$mean
  names(par) <- as.character(est_list$params_est$param)


  g2 <-  ggplot2::ggplot() +
    ggplot2::geom_ribbon(data = gg_df,
                                ggplot2::aes(x = .data$dose_B,
                                    ymin = .data$lower,
                                    ymax = .data$upper,
                                    fill = .data$dose_A),
                                alpha = .3) +
    ggplot2::geom_line(data = gg_df,
              ggplot2::aes(x = .data$dose_B,
                  y = .data$mean,
                  col = .data$dose_A),
              size = 1) +
    ggplot2::facet_wrap(ggplot2::vars(gg_df$dose_A),
                        ncol = 3) + #,
    #  labeller = label_both) +
    ggplot2::theme_bw(base_size = base_size) +
    viridis::scale_color_viridis(name = paste(dose_A, "(mg/mL)")) +
    viridis::scale_fill_viridis(name = paste(dose_A, "(mg/mL)")) +
    ggplot2::geom_point(data = gg_df,
               ggplot2::aes(x = .data$dose_B,
                   y = .data$GIA)) +
    ggplot2::labs(x = paste(dose_B, "(mg/mL)"),
         y = "GIA %",
         title = title,
         subtitle = subtitle)





  g1 <-  ggplot2::ggplot() +
    ggplot2::geom_ribbon(data = gg_df,
                         ggplot2::aes(x = .data$dose_A,
                                      ymin = .data$lower,
                                      ymax = .data$upper,
                                      fill = .data$dose_B),
                         alpha = .3) +
    ggplot2::geom_line(data = gg_df,
                       ggplot2::aes(x = .data$dose_A,
                                    y = .data$mean,
                                    col = .data$dose_B),
                       size = 1) +
    ggplot2::facet_wrap(
                        ggplot2::vars(gg_df$dose_B),
                        ncol = 3) + #,
    #  labeller = label_both) +
    ggplot2::theme_bw(base_size = base_size) +
    viridis::scale_color_viridis(name = paste(dose_B, "(mg/mL)")) +
    viridis::scale_fill_viridis(name = paste(dose_B, "(mg/mL)")) +
    ggplot2::geom_point(data = gg_df,
                        ggplot2::aes(x = .data$dose_A,
                            y = .data$GIA)) +
    ggplot2::labs(x = paste(dose_A, "(mg/mL)"),
                  y = "GIA %",
                  title = title,
                  subtitle = subtitle)



 gridExtra::grid.arrange(g1, g2)


  return(list(g1 = g1, g2 = g2))

}


#' Plot the estimated isobologram
#'
#' @param est_list output from \code{estimate_params}
#' @param GIA_fn function to calculate GIA
#' @param fn_list additional arguments to pass to GIA fn
#' @param dose_A to pass to ggplot
#' @param dose_B to pass to ggplot
#' @param title to pass to ggplot
#' @param subtitle to pass to ggplot
#' @param base_size to pass to ggplot
#' @return ggplot object
#' @examples
#' df <- loewesadditivity::cyrpa_ripr
#' df$dose_A <- df$CyRPA
#' df$dose_B <- df$RIPR
#' data <- fortify_gia_data(df)
#' model_params <- c("beta_A" = .5, "beta_B" = .5,
#'                  "gamma_A" = .5, "gamma_B" = .5,
#'                  "tau_1" = 0, "tau_2" = 0)
#' n_boot <- 10
#' GIA_fn <- base_GIA
#' S_fn <- calc_S_base
#' fn_list <- NULL
#' alpha <- .05
#' verbose <- FALSE
#' out <- estimate_params(data = data,
#' init_params = model_params,
#' n_boot = n_boot,
#' GIA_fn = GIA_fn,
#' S_fn = S_fn,
#' fn_list = fn_list,
#' alpha = alpha,
#' verbose = verbose)
#' plot_curves(out, dose_A = "CyRPA",
#' dose_B = "RIPR")
#'
#' @export
plot_isobologram <- function(est_list,
                        dose_A = "Dose A",
                        dose_B = "Dose B",
                        GIA_fn = base_GIA,
                        fn_list = NULL,
                        title = "Isobologram Dose Combos",
                        subtitle = "",
                        base_size = 14){
  if(!("loewes_list" %in% class(est_list))){
    stop("We need the output from estimate_params()")
  }
   #browser()

  gg_df <- est_list$GIA_est
  par <- est_list$params_est$mean
  names(par) <- as.character(est_list$params_est$param)

  beta_A <- par["beta_A"]
  beta_B <- par["beta_B"]
  ed50_df <- get_ed_line(grid_width = 50,
                         par = par,
                         GIA_fn = GIA_fn,
                         fn_list = fn_list,
                         ed_val = 50)

  S1 <- est_list$S_est$lower
  S2 <- est_list$S_est$upper
  SM <- est_list$S_est$mean
  S_CI <- data.frame( x =  1 / (2 * c(S1, S2))  ,
                      y =  1 / (2 * c(S1, S2)),
                      type = "Inverse Hewlett S")


  ed50_df$type <- "Estimate"
  dummy <- seq(1, 0, length.out = 100)
  df2 <- data.frame(dose_A = dummy * par["beta_A"] ,
                    dose_B =(1 -dummy ) * par[ "beta_B"] ,
                    type = "Baseline")

  g <- ggplot2::ggplot(data = ed50_df,
                       ggplot2::aes(x = .data$dose_A / par[ "beta_A"],
                             y = .data$dose_B / par["beta_B"],
                             col = .data$type)) +
    ggplot2::geom_line(size = 2) +
    ggplot2::geom_line(data = df2,  linetype = "dashed",
              size = 2) +
    ggplot2::geom_line(data = S_CI,
                       ggplot2::aes(x = .data$x,
                                    y = .data$y),
              size = 2) +
    ggplot2::theme_bw(base_size = base_size) +
    ggplot2::scale_color_manual(values = c("red", "black", "cornflowerblue"),
                       name = "Type") +
    ggplot2::labs(x = paste(dose_A, " / ED50"),
         y = paste(dose_B, " / ED50"),
         title = title,
         subtitle = subtitle) +
    ggplot2::theme(legend.position = "bottom")




  print(g)
  return(g)

}

#' Helper function to get the ED50 line
#'
#' @param grid_width number of levels to find points at
#' @param par named vector of parameters
#' @param GIA_fn function to calculate GIA
#' @param fn_list additional parameters to pass to GIA_fn
#' @param ed_val Which line to compute.  Default is 50
#' @return data frame with the following columns
#' \describe{
#' \item{dose_A}{dose of A (unscaled)}
#' \item{dose_B}{dose of B (unscaled)}
#' \item{GIA}{value of GIA \%}
#' }
get_ed_line <- function(grid_width = 50,
                        par,
                        GIA_fn = base_GIA,
                        fn_list = NULL,
                        ed_val = 50){

  beta_A <- par["beta_A"]
  beta_B <- par["beta_B"]

  vals_B <- seq(0, beta_B, length.out = grid_width)
  vals_A <- c(beta_A, rep(NA, grid_width - 2), 0)


#  browser()
  ## Make a root function
  root_fxn <- function(x,
                       y) {
    out <- GIA_fn(model_params = par,
                  fn_list = fn_list,
                  dose_A = x,
                  dose_B = y)
    return(out - ed_val)
  }

  ## Get the roots
  vals_A[2:(grid_width - 1)] <-
    sapply(2:(length(vals_B) - 1), function(ii) {
      roots <- rootSolve::uniroot.all(root_fxn,
                                      c(0, 2 * beta_A),
                                      y = vals_B[ii])
      min(roots)
    })

  df <- data.frame(dose_A = vals_A,
                   dose_B = vals_B,
                   GIA = ed_val)
  return(df)



}

