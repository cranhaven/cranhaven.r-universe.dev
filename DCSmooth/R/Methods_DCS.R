################################################################################
#                                                                              #
#                      DCSmooth Package: Methods for DCS                       #
#                                                                              #
################################################################################

### Includes all methods for summarizing, printing and plotting classes "dcs" and
### "dcs_options".

  # summary.dcs
    # print.summary_dcs
  # print.dcs
  # summary.dcs_options
  # print.dcs_options
  # residuals.dcs
  # plot.dcs
  # .order.to.string
  # .onUnload

#------------------------------Summary Methods---------------------------------#

#' Summarizing Results from Double Conditional Smoothing
#' 
#' @description \code{summary} method for class \code{"dcs"}
#' 
#' @param object an object of class "dcs", usually, a result of a call to 
#'  \code{\link{dcs}}.
#' @param ... Additional arguments passed to the \code{summary.dcs} function.
#' 
#' @section Details:
#' \code{summary.dcs} strips an object of class \code{"dcs"} from all large
#'  matrices (\code{Y}, \code{X}, \code{T}, \code{M}, \code{R}), allowing
#'  for easier handling of meta-statistics of the bandwidth selection procedure.
#' 
#'  \code{print.summary_dcs} returns a list of summary statistics
#'  from the dcs procedure. The output depends on the use of the \code{dcs}-
#'  function. If automatic bandwidth selection is chosen, \code{summary.dcs}
#'  prints detailed statistics of the type of regression, the estimated 
#'  bandwidths \code{h_x}, \code{h_t}, the variance coefficient \code{c_f} and 
#'  performance statistics such as the number of iterations of the IPI-algorithm 
#'  and the time used for bandwidth selection.
#' 
#'  The method used for estimation of the variance coefficient is printed and 
#'  the results of an SARMA/SFARIMA-estimation, if available.
#' 
#'  If bandwidths are supplied to \code{dcs}, \code{summary.dcs} only prints
#'  the given bandwidths.
#' 
#' @return The function \code{summary.dcs} returns an object of class 
#'  \code{summary_dcs} including \tabular{ll}{
#'  \code{h_opt} \tab estimated optimal bandwidth from the IPI-procedure. \cr
#'  \code{c_f} \tab estimated variance factor. \cr
#'  \code{iterations} \tab number of iterations of the IPI-procedure. \cr
#'  \code{time_used} \tab time spend searching for optimal bandwidths (not 
#'   overall runtime of the function). \cr
#'  \code{var_est} \tab estimated variance model. Has class "sarma" if an
#'   SARMA model is used and class "sfarima" if an SFARIMA model is used.\cr
#'  \code{var_model_id} \tab identifier for the variance model estimated. \cr
#'  \code{var_model_order} \tab order of the estimated variance model, if either
#'   SARMA or SFARIMA is used. \cr
#'  \code{dcs_options} \tab an object of class \code{cds_options} containing the
#'   initial options of the dcs procedure. \cr
#' }
#' 
#' @examples
#' y <- y.norm1 + matrix(rnorm(101^2), nrow = 101, ncol = 101)
#' dcs_object <- dcs(y)
#' summary(dcs_object)
#' 
#' @export
#' 

summary.dcs = function(object, ...)
{
  var_temp = object$var_est$model
  if (object$dcs_options$var_model == "iid")
  {
    var_model_id = "iid"
    var_model_order = NA
  } else if (object$dcs_options$var_model == "sfarima_RSS") {
    var_model_id = "sfarima"
    var_model_order = list(ar = dim(var_temp$ar), ma = dim(var_temp$ma))
  } else {
    var_model_id = "sarma"
    var_model_order = list(ar = dim(var_temp$ar), ma = dim(var_temp$ma))
  }
  
  summary_dcs = list(h = object$h, c_f = object$c_f,
                       iterations = object$iterations, 
                       time_used = object$time_used,
                       var_est = object$var_est,
                       var_model_id = var_model_id,
                       var_model_order = var_model_order,
                       dcs_options = object$dcs_options)
  
  attr(summary_dcs, "h_select_auto") = attr(object, "h_select_auto")
                     
  class(summary_dcs) = "summary_dcs"
  return(summary_dcs)
}

#----------------------------Print (Summary) Methods---------------------------#

#' Print the Summary of a DCS estimation
#' 
#' @description \code{print} method for class \code{"summary_dcs"}
#' 
#' @param x An object of class \code{"summary_dcs"}.
#' @param ... Additional arguments passed to \code{print.summary_dcs}.
#' 
#' @return No return value.
#' 
#' @seealso \code{\link{summary.dcs}}
#' 
#' @export

print.summary_dcs = function(x, ...)
{
  args_list = list(...)
  if (!exists("digits", args_list))
  {
    digits = max(3, getOption("digits") - 3)
  }
  name_kern_fcn = paste0("MW", x$dcs_options$kern_par[1],
                       x$dcs_options$kern_par[2], "0")
  if (x$dcs_options$type == "KR")
  {
    reg_type = paste0("kernel regression")
  } else if (x$dcs_options$type == "LP") {
    reg_type = paste0("local polynomial regression")
  }
  
  # when automatic bandwidth selection is selected
  if (attr(x, "h_select_auto") == TRUE)
  {
    cat(class(x), "\n")
    cat("------------------------------------------", "\n")
    cat("DCS with automatic bandwidth selection:\n")
    cat("------------------------------------------", "\n")
    cat("Results of ", reg_type, ":\n", sep = "")
    cat("Estimated Bandwidths: \t h_x:\t", signif(x$h[1], 
                                                  digits = digits), "\n")
    cat("\t \t \t h_t:\t", signif(x$h[2], digits = digits), "\n")
    cat("Variance Factor: \t c_f:\t", signif(x$c_f, digits = digits), "\n")
    cat("Iterations:\t\t\t", x$iterations, "\n")
    cat("Time used (seconds):\t \t", signif(x$time_used, 
                                           digits = digits), "\n")
    cat("------------------------------------------", "\n")
    cat("Variance Model:\t", x$var_model_id, "\n")
    if (x$var_model_id == "iid") {
      cat("------------------------------------------", "\n")
      cat("sigma:\t", x$var_est$model$sigma, "\n")
      cat("stationary:\tTRUE\n")
    } else if (x$var_model_id == "sarma") {
      print.summary_sarma(summary.sarma(x$var_est))
    } else if (x$var_model_id == "sfarima") {
      print.summary_sfarima(summary.sfarima(x$var_est))
    }
    cat("------------------------------------------", "\n")
    cat("See used parameter with \"$dcs_options\".\n")
    
  # when given bandwidths are used.
  } else if (attr(x, "h_select_auto") == FALSE) {
    cat(class(x), "\n")
    cat("------------------------------------------", "\n")
    cat("DCS with given bandwidths:\n")
    cat("------------------------------------------", "\n")
    cat("Used bandwidths: \t h_x:\t", x$h[1], "\n")
    cat("\t \t \t h_t:\t", x$h[2], "\n")
    cat("------------------------------------------", "\n")
    cat("See used parameter with \"$dcs_options\".\n")
  }
}
  
#' Summarize Results from Double Conditional Smoothing
#' 
#' @description \code{print} method for class \code{"dcs"}
#' 
#' @section Details:
#' \code{print.dcs} prints a short summary of an object of class \code{dcs},
#' only including bandwidths and the estimated variance coefficient (only if
#' automatic bandwidth selection is used).
#' 
#' @param x an object of class "dcs", usually, a result of a call to 
#' \code{\link{dcs}}.
#' @param ... Additional arguments passed to \code{print.dcs}.
#' 
#' @return No return value.
#' 
#' @seealso \code{\link{plot.dcs}}, \code{\link{print.dcs_options}}
#' 
#' @examples
#' y <- y.norm1 + matrix(rnorm(101^2), nrow = 101, ncol = 101)
#' dcs_object <- dcs(y)
#' print(dcs_object)
#' dcs_object
#' 
#' @export
#'

print.dcs = function(x, ...)
{
  # when automatic bandwidth selection is selected
  if (attr(x, "h_select_auto") == TRUE)
  {
    cat(class(x), "\n")
    cat("------------------------------------------", "\n")
    cat("DCS with automatic bandwidth selection:\n")
    cat("------------------------------------------", "\n")
    cat(" Selected Bandwidths:\n")
    cat("\t\th_x:", signif(x$h[1], digits = 5), "\n")
    cat("\t\th_t:", signif(x$h[2], digits = 5), "\n")
    cat(" Variance Factor:\n")
    cat("\t\tc_f:", signif(x$c_f, digits = 5), "\n")
    cat("------------------------------------------", "\n")
  # when given bandwidths are used.
  } else if (attr(x, "h_select_auto") == FALSE) {
    cat(class(x), "\n")
    cat("------------------------------", "\n")
    cat("DCS with given bandwidths:\n")
    cat("------------------------------", "\n")
    cat("Used Bandwidths:\n")
    cat("\th_x:", x$h[1], "\n")
    cat("\th_t:", x$h[2], "\n")
    cat("------------------------------")
  }
}

#-------------------------------DCS Options------------------------------------#

#' Print and Summarize Options for Double Conditional Smoothing
#' 
#' @description \code{summary} method for class \code{"dcs_options"}
#' 
#' @section Details:
#' \code{print.dcs_options} prints the main options and 
#' \code{summary.dcs_options} prints main and advanced (IPI) options used for
#' the \code{\link{dcs}} function. Arguments should be an object of class
#' \code{"dcs_options"}.
#' 
#' @param object an object of class "dcs_options", usually, a result of a call
#' to \code{\link{set.options}}.
#' @param ... Additional arguments passed to \code{summary.dcs_options}.
#' 
#' @return No return value.
#' 
#' @seealso \code{\link{print.dcs}}, \code{\link{print.dcs_options}}
#' 
#' @examples
#' ## Default options
#' myOpt <- set.options()
#' print(myOpt)
#' summary(myOpt)
#' 
#' ## Use Kernel regression
#' myOpt <- set.options(type = "KR")
#' print(myOpt)
#' summary(myOpt)
#' 
#' @export

summary.dcs_options = function(object, ...)
{
  cat(class(object), "\n")
  cat("---------------------------------------", "\n")
  cat("options for DCS \t rows \t cols \n")
  cat("---------------------------------------", "\n")
  
  # when Kernel Regression is selected
  if (object$type == "KR")
  {
    cat("type: kernel regression \n", sep = "")
    cat("kernels used: \t \t", object$kerns[1], "\t", object$kerns[2], 
        "\n", sep = "")
    cat("derivative: \t \t", object$drv[1], "\t", object$drv[2], "\n", sep = "")
    cat("variance model: \t", object$var_est, "\n", sep = "")
    cat("---------------------------------------", "\n", sep = "")
  } else if (object$type == "LP") {  
    # when Local Polynomial Regression is selected
    cat("type: local polynomial regression \n", sep = "")
    cat("kernel order: \t \t", object$kerns[1], "\t", object$kerns[2], "\n",
        sep = "")
    cat("derivative: \t \t", object$drv[1], "\t", object$drv[2], "\n", sep = "")
    cat("polynomial order: \t", object$p_order[1], "\t", object$p_order[2], 
        "\n", sep = "")
    cat("variance model: \t", object$var_model, "\n", sep = "")
    cat("---------------------------------------", "\n")
  }
  
  ipi = object$IPI_options
  
  cat("IPI options:\n")
  cat("inflation parameters \t", ipi$infl_par[1], "\t", ipi$infl_par[2], "\n",
      sep = "")
  cat("inflation exponents \t", ipi$infl_exp[1], "\t", ipi$infl_exp[2], "\n",
      sep = "")
  cat("trim \t \t \t", ipi$trim[1], "\t", ipi$trim[2], "\n", sep = "")
  cat("constant window width \t", ipi$const_window, "\n", sep = "")
  cat("---------------------------------------", "\n")
}


#' Print and Summarize Options for Double Conditional Smoothing
#' 
#' @description \code{print} method for class \code{"dcs_options"}
#' 
#' @section Details:
#' \code{print.dcs_options} prints the main options and 
#' \code{summary.dcs_options} prints main and advanced (IPI) options used for
#' the \code{\link{dcs}} function. Arguments should be an object of class
#' \code{"dcs_options"}.
#' 
#' @param x an object of class "dcs_options", usually, a result of a call
#' to \code{\link{set.options}}.
#' @param ... Additional arguments passed to \code{print.dcs_options}.
#' 
#' @return No return value.
#' 
#' @seealso \code{\link{print.dcs}}, \code{\link{summary.dcs_options}}
#' 
#' @examples
#' ## Default options
#' myOpt <- set.options()
#' print(myOpt)
#' summary(myOpt)
#' 
#' ## Use Kernel regression
#' myOpt <- set.options(type = "KR")
#' print(myOpt)
#' summary(myOpt)
#' 
#' @export

print.dcs_options = function(x, ...)
{
  cat(class(x), "\n")
  cat("---------------------------------------", "\n")
  cat("options for DCS \trows \tcols \n")
  cat("---------------------------------------", "\n")
  
  # when Kernel Regression is selected
  if (x$type == "KR")
  {
    cat("type: kernel regression \n", sep = "")
    cat("kernels used: \t \t", x$kerns[1], "\t", x$kerns[2], 
        "\n", sep = "")
    cat("derivative: \t \t", x$drv[1], "\t", x$drv[2], "\n", sep = "")
    cat("variance model: \t", x$var_model, "\n", sep = "")
    cat("---------------------------------------", "\n", sep = "")
  } else if (x$type == "LP") {  
    # when Local Polynomial Regression is selected
    cat("type: local polynomial regression \n", sep = "")
    cat("kernel order: \t \t", x$kerns[1], "\t", x$kerns[2], "\n",
        sep = "")
    cat("derivative: \t \t", x$drv[1], "\t", x$drv[2], "\n", sep = "")
    cat("polynomial order: \t", x$p_order[1], "\t", x$p_order[2], 
        "\n", sep = "")
    cat("variance model: \t", x$var_mode, "\n", sep = "")
    cat("---------------------------------------", "\n")
  }
}

#----------------------------------Other Methods-------------------------------#

#' Residuals of "dcs"-object
#' 
#' @description Returns the residuals of an object of class \code{"dcs"}.
#' 
#' @param x an object of class \code{"dcs"}, usually the result of a call to
#'  \code{\link{dcs}}.
#' @param ... Additional arguments passed to \code{residuals.dcs}.
#' 
#' @return Returns the \eqn{n_x \times n_t}{n_x x n_t}-matrix of residuals.
#' 
#' @seealso \code{\link{dcs}}
#' 
#' @examples
#' y = y.norm1 + matrix(rnorm(101^2), nrow = 101, ncol = 101)
#' dcs_object = dcs(y)
#' residuals(dcs_object)
#' 

residuals.dcs = function(x, ...)
{
  return(x$R)
}

#' Contour Plot for the Double Conditional Smoothing
#' 
#' @description \code{plot} method for class \code{"dcs"}
#' 
#' @section Details:
#' \code{plot.dcs} provides a contour plot of either the original data (1),
#'  smoothed surface (2) or residuals (3).
#' 
#' @param x an object of class "dcs_options", usually, a result of a call to 
#' \code{\link{set.options}}.
#' @param ... Additional arguments passed to \code{print.dcs_options}. The
#'  argument \code{plot_choice} overrides the prompt to specify a plot, can be 
#'  \code{c(1, 2, 3)}.
#'  
#' @return No return value.
#' 
#' @seealso \code{\link{surface.dcs}} to plot the surface.
#' 
#' @examples
#' ## Contour plot of smoothed surface
#' y <- y.norm1 + matrix(rnorm(101^2), nrow = 101, ncol = 101)
#' dcs_object <- dcs(y)
#' plot(dcs_object, plot_choice = 2)
#' 
#' @export
#' 

plot.dcs = function(x, ...)
{
  if(all(class(x) != "dcs"))
  {
    stop("Object is not of class \"dcs\".")
  }
  
  args_list = list(...)
  
  choice_names <- c("original observations", "smoothed surface",
                    "residuals")
  
  if (!exists("plot_choice", args_list))
  {
    cat("Plot choices for dcs object:", fill = TRUE)
    choices <- c(1, 2, 3)
    choices_df <- data.frame(choices)
    colnames(choices_df) <- ""
    rownames(choices_df) <- choice_names
    print.data.frame(choices_df)
    plot_choice <- readline(prompt="Please enter the corresponding number: ")
    plot_choice <- as.numeric(plot_choice)
  } else if (!(args_list$plot_choice %in% 1:3)) {
    stop("Invalid value in argument \"plot_choice\". Use c(1, 2, 3).")
  } else {
    plot_choice = args_list$plot_choice
  }
  
  if (plot_choice == 1) {
    Y = x$Y
  } else if (plot_choice == 2) {
    Y = x$M
  } else if (plot_choice == 3) {
    Y = x$R
  } else {
    stop(plot_choice, " is not a valid plot-type.")
  }
  
  if (exists("color", args_list))
  {
    color = args_list$color
  } else {
    color = c("#444C5C", "#78A5A3", "#E1B16A", "#CE5A57")
  }
  
  vec_XT = base::expand.grid(x$X, x$T)
  col_vec = plot.dcs.colors(Y, color = color)
  main_title = paste("Contour plot of ", choice_names[plot_choice])
  graphics::plot(vec_XT[, 1], vec_XT[, 2], pch = 15, col = col_vec,
       xlab = "T", ylab = "X", main = main_title)
}

#-----------------------Additional Functions for Methods-----------------------#

.order.to.string = function(model_order)
{
  paste0("((", model_order$ar[1], ",", model_order$ar[2], "),(", 
         model_order$ma[1], ",", model_order$ma[2], "))")
}


.onUnload <- function(libpath) { library.dynam.unload("DCSmooth", libpath) }