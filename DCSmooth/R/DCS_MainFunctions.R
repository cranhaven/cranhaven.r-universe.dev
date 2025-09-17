################################################################################
#                                                                              #
#                      DCSmooth Package: User Functions                        #
#                                                                              #
################################################################################

### Includes the main functions for the DCS package

  # set.options (exported)
  # dcs (exported)
  # surface.dcs (exported)
  # kernel.assign (exported)
  # kernel.list (exported)


#------------------------Set Options via Function------------------------------#

#' Set Options for the DCS procedure
#' 
#' @param type either local polynomial regression (\code{"LP"}, the default) or 
#'  kernel regression (\code{"KR"}).
#' @param kerns a character vector of length 2 containing the identifier for the
#'  kernels to be used in kernel regression. Weighting functions in local
#'  polynomial regression are computed according to the identifier. Default value
#'  is \code{MW_220}, the Mueller-Wang kernel of order \eqn{(2, 2, 0)}. If only
#'  a single value is provided, it is used as kernel in both directions.
#' @param drv A non-negative vector of length 2, containing the derivative
#'  orders to be estimated from the given data. The default is \code{c(0, 0)}. For
#'  LP-regression, polynomial order is selected as \eqn{(\nu_1 + 1, \nu_2 + 1)}.
#'  If only a single value is provided, it is used as derivative in both 
#'  directions.
#' @param var_model the method of estimating the variance coefficient \eqn{c_f}. 
#'  Currently available are \code{var_model = c("iid", "sarma_HR", "sarma_sep",
#'  "sarma_RSS", "sfarima_RSS")}. Replacing the argument \code{var_model}. For
#'  code using \code{var_est}, the argument is converted to \code{var_model}.
#' @param ... Additional arguments passed to \code{set.options()}. This includes
#'  \code{IPI_options}, a list containing further options used by the iterative
#'  plug-in algorithm. For convenience, any of the options usually included in 
#'  the list \code{IPI_options} can be passed as argument directly to 
#'  \code{set.options} and will be converted into the \code{IPI_options} list.
#'  Further arguments accepted are \code{model_order} controlling the order of
#'  the variance model, if either an SARMA or SFARIMA model is used. This 
#'  argument is either a list of the form \code{list(ar = c(1, 1), ma = c(1, 1))}
#'  or specifies an order selection criterion from \code{c("aic", "bic", "gpac")}.
#'  If an order selection criterion is used, the argument \code{order_max} 
#'  controls the maximum order to be tested.
#' 
#' @section Details:
#' This function is used to set the options for bandwidth selection in the 
#' \code{dcs} function.
#' Detailed information can be found in the vignette.
#' 
#' @return  An object of class \code{"dcs_options"}.
#' 
#' @seealso \code{\link{dcs}}
#' 
#' @export
#' 
#' @examples
#' # See vignette("DCSmooth") for examples and explanation
#' 
#' set.options()
#' 
#' myOpt <- set.options(type = "KR", var_model = "iid")
#' y <- y.norm1 + matrix(rnorm(101^2), nrow = 101, ncol = 101)
#' dcs(y, dcs_options = myOpt)

set.options <- function(    # inside function with default values in arguments
  # standard options
  type      = "LP",        # either "LP" for local polynomial regression or
  # "KR" for kernel regression
  kerns     = c("MW_220", "MW_220"), # choose a kernel function
  drv       = c(0, 0),
  var_model = "iid",
  
  # advanced options in ellipsis
  ...
)
{
  # get ellipsis
  args_list <- list(...)
  
  args_names <- names(args_list)
  if (any(!(args_names %in% c("IPI_options", "model_order", "order_max",
                              "var_est", dcs_list_IPI, "delta"))))
  {
    arg_unknown <- args_names[which(!(args_names %in% 
                         c("IPI_options", "model_order", "order_max",
                           "var_est", dcs_list_IPI, "delta")))]
    stop("Unsupported argument(s) \"",arg_unknown, "\" in set.options().")
  }
  
  ### include old var_est options
  if (exists("var_est", args_list))
  {
    message("Note: option \"var_est\" is deprecated, argument is converted to ",
            "\"var_model\" automatically.")
    if (args_list$var_est == "iid") {
      var_model <- "iid"
    } else if (args_list$var_est == "qarma") {
      var_model <- "sarma_HR"
    } else if (args_list$var_est == "sarma") {
      var_model <- "sarma_sep"
    } else if (args_list$var_est == "sarma2") {
      var_model <- "sarma_RSS"
    } else if (args_list$var_est == "lm") {
      var_model <- "sfarima_RSS"
    } else if (args_list$var_est %in% c("qarma_gpac", "qarma_bic")) {
      args_list$var_model <- "sarma_HR"
      warning("For automatic order selection, use \"model_order\" in ",
              "\"dcs()\".")
    } else {
      stop("Unknown argument in \"var_est\". Use \"var_model\" instead.")
    }
  }
  
  ### set IPI_options
  if (exists("IPI_options", args_list))
  {
    IPI_options = args_list$IPI_options
    # check for argument "delta"
    if (exists("delta", IPI_options)) {
      IPI_options$trim <- IPI_options$delta
      message("IPI_options argument \"delta\" is deprecated. ",
              "Use \"trim\" instead.")
    }
  } else {
    IPI_options = list()
    # get IPI_options from higher-ranking list "args_list"
    if (exists("trim", args_list))
    {
      IPI_options$trim <- args_list$trim
    } else if (exists("delta", args_list)) {
      IPI_options$trim <- args_list$delta
      message("IPI_options argument \"delta\" is deprecated. ",
              "Use \"trim\" instead.")
    }
    if (exists("infl_par", args_list))
    {
      IPI_options$infl_par <- args_list$infl_par
    }
    if (exists("infl_exp", args_list))
    {
      IPI_options$infl_exp <- args_list$infl_exp
    }
    if (exists("const_window", args_list))
    {
      IPI_options$const_window <- args_list$const_window
    }
  }
  
  ### set model orders
  add_options <- list()
  
  if (exists("model_order", where = args_list))
  {
    exception.check.model_order(args_list$model_order, var_model)
    add_options$model_order <- args_list$model_order
  } else if (var_model %in% dcs_list_var_model && var_model != "iid") {
    add_options$model_order <- list(ar = c(1, 1), ma = c(1, 1))
  }
  if (exists("order_max", where = args_list) && 
      length(add_options$model_order) == 1 &&
      !is.list(add_options$model_order) &&
      add_options$model_order %in% c("aic", "bic", "gpac"))
  {
    exception.check.order_max(args_list$order_max)
    add_options$order_max = args_list$order_max
  } else if (length(add_options$model_order) == 1 &&
             !is.list(add_options$model_order) &&
             add_options$model_order %in% c("aic", "bic", "gpac")) {
    add_options$order_max = list(ar = c(1, 1), ma = c(1, 1))
  }
  
  ### check if inputs are vectors
  if (length(kerns) == 1) { kerns <- c(kerns, kerns) }
  if (length(drv) == 1) { drv <- c(drv, drv) }
  
  ### check inputs
  exception.check.options.input(type, kerns, drv, var_model, IPI_options)
  
  # change IPI_options for long memory estimation
  if (!exists("infl_exp", IPI_options) && var_model == "sfarima_RSS")
  {
    IPI_options$infl_exp <- c(0.5, 0.5)
  }
  if (!exists("infl_par", IPI_options) && var_model == "sfarima_RSS")
  {
    IPI_options$infl_par <- c(3, 1)
  }
  
  # Select options according to type ("LP", "KR")
  if (type == "LP")
  {
    p_order <- drv + 1
    if (!exists("infl_exp", IPI_options))
    {
      IPI_options$infl_exp <- c("auto", " ")
    }
    if (!exists("infl_par", IPI_options)) { IPI_options$infl_par <- c(2, 1) }
    if (!exists("trim", IPI_options)) { IPI_options$trim <- c(0.05, 0.05) }
    if (!exists("const_window", IPI_options))
    {
      IPI_options$const_window <- FALSE
    }
  } else if (type == "KR") {
    p_order <- NA
    if (!exists("infl_exp", IPI_options))
    {
      IPI_options$infl_exp <- c(0.5, 0.5)
    }
    if (!exists("infl_par", IPI_options)) { IPI_options$infl_par <- c(2, 1) }
    if (!exists("trim", IPI_options)) { IPI_options$trim <- c(0.05, 0.05) }
    if (!exists("const_window", IPI_options))
    {
      IPI_options$const_window <- FALSE
    }
  } else {
    stop("Unknown type \"", type, "\"")
  }
  
  options_list <- list(type = type, kerns = kerns, p_order = p_order,
                       drv = drv, var_model = var_model,
                       IPI_options = IPI_options,
                       add_options = add_options)
  
  # apply class to output object
  class(options_list) <- "dcs_options"
  
  exception.check.options(options_list)
  
  return(options_list)
}

#--------------Function for smoothing and bandwidth estimation----------------#

#' Nonparametric Double Conditional Smoothing for 2D Surfaces
#' 
#' \code{dcs} provides a double conditional nonparametric smoothing of the
#' expectation surface of a functional time series or a random field on a
#' lattice. Bandwidth selection is done via an iterative plug-in method.
#' 
#' @param Y A numeric matrix that contains the observations of the random field
#'   or functional time-series.
#' @param dcs_options An object of class \code{"dcs_options"}, specifying the
#'  parameters for the smoothing and bandwidth selection procedure.
#' @param h Bandwidth for smoothing the observations in \code{Y}. Can be a
#'  two-valued numerical vector with bandwidths in row- and column-direction.
#'  If the value is \code{"auto"} (the default), bandwidth selection will be 
#'  carried out by the iterative plug-in algorithm.
#' @param parallel A logical value indicating if parallel computing should be
#'  used for faster computation. Default value is \code{parallel = FALSE}.
#'  Parallelization seems to be efficient at above 400,000 observations.
#' @param ... Additional arguments passed to \code{dcs}. Currently supported are
#'  numerical vectors \code{X} and/or \code{T} containing the exogenous
#'  covariates with respect to the rows and columns.
#' 
#' @return \code{dcs} returns an object of class "dcs", including
#'  \tabular{ll}{
#'  \code{Y} \tab matrix of original observations. \cr
#'  \code{X, T} \tab vectors of covariates over rows (\code{X}) and columns 
#'   (\code{T}). \cr
#'  \code{M} \tab resulting matrix of smoothed values. \cr
#'  \code{R} \tab matrix of residuals of estimation, \eqn{Y - M}. \cr
#'  \code{h} \tab optimized or given bandwidths. \cr
#'  \code{c_f} \tab estimated variance coefficient. \cr
#'  \code{var_est} \tab estimated variance model. If the variance function is
#'   modeled by an SARMA/SFARIMA, \code{var_est} is an object of class "sarma"/
#'   "sfarima".\cr
#'  \code{dcs_options} \tab an object of class \code{cds_options} containing the
#'   initial options of the dcs procedure. \cr
#'  \code{iterations} \tab number of iterations of the IPI-procedure. \cr
#'  \code{time_used} \tab time spend searching for optimal bandwidths (not
#'   overall runtime of the function). \cr
#' }
#' 
#' @section Details:
#' See the vignette for a more detailed description of the function.
#' 
#' @references
#'  Schäfer, B. and Feng, Y. (2021). Fast Computation and Bandwidth Selection 
#'  Algorithms for Smoothing Functional Time Series. Working Papers CIE 143, 
#'  Paderborn University.
#' 
#' @seealso \code{\link{set.options}}
#' 
#' @examples 
#' # See vignette("DCSmooth") for examples and explanation
#' 
#' y <- y.norm1 + matrix(rnorm(101^2), nrow = 101, ncol = 101)
#' dcs(y)
#' 
#' @export
#' 

dcs <- function(Y, dcs_options = set.options(), h = "auto", 
                parallel = FALSE, ...)
{
  #-------Front Matter-------#
  
  # check for correct inputs of data and options
  exception.check.Y(Y)
  exception.check.bndw(h, dcs_options)
  exception.check.options(dcs_options)
  exception.check.parallel(parallel)
  n_x = dim(Y)[1]; n_t = dim(Y)[2]
  
  # process ellipsis arguments from (...)
  {
    # set up vectors for X and T if neccessary
    args_list <- list(...)
    
    exception.check.args_list(args_list)
    
    if (!exists("X", where = args_list))
    {
      X <- 0:(n_x - 1)/(n_x - 1)
    } else {
      X <- args_list$X
    }
    if (!exists("T", where = args_list))
    {
      T <- 0:(n_t - 1)/(n_t - 1)
    } else {
      T <- args_list$T
    }
    exception.check.XT(Y, X, T)
  }
  
  add_options <- list()
  add_options$parallel <- parallel
  
  if (exists("add_options", dcs_options))
  {
    add_options <- append(add_options, dcs_options$add_options)
  }
  
  #-------Bandwidth Selection-------#
  
  # check if given bandwidths exist
  if (length(h) == 1 && h[1] == "auto")
  {
    h_select_auto <- TRUE
    
    time_start <- Sys.time() # get starting time for performance measuring
  
    # bandwidth selection process
    if (dcs_options$type == "KR")
    {
      if (any(dcs_options$drv > 0))
      {
        dcs_options_0 <- dcs_options
        dcs_options_0$drv <- c(0, 0)
        kernel_0_id <- paste0(sub("^([[:alpha:]]*).*", "\\1", dcs_options$kerns),
                             "_220")
        dcs_options_0$kerns <- kernel_0_id# TODO: Change later to correct orders
        h_aux_obj <- KR.bndw(Y, dcs_options_0, add_options, cf_est = TRUE)
        cf_est <- list(var_coef = h_aux_obj$var_coef,
                      var_model = h_aux_obj$var_model)
      } else {
        cf_est <- TRUE
      }
      
      h_select_obj <- KR.bndw(Y, dcs_options, add_options, cf_est = cf_est)
      h_opt <- pmin(h_select_obj$h_opt, c(0.45, 0.45)) # max bndw for KR
    
    } else if (dcs_options$type == "LP") {
      if (any(dcs_options$drv > 0))
      {
        dcs_options_0 <- dcs_options
        dcs_options_0$drv <- c(0, 0)
        dcs_options_0$p_order <- c(1, 1)
        kernel_0_id <- paste0(sub("^([[:alpha:]]*).*", "\\1", dcs_options$kerns),
                             "_220")
        dcs_options_0$kerns <- kernel_0_id# TODO: Change later to correct orders
        h_aux_obj <- LP.bndw(Y, dcs_options_0, add_options, cf_est = TRUE)
        cf_est <- list(var_coef = h_aux_obj$var_coef,
                      var_model = h_aux_obj$var_model)
      } else {
        cf_est <- TRUE
      }
      
      h_select_obj <- LP.bndw(Y, dcs_options, add_options, cf_est = cf_est)
      h_opt <- h_select_obj$h_opt
    }
    
    if (h_select_obj$var_model$stnry == FALSE)
    {
      warning("error model is not stationary.")
    }
    
    time_end <- Sys.time()
    
  } else {
    h_opt <- h
    h_select_auto <- FALSE
    time_end <- time_start <- Sys.time()
  }
  
#-------Estimation of Result-------#
  
  if (dcs_options$type == "KR") # kernel regression
  {
    ### prepare kernel functions
      kernel_x <- kernel_fcn_assign(dcs_options$kerns[1])
      kernel_t <- kernel_fcn_assign(dcs_options$kerns[2])
      
    ### check bandwidth
      if (any(h_opt > c(0.45, 0.45)))
      {
        h_opt <- pmin(h_opt, c(0.45, 0.45))
        warning("Bandwidth h too large for \"KR\", changed to largest ",
                "working value.")
      }
    
    # Surface to estimate
    Y_smth_out <- KR_dcs_const0(yMat = Y, hVec = h_opt,
                                drvVec = dcs_options$drv, 
                                kernFcnPtrX = kernel_x,
                                kernFcnPtrT = kernel_t)
    
    if (any(dcs_options$drv > 0))
    {
      kernel_0 <- kernel_fcn_assign(kernel_0_id[1])
      R <- Y - KR_dcs_const0(yMat = Y, hVec = h_aux_obj$h_opt,
                                drvVec = c(0, 0), 
                                kernFcnPtrX = kernel_0,
                                kernFcnPtrT = kernel_0)
    } else {
      R <- Y - Y_smth_out 
    }
  } else if (dcs_options$type == "LP") {     # local polynomial regression
    ### prepare weight functions
      # set variables for weight type
      kern_type_vec <- sub("^([[:alpha:]]*).*", "\\1", dcs_options$kern)
      # extract weighting type
      mu_vec <- as.numeric(substr(dcs_options$kern, 
                                 nchar(dcs_options$kern) - 1,
                                 nchar(dcs_options$kern) - 1))
      # extract kernel parameter mu
      weight_x <- weight_fcn_assign(kern_type_vec[1])
      weight_t <- weight_fcn_assign(kern_type_vec[2])
    
    ### check bandwidth
      if (any(h_opt < (dcs_options$p_order + 2)/(c(n_x, n_t) - 1)))
      {
        h_opt <- pmax(h_opt, (dcs_options$p_order + 2)/(c(n_x, n_t) - 1))
        warning("Bandwidth h too small for \"LP\", changed to smallest ",
                "working value.")
      }
      
    Y_smth_out <- LP_dcs_const0_BMod(yMat = Y, hVec = h_opt, polyOrderVec = 
                            dcs_options$p_order, drvVec = dcs_options$drv,
                            muVec = mu_vec, weightFcnPtr_x = weight_x,
                            weightFcnPtr_t = weight_t)
    
    if (any(dcs_options$drv > 0))
    {
      R <- Y - LP_dcs_const0_BMod(yMat = Y, hVec = h_aux_obj$h_opt,
                                  polyOrderVec = c(1, 1), drvVec = c(0, 0),
                                  muVec = c(2, 2), weightFcnPtr_x = weight_x,
                                  weightFcnPtr_t = weight_t)
    } else {
      R <- Y - Y_smth_out 
    }
  }
  
  # Variance Model
  var_model_est <- cf.estimation(R, dcs_options, add_options)
  var_model <- var_model_est$model
  
  
#-------Preperation of Output-------#
  if (h_select_auto == TRUE)
  {
    dcs_out <- list(Y = Y, X = X, T = T, M = Y_smth_out, R = R, h = h_opt,
                   c_f = h_select_obj$var_coef, 
                   var_est = var_model_est$model_est,
                   dcs_options = dcs_options,
                   iterations = h_select_obj$iterations,
                   time_used = difftime(time_end, time_start, units = "secs"))
    attr(dcs_out, "h_select_auto") <- h_select_auto
  } else if (h_select_auto == FALSE) {
    dcs_out <- list(X = X, T = T, Y = Y, M = Y_smth_out, R = R, h = h_opt,
                   c_f = NA, var_est = var_model_est$model_est,
                   dcs_options = dcs_options,
                   iterations = NA, time_used = NA)
    attr(dcs_out, "h_select_auto") <- h_select_auto
  }
  
  # apply class to output object
  class(dcs_out) <- "dcs"

  return(dcs_out)
}

#-------------------Function for plotting smoothed surface--------------------#

#' 3D Surface Plot of "dcs"-object or numeric matrix
#' 
#' @section Details:
#' \code{surface.dcs} uses the plotly device to plot the 3D surface of the given
#' \code{"dcs"}-object or matrix. If a "dcs"-object is passed to the function, 
#'  it can be chosen between plots of the original data (1), smoothed surface 
#'  (2) and residuals (3).
#' @param Y an object of class \code{"dcs"} or a numeric matrix that contains the
#'   values to be plotted.
#' @param trim a numeric vector with two values specifying the percentage of
#'  trimming applied to the boundaries of the surface to plot. Useful for
#'  derivative estimation.
#' @param plot_choice override the prompt to specify a plot, can be 
#'  \code{c(1, 2, 3)}.
#' @param ... optional arguments passed to the plot function.
#' 
#' @return \code{dcs.3d} returns an object of class "plotly" and "htmlwidget".
#' 
#' @seealso \code{\link{plot.dcs}}
#' 
#' @examples
#' # See vignette("DCSmooth") for examples and explanation
#' 
#' smth <-  dcs(y.norm1 + rnorm(101^2))
#' surface.dcs(smth, trim = c(0.05, 0.05), plot_choice = 2)
#' 
#' @export
#' 

surface.dcs <- function(Y, trim = c(0, 0), plot_choice = "choice", ...)
{
  # check exceptions for argument "trim"
  if ((length(trim) != 2) || !is.numeric(trim) || any(trim < 0) ||
      any(trim >= 0.5))
  {
    stop("Only values [0, 0.5) allowed for argument \"trim\".")
  }
  
  # procedure if "dcs"-object is passed
  if (class(Y)[1] == "dcs")
  {
    fcn_arg <- list(...)
    
    if (plot_choice == "choice")
    {
      cat("Plot choices for dcs object:", fill = TRUE)
      choices <- c(1, 2, 3)
      choice_names <- c("original observations", "smoothed surface",
                        "residuals")
      choices_df <- data.frame(choices)
      colnames(choices_df) <- ""
      rownames(choices_df) <- choice_names
      print.data.frame(choices_df)
      plot_choice <- readline("Please enter the corresponding number: ")
      plot_choice <- as.numeric(plot_choice)
    } else if (!(plot_choice %in% 1:3)) {
      stop("Invalid value in argument \"plot_choice\". Use c(1, 2, 3).")
    }
    
    index_x = ceiling(trim[1] * dim(Y$Y)[1]):
                (dim(Y$Y)[1] - floor(trim[1] * dim(Y$Y)[1]))
    index_t = ceiling(trim[2] * dim(Y$Y)[2]):
                (dim(Y$Y)[2] - floor(trim[2] * dim(Y$Y)[2]))
    
    if (plot_choice == 1) {
      Y_mat <- Y$Y[index_x, index_t]
    } else if (plot_choice == 2) {
      Y_mat <- Y$M[index_x, index_t]
    } else if (plot_choice == 3) {
      Y_mat <- Y$R[index_x, index_t]
    } else {
      stop(plot_choice, " is not a valid plot-type.")
    }
    
    .plotly.3d(Y = Y_mat, X = Y$X[index_x], T = Y$T[index_t], ...)
  } else {
    index_x <- ceiling(trim[1] * dim(Y)[1]):
               (dim(Y)[1] - floor(trim[1] * dim(Y)[1]))
    index_t <- ceiling(trim[2] * dim(Y)[2]):
               (dim(Y)[2] - floor(trim[2] * dim(Y)[2]))
    
    .plotly.3d(Y = Y[index_x, index_t], ...)
  }
}

#-------------------Assign Kernel from Package to Function---------------------#

#' Assign a Kernel Function
#' 
#' @section Details:
#' \code{kernel.assign} sets a pointer to a specified kernel function available 
#'  in the DCSmooth package. The kernels are boundary kernels of the form
#'  \eqn{K(u,q)}, where \eqn{u \in [-1, q]}{u = [-1, q]} and \eqn{q \in [0, 1]}
#'  {q = [0, 1]}. Kernels are of the Müller-Wang type ("MW"), Müller type ("M")
#'  or truncated kernels ("TR").
#'  
#' @param kernel_id a string specifying the kernel identifier as given in the
#'  details.
#' 
#' @return \code{kernel.assign} returns an object of class "function". This
#'  function takes two arguments, a numeric vector in the first argument and a
#'  single number in the second. The function itself will return a matrix with
#'  one column and the same number of rows as the input vector.
#'  
#' @references
#'  Müller, H.-G. and Wang, J.-L. (1994). Hazard rate estimation under random
#'  censoring with varying kernels and bandwidths. Biometrics, 50:61-76.
#'  
#'  Müller, H.-G. (1991). Smooth optimum kernel estimators near endpoints.
#'  Biometrika, 78:521-530.
#'  
#'  Feng, Y. and Schäfer B. (2021). Boundary Modification in Local Regression.
#'  Working Papers CIE 144, Paderborn University.
#' 
#' @seealso \code{\link{kernel.list}}
#' 
#' @examples
#'  # See vignette("DCSmooth") for further examples and explanation
#' 
#' u <- seq(from = -1, to = 0.5, length.out = 151)
#' kern_MW220 <- kernel.assign("MW_220")
#' k <- kern_MW220(u, 0.5)
#' plot(u, k, type = "l") 
#' 
#' @export
#' 

kernel.assign = function(kernel_id)
{
  # check for correct input
  if (!(kernel_id %in% dcs_list_kernels))
  {
    stop("unknown kernel identifier. See available kernels with ",
         "\"kernel.list()\".")
  } else {
    kernel_fcn_id <- paste0("kern_fcn_", gsub("_", "", kernel_id))
    kern_out <- get(kernel_fcn_id)
  }
  
  return(kern_out)
}

#' Print a list of available kernels in the DCSmooth package
#' 
#' @section Details:
#' \code{kernel.list} is used to get a list of available kernels in the DCSmooth
#'  package. 
#'  
#' \code{kernel.list} prints a list of identifiers \code{kernel_id} of available
#'  kernels in the DCSmooth package. The available kernel types are "T": 
#'  truncated, "MW": Müller-Wang boundary correction, "M": Müller boundary
#'  correction.
#'  
#' @param print Logical value. Should the list be printed to the console? If
#'  \code{TRUE} (the default), the list is printed to the console, if
#'  \code{FALSE} the list of identifiers is returned from the function as
#'  (surprise!) a list.
#'  
#' @return If \code{print = FALSE}, a list is returned containing the kernel
#'  identifiers
#'  
#' @references
#'  Müller, H.-G. and Wang, J.-L. (1994). Hazard rate estimation under random
#'  censoring with varying kernels and bandwidths. Biometrics, 50:61-76.
#'  
#'  Müller, H.-G. (1991). Smooth optimum kernel estimators near endpoints.
#'  Biometrika, 78:521-530.
#'  
#'  Feng, Y. and Schäfer B. (2021). Boundary Modification in Local Regression.
#'  Working Papers CIE 144, Paderborn University.
#' 
#' @seealso \code{\link{kernel.assign}}
#' 
#' @examples
#'  # See vignette("DCSmooth") for further examples and explanation
#'  
#'  kernel.list()
#' 
#' @export

kernel.list = function(print = TRUE)
{
  MW_kerns <- dcs_list_kernels[grepl("MW_", dcs_list_kernels)]
  M_kerns <- dcs_list_kernels[grepl("M_", dcs_list_kernels)]
  T_kerns <- dcs_list_kernels[grepl("T_", dcs_list_kernels)]
  
  if (isTRUE(print))
  {
    cat("Kernels available in the DCSmooth package:\n")
    cat("------------------------------------------", "\n")
    cat("M\u00FCller-Wang type kernels:\n")
    cat(MW_kerns, "\n", fill = 42)
    cat("M\u00FCller type kernels:\n")
    cat(M_kerns, "\n", fill = 42)
    cat("Truncated kernels:\n")
    cat(T_kerns, "\n", fill = 42)
  } else if (isFALSE(print)) {
    kernel_list <- list(MW_kerns = MW_kerns, M_kerns = M_kerns,
                       T_kerns = T_kerns)
    return(kernel_list)
  }
}