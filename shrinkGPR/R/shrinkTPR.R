#' Student-t Process Regression with Shrinkage and Normalizing Flows
#'
#' \code{shrinkTPR} implements Student-t process regression (TPR), as described in Shah et al. (2014), with a hierarchical shrinkage prior for hyperparameter estimation,
#' incorporating normalizing flows to approximate the posterior distribution. The function facilitates model specification, optimization,
#' and training, including support for early stopping, user-defined kernels, and flow-based transformations.
#'
#' This implementation provides a computationally efficient framework for TPR, enabling flexible modeling of mean and covariance structures.
#' Users can specify custom kernel functions, flow transformations, and hyperparameter configurations to adapt the model to their data.
#'
#' @param formula object of class "formula": a symbolic representation of the model for the covariance equation, as in \code{\link{lm}}.
#' The response variable and covariates are specified here.
#' @param data \emph{optional} data frame containing the response variable and the covariates. If not found in \code{data},
#' the variables are taken from \code{environment(formula)}. No \code{NA}s are allowed in the response variable or covariates.
#' @param a positive real number controlling the behavior at the origin of the shrinkage prior for the covariance structure. The default is 0.5.
#' @param c positive real number controlling the tail behavior of the shrinkage prior for the covariance structure. The default is 0.5.
#' @param formula_mean \emph{optional} formula for the linear mean equation. If provided, the covariates for the mean structure
#' are specified separately from the covariance structure. A response variable is not required in this formula.
#' @param a_mean positive real number controlling the behavior at the origin of the shrinkage for the mean structure. The default is 0.5.
#' @param c_mean positive real number controlling the tail behavior of the shrinkage prior for the mean structure. The default is 0.5.
#' @param sigma2_rate positive real number controlling the prior rate parameter for the residual variance. The default is 10.
#' @param nu_alpha positive real number controlling the shape parameter of the shifted gamma prior for the degrees of freedom of
#' the Student-t process. The default is 0.5.
#' @param nu_beta positive real number controlling the scale parameter of the shifted gamma prior for the degrees of freedom of
#' the Student-t process. The default is 2.
#' @param kernel_func function specifying the covariance kernel. The default is \code{\link{kernel_se}}, a squared exponential kernel.
#' For guidance on how to provide a custom kernel function, see Details.
#' @param n_layers positive integer specifying the number of flow layers in the normalizing flow. The default is 10.
#' @param n_latent positive integer specifying the dimensionality of the latent space for the normalizing flow. The default is 10.
#' @param flow_func function specifying the normalizing flow transformation. The default is \code{\link{sylvester}}.
#' For guidance on how to provide a custom flow function, see Details.
#' @param flow_args \emph{optional} named list containing arguments for the flow function. If not provided, default arguments are used.
#' For guidance on how to provide a custom flow function, see Details.
#' @param n_epochs positive integer specifying the number of training epochs. The default is 1000.
#' @param auto_stop logical value indicating whether to enable early stopping based on convergence. The default is \code{TRUE}.
#' @param cont_model \emph{optional} object returned from a previous \code{shrinkTPR} call, enabling continuation of training from the saved state.
#' @param device \emph{optional} device to run the model on, e.g., \code{torch_device("cuda")} for GPU or \code{torch_device("cpu")} for CPU.
#' Defaults to GPU if available; otherwise, CPU.
#' @param display_progress logical value indicating whether to display progress bars and messages during training. The default is \code{TRUE}.
#' @param optim_control \emph{optional} named list containing optimizer parameters. If not provided, default settings are used.
#'
#' @return A list object of class \code{shrinkTPR}, containing:
#' \item{\code{model}}{The best-performing trained model.}
#' \item{\code{loss}}{The best loss value (ELBO) achieved during training.}
#' \item{\code{loss_stor}}{A numeric vector storing the ELBO values at each training iteration.}
#' \item{\code{last_model}}{The model state at the final iteration.}
#' \item{\code{optimizer}}{The optimizer object used during training.}
#' \item{\code{model_internals}}{Internal objects required for predictions and further training, such as model matrices and formulas.}
#'
#' @details
#' The \code{shrinkTPR} function combines Student-t process regression with shrinkage priors and normalizing flows for efficient
#' and flexible hyperparameter estimation. It supports custom kernels, hierarchical shrinkage priors for mean and covariance structures,
#' and flow-based posterior approximations. The \code{auto_stop} option allows early stopping based on lack of improvement in ELBO.
#'
#' \strong{Custom Kernel Functions}
#'
#' Users can define custom kernel functions for the covariance structure of the Gaussian process by passing them to the \code{kernel_func} argument.
#' A valid kernel function must follow the same structure as the provided \code{kernel_se} (squared exponential kernel). The function should:
#'
#' \enumerate{
#' \item \strong{Accept the following arguments:}
#'   \itemize{
#'     \item \code{thetas}: A \code{torch_tensor} of dimensions \code{n_latent x d}, representing latent length-scale parameters.
#'     \item \code{tau}: A \code{torch_tensor} of length \code{n_latent}, representing latent scaling factors.
#'     \item \code{x}: A \code{torch_tensor} of dimensions \code{N x d}, containing the input data points.
#'     \item \code{x_star}: Either \code{NULL} or a \code{torch_tensor} of dimensions \code{N_new x d}. If \code{NULL}, the kernel is computed for \code{x} against itself.
#'     Otherwise, it computes the kernel between \code{x} and \code{x_star}.
#'   }
#'
#' \item \strong{Return:}
#'   \itemize{
#'     \item If \code{x_star = NULL}, the function must return a \code{torch_tensor} of dimensions \code{n_latent x N x N}, representing pairwise covariances
#'     between all points in \code{x}.
#'     \item If \code{x_star} is provided, the function must return a \code{torch_tensor} of dimensions \code{n_latent x N_new x N},
#'     representing pairwise covariances between \code{x_star} and \code{x}.
#'   }
#'
#' \item \strong{Requirements:}
#'   \itemize{
#'     \item The kernel must compute a valid positive semi-definite covariance matrix.
#'     \item It should use efficient tensor operations from the Torch library (e.g., \code{torch_bmm}, \code{torch_sum}) to ensure compatibility with GPUs or CPUs.
#'   }
#' }
#'
#' \strong{Testing a Custom Kernel Function}
#'
#' To test a custom kernel function:
#' \enumerate{
#' \item \strong{Verify Dimensions:}
#'   \itemize{
#'     \item When \code{x_star = NULL}, ensure the output is \code{n_latent x N x N}.
#'     \item When \code{x_star} is provided, ensure the output is \code{n_latent x N_new x N}.
#'   }
#' \item \strong{Check Positive Semi-Definiteness:}
#'   Validate that the kernel produces a positive semi-definite covariance matrix for valid inputs.
#' \item \strong{Integrate:}
#'   Use the custom kernel with \code{shrinkTPR} to confirm its compatibility.
#' }
#'
#' Examples of kernel functions can be found in the \code{kernel_funcs.R} file in the package source code,
#' which are documented in the \code{\link{kernel_functions}} help file.
#'
#' \strong{Custom Flow Functions}
#'
#' Users can define custom flow functions for use in Gaussian process regression models by following the structure
#' and conventions of the provided \code{sylvester} function. A valid flow function should be implemented as a
#' \code{nn_module} in \code{torch} and must meet the following requirements:
#'
#' \strong{Structure of a Custom Flow Function}
#'
#' \enumerate{
#' \item \strong{Initialization (\code{initialize})}:
#'   \itemize{
#'     \item Include all required parameters as \code{nn_parameter} or \code{nn_buffer}, and initialize them appropriately.
#'     \item Parameters may include matrices for transformations (e.g., triangular matrices), biases, or other learnable components.
#'   }
#'
#' \item \strong{Forward Pass (\code{forward})}:
#'   \itemize{
#'     \item The \code{forward} method should accept an input tensor \code{z} of dimensions \code{n_latent x D}.
#'     \item The method must:
#'       \itemize{
#'         \item Compute the transformed tensor \code{z}.
#'         \item Compute the log determinant of the Jacobian (\code{log|det J|}).
#'       }
#'     \item The method should return a list containing:
#'       \itemize{
#'         \item \code{zk}: The transformed samples after applying the flow (\code{n_latent x D}).
#'         \item \code{log_diag_j}: A tensor of size \code{n_latent} containing the log determinant of the Jacobian for each sample.
#'       }
#'   }
#'
#' \item \strong{Output Dimensions}:
#'   \itemize{
#'     \item Input tensor \code{z}: \code{n_latent x D}.
#'     \item Outputs:
#'       \itemize{
#'         \item \code{zk}: \code{n_latent x D}.
#'         \item \code{log_diag_j}: \code{n_latent}.
#'       }
#'   }
#'}
#' An example of a flow function can be found in the \code{sylvester.R} file in the package source code,
#' which is documented in the \code{\link{sylvester}} help file.
#'
#' @examples
#' \donttest{
#' if (torch::torch_is_installed()) {
#'   # Simulate data
#'   set.seed(123)
#'   torch::torch_manual_seed(123)
#'   n <- 100
#'   x <- matrix(runif(n * 2), n, 2)
#'   y <- sin(2 * pi * x[, 1]) + rnorm(n, sd = 0.1)
#'   data <- data.frame(y = y, x1 = x[, 1], x2 = x[, 2])
#'
#'   # Fit TPR model
#'   res <- shrinkTPR(y ~ x1 + x2, data = data)
#'
#'   # Check convergence
#'   plot(res$loss_stor, type = "l", main = "Loss Over Iterations")
#'
#'   # Check posterior
#'   samps <- gen_posterior_samples(res, nsamp = 1000)
#'   boxplot(samps$thetas) # Second theta is pulled towards zero
#'
#'   # Predict
#'   x1_new <- seq(from = 0, to = 1, length.out = 100)
#'   x2_new <- runif(100)
#'   y_new <- predict(res, newdata = data.frame(x1 = x1_new, x2 = x2_new), nsamp = 2000)
#'
#'   # Plot
#'   quants <- apply(y_new, 2, quantile, c(0.025, 0.5, 0.975))
#'   plot(x1_new, quants[2, ], type = "l", ylim = c(-1.5, 1.5),
#'         xlab = "x1", ylab = "y", lwd = 2)
#'   polygon(c(x1_new, rev(x1_new)), c(quants[1, ], rev(quants[3, ])),
#'         col = adjustcolor("skyblue", alpha.f = 0.5), border = NA)
#'   points(x[,1], y)
#'   curve(sin(2 * pi * x), add = TRUE, col = "forestgreen", lwd = 2, lty = 2)
#'
#'
#'
#'   # Add mean equation
#'   res2 <- shrinkTPR(y ~ x1 + x2, formula_mean = ~ x1, data = data)
#'   }
#' }
#' @references
#' Shah, A., Wilson, A., & Ghahramani, Z. (2014, April). Student-t processes as alternatives to Gaussian processes. In Artificial intelligence and statistics (pp. 877-885). PMLR.
#' @export
#' @author Peter Knaus \email{peter.knaus@@wu.ac.at}
shrinkTPR <- function(formula,
                      data,
                      a = 0.5,
                      c = 0.5,
                      formula_mean,
                      a_mean = 0.5,
                      c_mean = 0.5,
                      sigma2_rate = 10,
                      nu_alpha = 0.5,
                      nu_beta = 2,
                      kernel_func = kernel_se,
                      n_layers = 10,
                      n_latent = 10,
                      flow_func = sylvester,
                      flow_args,
                      n_epochs = 1000,
                      auto_stop = TRUE,
                      cont_model,
                      device,
                      display_progress = TRUE,
                      optim_control) {

  # Input checking ----------------------------------------------------------

  # Check if formula is valid
  if (!inherits(formula, "formula")) {
    stop("The argument 'formula' must be of class 'formula'.")
  }

  # Check if data is provided and is a data frame
  if (!missing(data) && !is.data.frame(data)) {
    stop("The argument 'data' must be a data frame.")
  }

  # Check that numeric inputs are positive scalars
  to_check_numeric <- list(
    a = a,
    c = c,
    a_mean = a_mean,
    c_mean = c_mean,
    nu_alpha = nu_alpha,
    nu_beta = nu_beta,
    sigma2_rate = sigma2_rate
  )

  bad_numeric <- sapply(to_check_numeric, numeric_input_bad)
  if (any(bad_numeric)) {
    bad_names <- names(to_check_numeric)[bad_numeric]
    stop(paste0(paste(bad_names, collapse = ", "),
                ifelse(length(bad_names) == 1, " must", " must all"),
                " be positive numeric scalars."))
  }

  # Check that integer inputs are positive integers
  to_check_int <- list(
    n_layers = n_layers,
    n_latent = n_latent,
    n_epochs = n_epochs
  )

  bad_int <- sapply(to_check_int, int_input_bad)
  if (any(bad_int)) {
    bad_names <- names(to_check_int)[bad_int]
    stop(paste0(paste(bad_names, collapse = ", "),
                ifelse(length(bad_names) == 1, " must", " must all"),
                " be positive integers."))
  }

  # Check flow function and arguments
  if (!is.function(flow_func)) {
    stop("The argument 'flow_func' must be a valid function.")
  }

  if (!missing(flow_args) && !is.list(flow_args)) {
    stop("The argument 'flow_args', if provided, must be a named list.")
  }

  # Check kernel function
  if (!is.function(kernel_func)) {
    stop("The argument 'kernel_func' must be a valid function.")
  }

  # Check auto_stop is logical
  if (!is.logical(auto_stop) || length(auto_stop) != 1) {
    stop("The argument 'auto_stop' must be a single logical value.")
  }

  # Check display_progress is logical
  if (!is.logical(display_progress) || length(display_progress) != 1) {
    stop("The argument 'display_progress' must be a single logical value.")
  }

  # Check continuation model (if provided)
  if (!missing(cont_model) && !is.list(cont_model)) {
    stop("The argument 'cont_model', if provided, must be a list returned by a previous 'shrinkTPR' call.")
  }

  # Check device
  if (!missing(device) && !inherits(device, "torch_device")) {
    stop("The argument 'device', if provided, must be a valid 'torch_device' object.")
  }

  # Check optimizer control parameters
  if (!missing(optim_control) && !is.list(optim_control)) {
    stop("The argument 'optim_control', if provided, must be a named list.")
  }

  # Check mean formula
  if (!missing(formula_mean) && !inherits(formula_mean, "formula")) {
    stop("The argument 'formula_mean', if provided, must be of class 'formula'.")
  }

  if (!missing(device)) {
    if (!inherits(device, "torch_device")) {
      stop("The argument 'device', if provided, must be a valid 'torch_device' object.")
    }
  }

  if (!missing(cont_model)) {
    if (!inherits(cont_model, "shrinkTPR")) {
      stop("The argument 'cont_model', if provided, must be a list returned by a previous 'shrinkTPR' call.")
    }
  }

  # Add default device if not provided -------------------------------------
  if (missing(device)) {
    if (cuda_is_available()) {
      device <- torch_device("cuda")
    } else {
      device <- torch_device("cpu")
    }
  }

  # Formula interface -------------------------------------------------------
  # For main covar equation
  mf <- match.call(expand.dots = FALSE)
  m <- match(x = c("formula", "data"), table = names(mf), nomatch = 0L)
  mf <- mf[c(1L, m)]
  mf$drop.unused.levels <- TRUE
  mf$na.action <- na.pass
  mf[[1L]] <- quote(model.frame)
  mf <- eval(expr = mf, envir = parent.frame())

  # Create Vector y
  y <- model.response(mf, "numeric")

  # Modify the formula to exclude intercept
  mt <- attr(x = mf, which = "terms")
  attr(mt, "intercept") <- 0

  # Create Matrix X with dummies and transformations
  x <- model.matrix(object = mt, data = mf)

  # Check that there are no NAs in y and x
  if (any(is.na(y))) {
    stop("No NA values are allowed in response variable")
  }

  if (any(is.na(x))){
    stop("No NA values are allowed in covariates")
  }

  # For mean equation
  if (!missing(formula_mean)) {
    mf_mean <- model.frame(formula = formula_mean, data = data, drop.unused.levels = TRUE, na.action = na.pass)
    mt_mean <- attr(x = mf_mean, which = "terms")

    # Create Matrix X with dummies and transformations
    x_mean <- model.matrix(object = mt_mean, data = mf_mean)

    # Check that there are no NAs in x_mean
    if (any(is.na(x_mean))) {
      stop("No NA values are allowed in covariates for the mean equation")
    }

    colnames(x_mean)[colnames(x_mean) == "(Intercept)"] <- "Intercept"
    x_mean_colnames <- colnames(x_mean)
  } else {
    x_mean <- NULL
  }


  if (missing(cont_model)) {

    # Print initializing parameters message
    if (display_progress) {
      message("Initializing parameters...", appendLF = FALSE)
    }

    # Merge user and default flow_args
    if (missing(flow_args)) flow_args <- list()
    flow_args_merged <- list_merger(formals(flow_func), flow_args)

    # d is always handled internally
    flow_args_merged$d <- NULL

    # Create y, x and x_mean tensors
    y <- torch_tensor(y, device = device)
    x <- torch_tensor(x, device = device)

    if (!is.null(x_mean)) {
      x_mean <- torch_tensor(x_mean, device = device)
    }

    model <- TPR_class(y, x, x_mean, a = a, c = c, a_mean = a_mean, c_mean = c_mean,
                       sigma2_rate = sigma2_rate, nu_alpha = nu_alpha, nu_beta = nu_beta,
                       n_layers, flow_func, flow_args_merged, kernel_func = kernel_se, device)

    # Merge user and default optim_control
    if (missing(optim_control)) optim_control <- list()
    default_optim_params <- formals(optim_adam)
    default_optim_params$lr <- 1e-3
    default_optim_params$weight_decay <- 1e-3
    default_optim_params$params <- model$parameters
    optim_control_merged <- list_merger(default_optim_params, optim_control)

    optimizer <- do.call(optim_adam, optim_control_merged)

    if (display_progress) {
      message("Done!")
    }

  } else {
    model <- cont_model$last_model
    optimizer <- cont_model$optimizer
    best_model <- cont_model$best_model
    best_loss <- cont_model$best_loss
  }

  # Create progress bar if display_progress is TRUE
  if (display_progress) {
    pb <- progress_bar$new(total = n_epochs, format = "[:bar] :percent :eta | :message",
                           clear = FALSE, width = 100)
  }

  # Create vector to store ELBO
  loss_stor <- rep(NA_real_, n_epochs)


  # Number of iterations to check for significant improvement
  n_check <- 100

  # Initialize a variable to track whether the loop exited normally or due to interruption
  stop_reason <- "max_iterations"
  runtime <- system.time({
    tryCatch({
      for (i in 1:n_epochs) {

        # Sample from base distribution
        z <- model$gen_batch(n_latent)

        # Forward pass through model
        zk_log_det_J <- model(z)
        zk_pos <- zk_log_det_J$zk
        log_det_J <- zk_log_det_J$log_det_J

        # Calculate loss, i.e. ELBO
        # suppressWarnings because torchscript does not yet support torch.linalg.cholesky
        loss <- suppressMessages(-model$elbo(zk_pos, log_det_J))
        loss_stor[i] <- loss$item()

        # Zero gradients
        optimizer$zero_grad()

        # Compute gradients, i.e. backprop
        loss$backward(retain_graph = FALSE)

        # Update parameters
        optimizer$step()

        # Check if model is best
        if (i == 1) {
          best_model <- model
          best_loss <- loss$item()
        } else if (loss$item() < best_loss & !is.na(loss$item()) & !is.infinite(loss$item())) {
          best_model <- model
          best_loss <- loss$item()
        }


        # Auto stop if no improvement in n_check iterations
        if (auto_stop &
            i %% n_check == 0 &
            i > (n_check - 1)) {
          X <- 1:n_check
          Y <- loss_stor[(i - n_check + 1):i]
          p_val <- lightweight_ols(Y, X)

          # Slightly more lenient here, false positives are not as bad as false negatives
          if (p_val > 0.2) {
            stop_reason <- "auto_stop"
            break
          }
        }

        # Update progress bar
        if (display_progress) {

          # Prepare message, this way width can be set
          avg_loss_msg <- "Avg. loss last 50 iter.: "
          avg_loss_width <- 7


          # If less than 50 iterations, don't show avg loss
          if (i >= 50) {

            # Recalculate average loss every 10 iterations
            if (i %% 10 == 0) {
              avg_loss <- mean(loss_stor[(i - 49):i])
            }

            curr_message <- paste0(avg_loss_msg,
                                   sprintf(paste0("%-", avg_loss_width, ".2f"), avg_loss))
          } else {
            curr_message <- format("", width = nchar(avg_loss_msg) + avg_loss_width)
          }
          pb$tick(tokens = list(message = curr_message))
        }
      }
    }, interrupt = function(ex) {
      stop_reason <<- "interrupted"
      if (display_progress) {
        pb$terminate()
      }
      message("\nTraining interrupted at iteration ", i, ". Returning model trained so far.")
    }, error = function(ex) {
      stop_reason <<- "error"
      if (display_progress) {
        pb$terminate()
      }
      message("\nError occurred at iteration ", i, ". Returning model trained so far.")
    })
  })


  # Print messages based on how the loop ended
  if (display_progress) {

    if (stop_reason %in% c("auto_stop")) {
      pb$terminate()
    }

    message(paste0("Timing (elapsed): ", round(runtime["elapsed"], 2), " seconds."))
    message(paste0(round( i/ runtime[3]), " iterations per second."))

    if (stop_reason == "auto_stop" & i < n_epochs) {
      message("Auto stop triggered, iteration ", i)
    } else if (stop_reason == "max_iterations") {
      message("Max iterations reached, stopping at iteration ", i)
      message("Check if convergence is reached by looking at the loss_stor attribute of the returned object")
    }
  }

  if (missing(cont_model)) {
    model_internals <- list(
      terms = mt,
      xlevels = .getXlevels(mt, mf),
      data = data,
      d_cov = x$shape[2]
    )

    if (!is.null(x_mean)) {
      model_internals$terms_mean <- mt_mean
      model_internals$xlevels_mean <- .getXlevels(mt_mean, mf_mean)
      model_internals$x_mean <- TRUE
      model_internals$d_mean <- x_mean$shape[2]
      model_internals$x_mean_names <- x_mean_colnames
    } else {
      model_internals$x_mean <- FALSE
    }
  } else {
    model_internals <- cont_model$model_internals
  }


  # Return list of results
  res <- list(model = best_model,
              loss = best_loss,
              loss_stor = loss_stor,
              last_model = model,
              optimizer = optimizer,
              model_internals = model_internals)

  attr(res, "class") <- "shrinkTPR"
  attr(res, "device") <- device

  return(res)
}
