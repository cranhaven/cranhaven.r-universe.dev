#' Compute distributional moments from the parameters
#'
#' This function takes (predicted) parameters of a response distribution and
#' calculates the corresponding distributional moments from it. Furthermore, you
#' can specify own functions that calculate measures depending on distributional
#' parameters.
#'
#' With the exception of \link{betareg}, the distributional families behind the
#' estimation of the distributional regression models are represented by own
#' objects, e.g. \link[gamlss.dist]{GA} or \link[bamlss]{lognormal_bamlss}. We
#' worked together with both the authors of \link{gamlss} and \link{bamlss} such
#' that the functions to compute the moments from the parameters of the
#' underlying distribution is already implemented in the family functon itself.
#' As an example, try out \code{gamlss.dist::BE()$mean}, which shows one
#' example. The function \code{moments()} utilizes this fact and ensures that
#' the outcome is always in the right format: Two columns named `Expected_Value`
#' and `Variance` detailing the first two moments. One exception appears when an
#' external function is specified, at which point there are three columns.
#'
#' Each row details one `scenario` meaning one covariate combination for which
#' to predict the moments. \code{moments()} is heavily used in
#' \link{plot_moments}, where moments are calculated over the entire range of
#' one variable.
#'
#' If target distribution stems from a \link{bamlss} model, \code{moments()} can
#' also utilize the samples from the \link{preds} function to transform them.
#' This is important for correct estimates, as just taking the mean of the
#' samples and then using those means to estimate the moments can lead to
#' inaccurate results. \code{moments()} knows when samples of predicted
#' parameters were specified in the \code{par} argument, and then transforms the
#' samples to the moments, before taking averages. Only through this procedure
#' we even get credible intervals for the expected moments (see "upperlimit" and
#' "lowerlimit" as possible outcomes of argument \code{what}).
#'
#' @importFrom stats pnorm dnorm quantile
#' @param par Parameters of the modeled distribution in a data.frame form. Can
#'   be Output of \code{\link{preds}}, for example.
#' @param fam_name Name of the used family in character form. Can be one of
#'   \code{distreg.vis::dists$dist_name}. All gamlss.dist and exported bamlss
#'   families are supported. To obtain the family from a model in character
#'   form, use \code{\link{fam_obtainer}}.
#' @param what One of \code{"mean"}, \code{"upperlimit"}, \code{"lowerlimit"}.
#'   If it is \code{mean} (which is also the default), then the mean of the
#'   parameter samples is calculated. 2.5% and 97.5% quantiles are calculated
#'   for \code{lowerlimit} and \code{upperlimit}, respectively.
#' @param ex_fun An external function \code{function(par) {...}} which
#'   calculates a measure, whose dependency from a certain variable is of
#'   special interest.
#' @examples
#'
#' # Get some artificial data
#' gamma_data <- model_fam_data(fam_name = "gamma", nrow = 100)
#'
#' # Estimate model
#' library("bamlss")
#' model <- bamlss(list(gamma ~ norm2 + binomial1,
#'                      sigma ~ norm2 + binomial1),
#'                      data = gamma_data,
#'                      family = gamma_bamlss())
#'
#' # Get some predicted parameters in sample and without sample form
#' pred_params <- preds(model, vary_by = "binomial1")
#' pred_params_samples <- preds(model, vary_by = "binomial1", what = "samples")
#'
#' # Now calculate moments - with samples more correct estimates come out
#' moments(pred_params, fam_name = "gamma", what = "mean")
#' moments(pred_params_samples, fam_name = "gamma", what = "mean")
#'
#' # Now with specifying an external function
#' my_serious_fun <- function(par) {
#'   return(par[["mu"]] + 3*par[["sigma"]])
#' }
#' moments(pred_params_samples,
#'         what = "mean",
#'         fam_name = "gamma",
#'         ex_fun = "my_serious_fun")
#'
#' @import bamlss
#' @export

moments <- function(par, fam_name, what = "mean", ex_fun = NULL) {

  # If ex_fun is not specified in character form please do so
  if (!is.null(ex_fun) & !is.character(ex_fun))
    stop("Please specify ex_fun in character form.")

  # Stop if neither gamlss nor bamlss
  if (!is.distreg.fam(fam_name))
    stop("This function only works for selected distributional family models.")

  # This checks whether we have samples (list format) or not (data.frame format)
  if (is.list(par) && !is.data.frame(par))
    samples <- TRUE
  else if (is.data.frame(par))
    samples <- FALSE
  else
    stop("par has to be either a data.frame or a list.")

  # What to do if ex_fun is an empty string - this is for easier shiny app handling
  if (!is.null(ex_fun)) {
    if (ex_fun == "" || ex_fun == "NO FUNCTION") {
      ex_fun <- NULL
    }
  }

  if (!samples)
    rnames <- row.names(par)

  # Try out if the external function works
  funworks <- FALSE # Only do the computation if the external function is specified correctly
  if (!is.null(ex_fun)) {
    fun <- get(ex_fun, envir = .GlobalEnv)
    tryCatch({
      if (samples)
        ex_f(as.list(par[[1]][1, ]), unquotedfun = fun) # List of dataframes
      if (!samples)
        ex_f(as.list(par[1, ]), unquotedfun = fun) # dataframe
      funworks <- TRUE
    }, error = function(e) {
      stop("External function not specified correctly!")
    })
  }

  # gamlss moments
  if (is.gamlss(fam_name)) {

    # What is there to calculate?
    if (what != "mean")
      stop("gamlss cannot calculate confidence intervals for params/moments")

    # Call the family
    fam <- get(fam_name, envir = as.environment("package:gamlss.dist"))
    fam_called <- fam()

    if (!funworks) {
      # Get moments for each row of par
      moms_raw <- apply(par, 1, function(x) {
        ex <- do.call(fam_called$mean, args = as.list(x)) # Expected value
        vx <- do.call(fam_called$variance, args = as.list(x)) # Variance
        return(c(Expected_Value = ex, Variance = vx))
      })
    }

    if (funworks) {
      # Get moments for each row of par
      moms_raw <- apply(par, 1, function(x) {
        ex <- do.call(fam_called$mean, args = as.list(x)) # Expected value, use do.call because gamlss doesnt have par as parameter but only the named parameters...
        vx <- do.call(fam_called$variance, args = as.list(x)) # Variance
        ex_fun_vals <- do.call(ex_fun, args = list(x)) # External function, where we input it as a list
        return_vec <- c(Expected_Value = ex, Variance = vx, ex_fun = ex_fun_vals)
        names(return_vec)[names(return_vec) == "ex_fun"] <- ex_fun # Give it the name of the variable ex_fun
        return(return_vec)
      })
    }

    # Make into nice format
    moms <- as.data.frame(t(moms_raw), row.names = rnames)
  }

  # bamlss moments
  if (is.bamlss(fam_name)) {

    # This checks whether we can obtain CI's from samples
    if (what != "mean" && !samples)
      stop("lower/Upper CI bounds can only be calculated from samples")

    # Call the family
    fam <- get(paste0(fam_name, "_bamlss"),
               envir = as.environment("package:bamlss"))
    fam_called <- fam()

    if (is.null(fam_called$mean) | is.null(fam_called$variance))
      stop("Not all moment functions implemented")

    # If we don't want to do it with transforming samples
    if (what == "mean" && !samples) {

      # If we don't have external_function
      if (!funworks) {
        # Get moments for each row of par
        moms_raw <- apply(par, 1, function(x) {
          ex <- fam_called$mean(as.list(x)) # Expected value
          vx <- fam_called$variance(as.list(x)) # Variance
          return(c(Expected_Value = ex, Variance = vx))
        })
      }

      # If we have external function
      if (funworks) {

        # Get moments for each row of par
        moms_raw <- apply(par, 1, function(x) {
          ex <- fam_called$mean(as.list(x)) # Expected value
          vx <- fam_called$variance(as.list(x)) # Variance
          ex_fun_vals <- fun(as.list(x))
          return_vec <- c(Expected_Value = ex, Variance = vx, ex_fun = ex_fun_vals)
          names(return_vec)[names(return_vec) == "ex_fun"] <- ex_fun
          return(return_vec)
        })
      }

      # Make into nice format
      moms <- as.data.frame(t(moms_raw), row.names = rnames) # lots of reshaping here... no me gusta
    }

    if (samples) {

      # If we have no external function
      if (!funworks) {
        # Get moments for each sample and each prediction
        moms_raw <- lapply(par, function(listparts) {
          apply(listparts, 1, FUN = function(x) {
            ex <- fam_called$mean(as.list(x)) # Expected value
            vx <- fam_called$variance(as.list(x)) # Variance
            return(c(Expected_value = ex, Variance = vx))
          })
        })

        # Reshaping necessary
        moms_raw <- lapply(moms_raw, FUN = function(x) {
          x <- t(x)
          colnames(x) <- c("Expected_Value", "Variance")
          return(x)
        })
      }

      # If we have external function
      if (funworks) {

        # Get moments for each sample and each prediction
        moms_raw <- lapply(par, function(listparts) {
          apply(listparts, 1, FUN = function(x) {
            ex <- fam_called$mean(as.list(x)) # Expected value
            vx <- fam_called$variance(as.list(x)) # Variance
            ex_fun_vals <- fun(as.list(x))
            return_vec <- c(Expected_Value = ex, Variance = vx, ex_fun = ex_fun_vals)
            names(return_vec)[names(return_vec) == "ex_fun"] <- ex_fun
            return(return_vec)
          })
        })

        # Reshaping necessary
        moms_raw <- lapply(moms_raw, FUN = function(x) {
          x <- t(x)
          colnames(x) <- c("Expected_Value", "Variance", ex_fun)
          return(x)
        })
      }

      # Mean
      if (what == "mean") {
        comp_res <- lapply(moms_raw, FUN = function(listparts) {
          apply(listparts, 2, FUN = mean)
        })
        moms <- do.call("rbind", args = comp_res)
      }

      # Lower Limit
      if (what == "lowerlimit") {
        comp_res <- lapply(moms_raw, FUN = function(listparts) {
          apply(listparts, 2, FUN = quantile, probs = 0.025, na.rm = TRUE)
        })
        moms <- do.call("rbind", args = comp_res)
      }

      # Upper Limit
      if (what == "upperlimit") {
        comp_res <- lapply(moms_raw, FUN = function(listparts) {
          apply(listparts, 2, FUN = quantile, probs = 0.975, na.rm = TRUE)
        })
        moms <- do.call("rbind", args = comp_res)
      }
    }
  }

  # betareg moments
  if (is.betareg(fam_name)) {

    # Error catcher
    if (what != "mean")
      stop("In betareg models only option for argument 'what' is 'mean'.")

    # If we didnt specify external function
    if (!funworks) {
      moms_raw <- apply(par, 1, FUN = function(x) {
        ex <- x[["mu"]]
        vx <- x[["mu"]] * (1 - x[["mu"]]) / (1 + x[["phi"]])
        return_vec <- c(Expected_Value = ex, Variance = vx)
        return(return_vec)
      })
    }

    # If we did specify external function
    if (funworks) {
      moms_raw <- apply(par, 1, FUN = function(x) {
        ex <- x[["mu"]]
        vx <- x[["mu"]] * (1 - x[["mu"]]) / (1 + x[["phi"]])
        ex_fun_vals <- fun(as.list(x))
        return_vec <- c(Expected_Value = ex,
                        Variance = vx,
                        ex_fun = ex_fun_vals)
        names(return_vec)[names(return_vec) == "ex_fun"] <- ex_fun
      })
    }

    # Make into nice format
    moms <- as.data.frame(t(moms_raw), row.names = rnames) # lots of reshaping here... no me gusta
  }

  if (exists("moms"))
    return(as.data.frame(moms))
  else
    stop("Something went wrong")
}

