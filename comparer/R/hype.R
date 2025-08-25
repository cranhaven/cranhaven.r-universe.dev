# hype ----
#' Hyperparameter optimization
#'
#' @export
# @field X Data frame of inputs that have been evaluated or will be evaluated
# next.
# @field Z Output at X
# @field runtime The time it took to evaluate each row of X
# @field mod Gaussian process model used to predict what the output will be.
# @field parnames Names of the parameters
# @field parlowerraw Lower bounds for each parameter on raw scale
# @field parupperraw Upper bounds for each parameter on raw scale
# @field parlowertrans Lower bounds for each parameter on transformed scale
# @field paruppertrans Upper bounds for each parameter on transformed scale
# @field parlist List of all parameters
# @field partrans Transformation for each parameter
# @field modlist A list with details about the model. The user shouldn't
# ever edit this directly.
# @field ffexp An ffexp R6 object used to run the experiment and store
# the results.
#' @param eval_func The function we evaluate.
#' @param ... Pass in hyperparameters, such as par_unif()
#' as unnamed arguments.
#' @param X0 A data frame of initial points to include. They must
#' have the same names as the hyperparameters. If Z0 is also passed,
#' it should match the points in X0. If Z0 is not passed,
#' then X0 will be the first points evaluated.
#' @param Z0 A vector whose values are the result of applying `eval_func`
#' to each row of X0.
#' @param n_lhs The number of random points to start with. They are
#' selected using a Latin hypercube sample.
#' @param extract_output_func A function that takes in the output from
#' `eval_func` and returns the value we are trying to minimize.
# @field par_all_cts Are all the parameters continuous?
#' @param verbose How much should be printed? 0 is none, 1 is standard,
#' 2 is more, 5+ is a lot
#' @param model What kind of model to use.
#' @param covtype The covariance function to use for the Gaussian
#' process model.
#' @param nugget.estim Whether a nugget should be estimated when
#' fitting the Gaussian process model.
#'
#' @importFrom GauPro gpkm
#'
#' @examples
#'
#' # Have df output, but only use one value from it
#' h1 <- hype(
#'   eval_func = function(a, b) {data.frame(c=a^2+b^2, d=1:2)},
#'   extract_output_func = function(odf) {odf$c[1]},
#'   a = par_unif('a', -1, 2),
#'   b = par_unif('b', -10, 10),
#'   n_lhs = 10
#' )
#' h1$run_all()
#' h1$add_EI(n = 1)
#' h1$run_all()
#' #system.time(h1$run_EI_for_time(sec=3, batch_size = 1))
#' #system.time(h1$run_EI_for_time(sec=3, batch_size = 3))
#' h1$plotorder()
#' h1$plotX()
hype <- function(eval_func,
                 ..., # ... is params
                 X0=NULL, Z0=NULL,
                 n_lhs,
                 extract_output_func,
                 verbose=1,
                 model="GauPro",
                 covtype="matern5_2",
                 nugget.estim=TRUE
) {
  R6_hype$new(
    eval_func=eval_func,
    ...,
    X0=X0, Z0=Z0,
    n_lhs=n_lhs,
    extract_output_func=extract_output_func,
    verbose=verbose,
    model=model,
    covtype=covtype,
    nugget.estim=nugget.estim
  )
}

#' Hyperparameter optimization
#'
#' @export
#' @field X Data frame of inputs that have been evaluated or will be evaluated
#' next.
#' @field Z Output at X
#' @field runtime The time it took to evaluate each row of X
#' @field mod Gaussian process model used to predict what the output will be.
#' @field parnames Names of the parameters
#' @field parlowerraw Lower bounds for each parameter on raw scale
#' @field parupperraw Upper bounds for each parameter on raw scale
#' @field parlowertrans Lower bounds for each parameter on transformed scale
#' @field paruppertrans Upper bounds for each parameter on transformed scale
#' @field parlist List of all parameters
# @field partrans Transformation for each parameter
#' @field modlist A list with details about the model. The user shouldn't
#' ever edit this directly.
#' @field ffexp An ffexp R6 object used to run the experiment and store
#' the results.
#' @field eval_func The function we evaluate.
#' @field extract_output_func A function that takes in the output from
#' `eval_func` and returns the value we are trying to minimize.
#' @field par_all_cts Are all the parameters continuous?
#' @field verbose How much should be printed? 0 is none, 1 is standard,
#' 2 is more, 5+ is a lot
#'
#' @importFrom mixopt mixopt_coorddesc
#'
#' @examples
#'
#' # Have df output, but only use one value from it
#' h1 <- hype(
#'   eval_func = function(a, b) {data.frame(c=a^2+b^2, d=1:2)},
#'   extract_output_func = function(odf) {odf$c[1]},
#'   a = par_unif('a', -1, 2),
#'   b = par_unif('b', -10, 10),
#'   n_lhs = 10
#' )
#' h1$run_all()
#' h1$add_EI(n = 1)
#' h1$run_all()
#' #system.time(h1$run_EI_for_time(sec=3, batch_size = 1))
#' #system.time(h1$run_EI_for_time(sec=3, batch_size = 3))
#' h1$plotorder()
#' h1$plotX()
R6_hype <- R6::R6Class(
  # R6_hype ----
  classname="hype",
  # inherit=ffexp,
  # active=list(
  #   X = function(value) {
  #     if (!missing(value)) {
  #       stop("You can't set X in a hype object")
  #     }
  #     self$ffexp$rungrid2()
  #   }
  # ),
  public=list(
    X=NULL,
    Z=NULL,
    runtime=NULL,
    # mod=NULL,
    modlist=NULL,
    # params=NULL,
    parnames=NULL,
    parlowerraw=NULL,
    parupperraw=NULL,
    parlowertrans=NULL,
    paruppertrans=NULL,
    par_all_cts=NULL,
    # partrans=NULL,
    parlist=NULL,
    ffexp = NULL,
    eval_func = NULL,
    verbose=NULL,
    extract_output_func = NULL,
    #' @description Create hype R6 object.
    #' @param eval_func The function used to evaluate new points.
    #' @param ... Hyperparameters to optimize over.
    #' @param X0 Data frame of initial points to run, or points already
    #' evaluated. If already evaluated, give in outputs in "Z0"
    #' @param Z0 Evaluated outputs at "X0".
    #' @param n_lhs The number that should initially be run using
    #' a maximin Latin hypercube.
    #' @param extract_output_func A function that takes in the output from
    #' `eval_func` and returns the value we are trying to minimize.
    #' @param verbose How much should be printed? 0 is none, 1 is standard,
    #' 2 is more, 5+ is a lot
    #' @param model What package to fit the Gaussian process model with.
    #' Either "GauPro" or "DiceKriging"/"DK".
    #' @param covtype Covariance/correlation/kernel function for the GP model.
    #' @param nugget.estim Should the nugget be estimated when fitting
    #' the GP model?
    initialize = function(eval_func,
                          ..., # ... is params
                          X0=NULL, Z0=NULL,
                          n_lhs,
                          extract_output_func,
                          verbose=1,
                          model="GauPro",
                          covtype="matern5_2",
                          nugget.estim=TRUE
    ) {
      self$eval_func <- eval_func
      if (!missing(extract_output_func)) {
        self$extract_output_func <- extract_output_func
      }
      dots <- list(...)
      if (length(dots) == 0) {
        stop("No hyperparameters given. Give a par_unif to hype$new.")
      }
      stopifnot(length(verbose) == 1, is.numeric(verbose))
      self$verbose <- verbose

      parlist <- list()
      self$parnames <- c()
      # self$parlowerraw <- c()
      # self$parupperraw <- c()
      self$parlowertrans <- c()
      self$paruppertrans <- c()
      # self$partrans <- c()
      for (pari in dots) {
        if (!("par_hype" %in% class(pari))) {
          stop("All ... should be par_hype objects")
        }
        parlist <- c(parlist, pari)
        self$parnames <- c(self$parnames, pari$name)
        # self$parlowerraw <- c(self$parlowerraw, pari$lower)
        # self$parupperraw <- c(self$parupperraw, pari$upper)
        self$parlowertrans <- c(self$parlowertrans, pari$fromraw(pari$lower))
        self$paruppertrans <- c(self$paruppertrans, pari$fromraw(pari$upper))
        # self$partrans <- c(self$partrans, pari$partrans)
      }
      # stopifnot(length(self$parnames) == c(length(self$parlowerraw),
      #                                      length(self$parupperraw),
      #                                      length(self$parlowertrans),
      #                                      length(self$paruppertrans),
      #                                      length(self$partrans)))
      # List of parameters
      self$parlist <- parlist
      self$par_all_cts <- TRUE
      for (i in 1:length(self$parlist)) {
        if (!any(c("par_unif", "par_log10") %in% class(self$parlist[[i]]))) {
          self$par_all_cts <- FALSE
        }
      }
      # if (self$par_all_cts) {
      #   self$parlowerraw <- sapply(self$parlist, function(p) {par$lower})
      #   self$parupperraw <- sapply(self$parlist, function(p) {par$upper})
      #   self$parlowertrans <- sapply(self$parlist, function(p) {par$fromraw(par$lower)})
      #   self$paruppertrans <- sapply(self$parlist, function(p) {par$fromraw(par$upper)})
      # }

      self$modlist <- list(
        needs_update=TRUE,
        type="NULL",
        mod=NULL,
        userspeclist=list(model='null', covtype='null', nugget.estim='null')
      )
      self$update_mod_userspeclist(model=model,
                                   covtype=covtype,
                                   nugget.estim=nugget.estim)

      if (!is.null(X0)) {
        stopifnot(is.data.frame(X0), nrow(X0) > .5,
                  colnames(X0) == self$parnames)
        X0raw <- X0
      } else {
        X0raw <- NULL
      }
      if (!missing(n_lhs) && !is.null(n_lhs)) {
        stopifnot(length(n_lhs) == 1)
        stopifnot(is.numeric(n_lhs))
        if (n_lhs > .5) {
          # Use add_LHS here?
          # Xlhstrans <- lhs::maximinLHS(n=n_lhs, k=length(self$parnames))
          # Xlhstrans <- sweep(sweep(Xlhstrans,
          #                          2, self$paruppertrans - self$parlowertrans, "*"
          # ), 2, self$parlowertrans, "+")
          # Xlhstrans <- as.data.frame(Xlhstrans)
          # names(Xlhstrans) <- self$parnames
          # X0trans <- rbind(X0trans, Xlhstrans)
          # Just get the X from add_LHS since ffexp not created yet
          Xlhsraw <- self$add_LHS(n_lhs, just_return_df = TRUE)
          X0raw <- rbind(X0raw, Xlhsraw)
        }
      }
      if (is.null(X0raw)) {
        stop(paste('Give in n_lhs, the number of initial points to evaluate.',
                   '(X0 is null.)'))
      }
      if (!is.data.frame(X0raw)) {stop("X0 is not a df?")}
      # Convert transformed back to raw
      # X0raw <- X0trans
      # X0raw <- self$convert_trans_to_raw(X0trans)
      # for (i in 1:ncol(X0trans)) {
      #   X0raw[, i] <- parlist[[i]]$toraw(X0trans[, i])
      # }
      # Use an ffexp object to manage simulations
      self$ffexp <- ffexp$new(eval_func=eval_func,
                              Xdfraw=X0raw
      )
      # If Y0 given in with X0, put it in
      if (!is.null(Z0)) {
        stopifnot(is.numeric(Z0), length(Z0) == nrow(X0))
        for (i in 1:nrow(X0)) {
          self$ffexp$run_one(i,
                             force_this_as_output=Z0[[i]],
                             verbose=0)
        }
        # Run this so it gathers X/Z properly.
        # It won't actually run anything.
        self$run_all()
      }
      invisible(self)
    },
    #' @description Add data to the experiment results.
    #' @param X Data frame with names matching the input parameters
    #' @param Z Output at rows of X matching the experiment output.
    add_data = function(X, Z) {
      newffexp <- self$ffexp$add_level('Xdfraw', X)
      stopifnot(is.data.frame(X), nrow(X) > .5,
                ncol(X) == ncol(self$X),
                colnames(X) == colnames(self$X),
                is.numeric(Z), nrow(X) == length(Z))
      for (i in 1:nrow(X)) {
        newffexp$run_one(self$ffexp$number_runs + i,
                         force_this_as_output = Z[[i]],
                         verbose=0)
      }
      self$ffexp <- newffexp

      # Update this object
      self$X <- self$ffexp$rungrid2()
      self$Z <- unlist(self$ffexp$outlist)
      self$modlist$needs_update <- TRUE

      invisible(self)
    },
    #' @description Add new inputs to run. This allows the user to specify
    #' what they want run next.
    #' @param X Data frame with names matching the input parameters.
    add_X = function(X) {
      stopifnot(is.data.frame(X))
      stopifnot(all(colnames(X) == colnames(self$X)))
      nameoflevel <- "Xdfraw" #if (length(self$parnames) > 1) {"Xdf"} else {self$ffexp$allvars$name[1]}
      updatedffexp <- self$ffexp$add_level(nameoflevel, X, suppressMessage=T)
      self$ffexp <- updatedffexp
      invisible(self)
    },
    #' @description Add new input points using a maximin
    #' Latin hypercube.
    #' Latin hypercubes are usually more spacing than randomly picking points.
    #' @param n Number of points to add.
    #' @param just_return_df Instead of adding to experiment, should
    #' it just return the new set of values?
    add_LHS = function(n, just_return_df=FALSE) {
      #   Xlhstrans <- lhs::maximinLHS(n=n, k=length(self$parnames))
      #   Xlhstrans <- sweep(sweep(Xlhstrans,
      #                            2, self$paruppertrans - self$parlowertrans, "*"
      #   ), 2, self$parlowertrans, "+")
      #   Xlhstrans <- as.data.frame(Xlhstrans)
      #   names(Xlhstrans) <- self$parnames
      #   # Convert trans to raw
      #   Xlhsraw <- self$convert_trans_to_raw(Xlhstrans)
      #   self$add_X(Xlhsraw)
      #   invisible(self)
      # },
      # add_LHS2 = function(n) {
      if (requireNamespace("lhs", quietly = TRUE)) {
        lhs <- lhs::maximinLHS(n=n, k=length(self$parnames))
      } else {
        # message("lhs package not available, using worse option. Please install lhs.")
        # Increasing lhs
        lhs <- (matrix(data=1:n, byrow=F,
                       nrow=n, ncol=length(self$parnames)) - 1 +
                  matrix(data=runif(n*length(self$parnames)),
                         nrow=n, ncol=length(self$parnames))
        ) / n
        # Randomize each column
        for (i in 1:length(self$parnames)) {
          lhs[, i] <- lhs[sample(1:n, n, replace=F), i]
        }

      }
      lst <- rep(list(NULL), length(self$parnames))
      for (i in 1:length(self$parnames)) {
        lst[[i]] <- self$parlist[[i]]$generate(lhs[, i])
      }
      names(lst) <- self$parnames
      Xlhsraw <- as.data.frame(lst)
      # Xlhstrans <- sweep(sweep(Xlhstrans,
      #                          2, self$paruppertrans - self$parlowertrans, "*"
      # ), 2, self$parlowertrans, "+")
      # Xlhstrans <- as.data.frame(Xlhstrans)
      # names(Xlhsraw) <- self$parnames
      # Convert trans to raw
      # Xlhsraw <- self$convert_trans_to_raw(Xlhstrans)
      if (just_return_df) {
        return(Xlhsraw)
      }
      self$add_X(Xlhsraw)
      invisible(self)
    },
    #' @description Convert parameters from transformed scale to raw scale.
    #' @param Xtrans Parameters on the transformed scale
    convert_trans_to_raw = function(Xtrans) {
      # Does it need to be converted back into a vector?
      # convert_back <- FALSE
      if (is.vector(Xtrans)) {
        # convert_back <- TRUE
        # Xtrans <- matrix(Xtrans, nrow=1)
        Xtrans <- as.data.frame(as.list(Xtrans))
        colnames(Xtrans) <- self$parnames
        stopifnot(nrow(Xtrans) == 1)
      } else if (is.matrix(Xtrans)) {
        Xtrans <- as.data.frame(Xtrans)
        colnames(Xtrans) <- self$parnames
      }
      Xraw <- Xtrans
      for (i in 1:ncol(Xtrans)) {
        Xraw[, i] <- self$parlist[[i]]$toraw(Xtrans[, i])
      }
      # if (convert_back) {
      #   Xraw <- Xraw[1, , drop=TRUE]
      # }
      Xraw
    },
    #' @description Convert parameters from raw scale to transformed scale.
    #' @param Xraw Parameters on the raw scale
    convert_raw_to_trans = function(Xraw) {
      convert_back <- FALSE
      if (is.vector(Xraw)) {
        convert_back <- TRUE
        Xraw <- matrix(Xraw, nrow=1)
      }
      Xtrans <- Xraw
      for (i in 1:ncol(Xtrans)) {
        Xtrans[, i] <- self$parlist[[i]]$fromraw(Xraw[, i])
      }
      if (convert_back) {
        Xtrans <- Xtrans[1, , drop=TRUE]
      }
      Xtrans
    },
    #' @description Change lower/upper bounds of a parameter
    #' @param parname Name of the parameter
    #' @param lower New lower bound. Leave empty if not changing.
    #' @param upper New upper bound. Leave empty if not changing.
    change_par_bounds = function(parname, lower, upper) {
      stopifnot(parname %in% self$parnames)
      parind <- which(parname == self$parnames)
      stopifnot(length(parind) == 1)
      if (!missing(lower)) {
        stopifnot(!is.null(lower), !is.na(lower),
                  length(lower) == 1, is.numeric(lower))
        self$parlist[[parind]]$lower <- lower
        self$parlowerraw[[parind]] <- lower
        self$parlowertrans[[parind]] <- self$parlist[[parind]]$fromraw(lower)
      }
      if (!missing(upper)) {
        stopifnot(!is.null(upper), !is.na(upper),
                  length(upper) == 1, is.numeric(upper))
        self$parlist[[parind]]$upper <- upper
        self$parupperraw[[parind]] <- upper
        self$paruppertrans[[parind]] <- self$parlist[[parind]]$fromraw(upper)
      }
      stopifnot(self$parlist[[parind]]$lower < self$parlist[[parind]]$upper)
      invisible(self)
    },
    #' @description Add new inputs to run using the expected information
    #' criteria
    #' @param n Number of points to add.
    #' @param covtype Covariance function to use for the Gaussian process
    #' model.
    #' @param nugget.estim Should a nugget be estimated?
    #' @param model Which package should be used to fit the model and
    #' calculate the EI? Use "DK" for DiceKriging or "GauPro" for GauPro.
    #' @param eps Exploration parameter. The minimum amount of improvement
    #' you care about.
    #' @param just_return Just return the EI info, don't actually add the
    #' points to the design.
    #' @param calculate_at Calculate the EI at a specific point.
    add_EI = function(n, covtype=NULL, nugget.estim=NULL,
                      model=NULL, eps, just_return=FALSE,
                      calculate_at) {
      if (is.null(self$X)) {
        stop('X is null, you need to run_all first.')
      }

      # If unevaluated points, set to lowest value seen so far.
      Xraw <- self$ffexp$rungrid2()
      Xtrans <- self$convert_raw_to_trans(Xraw)
      if (nrow(Xtrans) == length(self$Z)) { # All have been evaluated
        Z <- self$Z
      } else { # Unevaluated points exist, set to constant liar
        Z <- c(self$Z, rep(min(self$Z), nrow(Xtrans) - length(self$Z)))
        message("Unevaluated points already exist, using constant liar")
      }

      # # Just update mod? Set covtype?
      # if (covtype == "random") {
      #   covtype <- sample(c("matern5_2", "matern3_2", "exp", "powexp", "gauss"), 1)
      # }

      # Fit model
      # stopifnot(length(model) == 1, is.character(model))
      # self$fit_mod(model=model,
      #              nugget.estim=nugget.estim,
      #              covtype=covtype
      # )
      # warning('covtype and nugest here')
      self$update_mod_userspeclist(model=model,
                                   nugget.estim=nugget.estim,
                                   covtype=covtype)

      if ("km" %in% class(self$mod)) {
        if (!self$par_all_cts) {
          stop(paste0("Can only add EI with DiceKriging if all",
                      "parameters are continuous (par_unif, par_log10)"))
        }
        if (!missing(eps) && !is.null(eps) && eps>0) {
          warning("eps isn't used in add_EI for DiceKriging model")
        }
        if (!missing(calculate_at) && !is.null(calculate_at)) {
          if (is.matrix(calculate_at) || is.data.frame(calculate_at)) {
            return(apply(calculate_at, 1,
                         function(xrow) DiceOptim::EI(x=xrow, model=self$mod)))
          } else {
            return(DiceOptim::EI(x=calculate_at, model=self$mod))
          }
        }
        if (n==1) {
          # Suppress "Stopped because hard maximum generation limit was hit"
          EIout <- suppressWarnings(DiceOptim::max_EI(
            model=self$mod,
            lower=self$parlowertrans,
            upper=self$paruppertrans,
            control=list(print.level=0)))
        } else {
          # Select multiple points to be evaluated, useful when running in parallel
          # Suppress "Stopped because hard maximum generation limit was hit."
          EIout <- suppressWarnings(DiceOptim::max_qEI(
            model=self$mod,
            npoints=n,
            crit="CL", # exact was very slow for more than a couple
            lower=self$parlowertrans,
            upper=self$paruppertrans))
        }
      } else if ("GauPro" %in% class(self$mod)) {
        if (missing(eps)) {eps <- 0}
        stopifnot(length(eps)==1, eps>=0)

        # Calculate EI at specific point
        if (!missing(calculate_at) && !is.null(calculate_at)) {
          if (is.matrix(calculate_at) || is.data.frame(calculate_at)) {
            return(apply(calculate_at, 1,
                         function(xrow) {
                           suppressWarnings({
                             self$mod$EI(x=xrow, minimize = T, eps=eps)
                           })
                         }))
          } else {
            return(suppressWarnings(
              self$mod$EI(x=calculate_at, minimize = T, eps=eps)))
          }
        }
        if (self$par_all_cts) {
          if (n==1) {
            EIout <- self$mod$maxEI(lower=self$parlowertrans,
                                    upper=self$paruppertrans,
                                    minimize=TRUE,
                                    eps=eps)
          } else { # n > 1
            # Select multiple points to be evaluated,
            #   useful when running in parallel
            EIout <- self$mod$maxqEI(npoints=n, method="CL",
                                     lower=self$parlowertrans,
                                     upper=self$paruppertrans,
                                     minimize=TRUE,
                                     eps=eps)
          }
        } else { # Not all cts par, need to use mixopt
          if (n==1) {
            # Convert pars to mixopt mopars
            mopars <- lapply(self$parlist,
                             function(p) {
                               p$convert_to_mopar(raw_scale = FALSE)
                             })
            EIout <- suppressWarnings({
              self$mod$maxEI(
                lower=NULL,
                upper=NULL,
                minimize=TRUE,
                eps=eps,
                mopar=mopars)
            })
          } else { # n > 1
            # Convert pars to mixopt mopars
            mopars <- lapply(self$parlist,
                             function(p) {
                               p$convert_to_mopar(raw_scale = FALSE)
                             })
            EIout <- suppressWarnings({
              self$mod$maxqEI(
                npoints=n,
                lower=NULL,
                upper=NULL,
                minimize=TRUE,
                eps=eps,
                mopar=mopars)
            })
            if (all(EIout$par[1,] == EIout$par[2,])) {
              message("maxqEI picked the same points for the first two")
            }
          }
        }
      } else {
        stop(paste("Model given to add_EI is not valid (", model,
                   "), should be one of: DK"))
      }
      if (just_return) {
        return(EIout)
      }
      newXtrans <- EIout$par
      newXraw <- self$convert_trans_to_raw(newXtrans)
      # Fix any that aren't valid. This happened when optim picked points
      #  just outside the bounds and can cause errors.
      for (i in 1:length(self$parlist)) {
        i_isvalid <- all(self$parlist[[i]]$isvalid(newXraw[, i]))
        if (!i_isvalid) {
          replvals <- self$parlist[[i]]$generate(runif(length(newXraw[, i])))
          warning(paste0("add_EI returned invalid value for index ", i,
                         ". Replacing ", newXraw[, i], " with ", replvals))
          newXraw[, i] <- replvals
        }
      }
      # Add new par to experiment
      nameoflevel <- "Xdfraw" #if (length(self$parnames) > 1) {"Xdf"} else {self$ffexp$allvars$name[1]}
      updatedffexp <- self$ffexp$add_level(nameoflevel, newXraw,
                                           suppressMessage=TRUE)
      self$ffexp <- updatedffexp
      invisible(self)
    },
    # calculate_EI = function() {
    #
    # },
    #' @description Fit model to the data collected so far
    #' @param covtype Covariance function to use for the Gaussian process
    #' model.
    #' @param nugget.estim Should a nugget be estimated?
    #' @param model Which package should be used to fit the model and
    #' calculate the EI? Use "DK" for DiceKriging or "GauPro" for GauPro.
    fit_mod = function(covtype=NULL, nugget.estim=NULL,
                       model=NULL) {
      if (is.null(self$X)) {
        stop('X is null, you need to run_all first.')
      }
      # if (!self$par_all_cts) {
      #   stop("Can only add EI if all parameters are continuous (par_unif, par_log10)")
      # }
      # If unevaluated points, set lowest value.
      Xraw <- self$ffexp$rungrid2()
      Xtrans <- self$convert_raw_to_trans(Xraw)
      if (nrow(Xtrans) == length(self$Z)) { # All have been evaluated
        Z <- self$Z
      } else { # Unevaluated points exist, set to constant liar
        Z <- c(self$Z, rep(min(self$Z), nrow(Xtrans) - length(self$Z)))
        message("Unevaluated points already exist, using constant liar")
      }

      self$update_mod_userspeclist(model=model, covtype=covtype,
                                   nugget.estim=nugget.estim)
      model <- self$modlist$userspeclist$model
      covtype <- self$modlist$userspeclist$covtype
      nugget.estim <- self$modlist$userspeclist$nugget.estim

      # # Just update mod? Set covtype?
      # if (covtype == "random") {
      #   covtype <- sample(c("matern5_2", "matern3_2", "exp", "powexp", "gauss"), 1)
      # }

      # if (!self$par_all_cts) {
      #   model <- "GauPro"
      # }
      stopifnot(length(model) == 1, is.character(model))
      if (tolower(model) %in% c('dk', 'dice', 'dicekriging') &&
          !requireNamespace("DiceKriging")) {
        message("DiceKriging R package not available. Changing to use GauPro.")
        model <- "GauPro"
      }
      if (tolower(model) %in% c('dk', 'dice', 'dicekriging')) {
        if (!self$par_all_cts) {
          stop(paste0("Can only add EI if all parameters are continuous",
                      " (par_unif, par_log10)"))
        }
        self$modlist$mod <- DiceKriging::km(formula = ~1,
                                            covtype=covtype,
                                            design = Xtrans,
                                            response = Z,
                                            nugget.estim=nugget.estim,
                                            control=list(trace=FALSE))
        self$modlist$type <-  "DK"
      } else if (tolower(model) %in% c("gaupro")) {
        if (self$par_all_cts) {
          # All are continuous, so just give name of kernel
          kern <- covtype
        } else {
          # List of kernels, combine later
          kernellist <- list()
          # Numeric indexes first (cts, log, discretenum)
          numinds <- which(sapply(self$parlist, function(par) {
            any(c("par_unif", "par_log10",
                  "par_discretenum",
                  "par_integer") %in% class(par))
          }))
          if (length(numinds) > .5) {
            kern1inner <-  if (covtype=="gauss") {
              GauPro::Gaussian$new(D=length(numinds))
            } else if (covtype == "matern5_2") {
              GauPro::Matern52$new(D=length(numinds))
            } else {stop("bad covtype for GauPro with discrete par")}
            numkern <- GauPro::IgnoreIndsKernel$new(
              k=kern1inner,
              ignoreinds = setdiff(1:length(self$parlist), numinds)
            )
            kernellist <- c(kernellist, numkern)
          }
          # Unordered inds (use LatentFactorKernel)
          unorderedinds <- which(sapply(self$parlist, function(par) {
            any(c("par_unordered") %in% class(par))
          }))
          if (length(unorderedinds) > .5) {
            for (i in 1:length(unorderedinds)) {
              kernellist <- c(
                kernellist,
                GauPro::LatentFactorKernel$new(
                  D=length(self$parlist),
                  nlevels=length(self$parlist[[unorderedinds[[i]]]]$values),
                  xindex=unorderedinds[[i]],
                  latentdim=if (length(self$parlist[[unorderedinds[[i]]]]$values
                  ) < 3.5) {1} else {2}
                )
              )
            }
          }
          # Ordered inds (use OrderedFactorKernel)
          orderedinds <- which(sapply(self$parlist, function(par) {
            any(c("par_ordered") %in% class(par))
          }))
          if (length(orderedinds) > .5) {
            for (i in 1:length(orderedinds)) {
              kernellist <- c(
                kernellist,
                GauPro::OrderedFactorKernel$new(
                  D=length(self$parlist),
                  nlevels=length(self$parlist[[orderedinds[[i]]]]$values),
                  xindex=orderedinds[[i]]
                )
              )
            }
          }

          # Check that all inds showed up in at least one
          allindsuseinkernels <- sort(c(numinds, unorderedinds, orderedinds))
          stopifnot(length(allindsuseinkernels) == length(self$parlist),
                    allindsuseinkernels == 1:length(self$parlist))

          # Combine kernellist into single kernel
          stopifnot(length(kernellist) > .5)
          # kern <- do.call(prod, kernellist)
          if (length(kernellist) < 1.5) {
            kern <- kernellist[[1]]
          } else {
            kern <- kernellist[[1]]
            for (i in 2:length(kernellist)) {
              kern <- kern * kernellist[[i]]
            }
          }


        }
        self$modlist$mod <- GauPro::GauPro_kernel_model$new(
          X=as.matrix(Xtrans),
          Z=Z,
          restarts=0, # Speed it up
          nug.est=nugget.estim,
          kernel=kern
        )
        # self$modlist <- list(model='gaupro')
        self$modlist$type <- "GauPro"
      } else {
        stop(paste("Model given is not valid (", model,
                   "), should be one of: DK"))
      }

      self$modlist$needs_update <- FALSE

      invisible(self)
    },
    #' @description Run all unevaluated input points.
    #' @param ... Passed into `ffexp$run_all`. Can set 'parallel=TRUE'
    #' to evaluate multiple points simultaneously as long as all needed
    #' variables have been passed to 'varlist'
    run_all = function(...) {
      self$ffexp$run_all(...)
      if (is.null(self$extract_output_func)) {
        self$Z <- self$ffexp$outlist
        if (!all(sapply(self$ffexp$outlist, length) == 1)) {
          print(self$ffexp$outlist)
          stop(paste("output from function must either all have length one",
                     " or else you must give extract_output_func"))
        }
        self$Z <- unlist(self$ffexp$outlist)
      } else {
        self$Z <- sapply(self$ffexp$outlist, self$extract_output_func)
      }
      self$X <- self$ffexp$rungrid2()
      self$runtime <- self$ffexp$outcleandf$runtime
      self$modlist$needs_update <- TRUE
      # self$Xtrans <- s
      invisible(self)
    },
    #' @description Add points using the expected information criteria,
    #' evaluate them, and repeat until a specified amount of time has passed.
    #' @param sec Number of seconds to run for. It will go over this time
    #' limit, finish the current iteration, then stop.
    #' @param batch_size Number of points to run at once.
    #' @param covtype Covariance function to use for the Gaussian process
    #' model.
    #' @param nugget.estim Should a nugget be estimated?
    #' @param verbose Verbose parameter to pass to ffexp$
    #' @param model Which package should be used to fit the model and
    #' calculate the EI? Use "DK" for DiceKriging or "GauPro" for GauPro.
    #' @param eps Exploration parameter. The minimum amount of improvement
    #' you care about.
    #' @param ... Passed into `ffexp$run_all`.
    run_EI_for_time = function(sec, batch_size, covtype="matern5_2",
                               nugget.estim=TRUE, verbose=0,
                               model="GauPro", eps=0,
                               ...) {
      pb <- progress::progress_bar$new(
        format=paste0("  Running for time (:spin) [:bar] :elapsed / ",
                      sec, "s"),
        total=sec)
      pb$tick(0)
      start_time <- Sys.time() #proc.time()
      ncompleted <- 0
      minbefore <- min(self$Z)
      timespentinEI <- 0
      # while(proc.time()[3] - start_time[3] < sec) {
      while(as.numeric(Sys.time() - start_time, units='secs') < sec) {
        # Only add EI once all existing are run
        if (sum(!self$ffexp$completed_runs) < .5) {
          EIstarttime <- Sys.time()
          self$add_EI(n=batch_size, covtype=covtype,
                      nugget.estim=nugget.estim,
                      model=model, eps=eps)
          timespentinEI <- timespentinEI + as.numeric(Sys.time() - EIstarttime,
                                                      units='secs')
        }
        # Run it
        self$run_all(verbose=0, ...)
        # Increment
        ncompleted <- ncompleted + batch_size
        pb$update(ratio=min(1, as.numeric(Sys.time() - start_time,
                                          units='secs') / sec))
      }
      pb$terminate()
      message(paste0("Completed ", ncompleted, " new points in ",
                     round(as.numeric(Sys.time() - start_time,
                                      units='secs'), 1),
                     " seconds"," (spent ", round(timespentinEI, 1),
                     " sec in EI)","\n",
                     "Reduced minimum from ", signif(minbefore,5),
                     " to ", signif(min(self$Z),5)))
      invisible(self)
    },
    #' @description Make a plot to summarize the experiment.
    plot = function() {
      self$plotorder()
    },
    #' @description Plot pairs of inputs and output
    pairs = function() {
      # Before changing to transformed coordinates
      # GGally::ggpairs(cbind(self$X, Z=self$Z))
      df <- cbind(self$X, Z=self$Z)
      ggs <- list()
      for (i in 1:ncol(df)) {
        for (j in 1:ncol(df)) {
          si <- colnames(df)[i]
          sj <- colnames(df)[j]
          if (i == j) {
            p <- ggplot2::ggplot(df, ggplot2::aes(.data[[si]])) +
              ggplot2::geom_histogram(bins = 30)
            if (i < ncol(df)) {
              p <- p +
                ggplot2::scale_x_continuous(trans = self$parlist[[i]]$ggtrans)
            }
          } else {
            p <- ggplot2::ggplot(df,
                                 ggplot2::aes(
                                   .data[[si]], .data[[sj]],
                                   color=.data[[colnames(df)[ncol(df)]]])) +
              ggplot2::geom_point() +
              ggplot2::theme(legend.position = "none") +
              ggplot2::scale_color_gradientn(colors=c('green', 'purple'))
            if (i < ncol(df)) {
              p <- p +
                ggplot2::scale_x_continuous(trans = self$parlist[[i]]$ggtrans)
            }
            if (j < ncol(df)) {
              p <- p +
                ggplot2::scale_y_continuous(trans = self$parlist[[j]]$ggtrans)
            }
          }
          if (i > 1) {
            p <- p + ggplot2::ylab(NULL)
          }
          if (i==1 && j==1) {
            p <- p + ggplot2::ylab(colnames(df)[1])
          }
          if (j < ncol(df)) {
            p <- p + ggplot2::xlab(NULL)
          }
          # ggs <- c(ggs, p)
          ggs[[(j-1) * ncol(df) + (i-1) + 1]] <- p
        }
      }
      # ggpubr
      if (requireNamespace("ggpubr", quietly = TRUE)) {
        do.call(ggpubr::ggarrange, ggs) #+ ggplot2::ylab("Outer ylab")
      } else {
        message(paste0("Only showing one plot, please install R package",
                       " ggpubr for this to work correctly."))
        ggs[[1]]
      }
    },
    #' @description Plot the output of the points evaluated in order.
    plotorder = function() {
      ggplot2::ggplot(data.frame(index=1:length(self$Z), Z=self$Z,
                                 col=ifelse(self$Z<=min(self$Z),'red','black')),
                      ggplot2::aes(index, Z, color=col)) +
        ggplot2::geom_point() +
        ggplot2::scale_colour_manual(values = c("black" = "black", "red" = "red")) +
        ggplot2::theme(legend.position = "none")

    },
    #' @description Plot the output as a function of each input.
    #' @param addlines Should prediction mean and 95\% interval be plotted?
    #' @param addEIlines Should expected improvement lines be plotted?
    #' @param covtype Covariance function to use for the Gaussian process
    #' model.
    #' @param nugget.estim Should a nugget be estimated?
    #' @param model Which package should be used to fit the model and
    #' calculate the EI? Use "DK" for DiceKriging or "GauPro" for GauPro.
    plotX = function(addlines=TRUE, addEIlines=TRUE,
                     covtype=NULL, nugget.estim=NULL,
                     model=NULL) {
      if (is.null(self$X) || is.null(self$Z)) {
        stop("Nothing has been evaluated yet. Call $run_all() first.")
      }
      stopifnot(!is.null(self$X), !is.null(self$Z),
                nrow(self$X) == length(self$Z))
      self$update_mod_userspeclist(model=model,
                                   covtype=covtype,
                                   nugget.estim=nugget.estim)

      tdf <- cbind(self$X, Z=self$Z, Rank=order(order(self$Z)))
      Xtrans <- self$convert_raw_to_trans(self$X)
      if (addlines || addEIlines && self$par_all_cts) {
        min_ind <- which.min(self$Z)[1]
        min_Xraw <- self$X[min_ind,,drop=TRUE]
        min_Xtrans <- Xtrans[min_ind,,drop=TRUE]
        # mod <- DiceKriging::km(formula = ~1,
        #                        covtype=covtype,
        #                        design = Xtrans,
        #                        response = self$Z,
        #                        nugget.estim=nugget.estim,
        #                        control=list(trace=FALSE))
        # self$fit_mod(covtype=covtype, nugget.estim=nugget.estim,
        #              model=model)
        # warning("covtype and nugest here")
        if (addlines) {
          preddf <- list()
          npts <- 30
          for (i in 1:ncol(self$X)) {
            # Get sequence of points for par
            parseq <- self$parlist[[i]]$getseq(n=npts)
            # Predict at points that are the same for all other components
            predXtrans <- matrix(rep(unlist(min_Xtrans), length(parseq$trans)),
                                 ncol=ncol(self$X), byrow=T)
            # predZ <- mod
            # Change current component
            # predXtrans[, i] <- seq(self$parlowertrans[i], self$paruppertrans[i],l=npts)
            predXtrans[, i] <- parseq$trans
            predXtransdf <- as.data.frame(predXtrans)
            names(predXtransdf) <- names(self$X)
            if ("km" %in% class(self$mod)) {
              predout <- DiceKriging::predict.km(self$mod, predXtransdf,
                                                 type="SK", light.return = T)
            } else if ("GauPro" %in% class(self$mod)) {
              predout <- suppressWarnings({
                self$mod$pred(as.matrix(predXtransdf), se.fit=TRUE)
              })
              predout$lower95 <- with(predout, mean - 2*se)
              predout$upper95 <- with(predout, mean + 2*se)
            } else {
              stop("Error: 23587asd9723")
            }
            predout$mean
            df_i <- data.frame(valuetrans=predXtrans[, i],
                               valueraw=parseq$raw, #self$parlist[[i]]$toraw((predXtrans[, i])),
                               mean=predout$mean,
                               lower95=predout$lower95, upper95=predout$upper95,
                               index=i, variable=colnames(self$X)[i])
            # preddf <- rbind(preddf, df_i)
            preddf[[i]] <- df_i
          }
        }
        if (addEIlines) {
          EIdf <- list()
          npts <- 30
          for (i in 1:ncol(self$X)) {
            # Get sequence of points for par
            parseq <- self$parlist[[i]]$getseq(n=npts)
            # Predict at points that are the same for all other components
            EIXtrans <- matrix(rep(unlist(min_Xtrans), length(parseq$trans)),
                               ncol=ncol(self$X), byrow=T)
            # EIXtrans[, i] <- seq(self$parlowertrans[i], self$paruppertrans[i],l=npts)
            EIXtrans[, i] <- parseq$trans
            EIXtransdf <- as.data.frame(EIXtrans)
            names(EIXtransdf) <- names(self$X)
            # EIout <- apply(EIXtransdf, 1,
            #                function(xxx) {
            #                  DiceOptim::EI(xxx, self$mod,
            #                                type="SK")
            #                }
            # )
            EIout <- self$add_EI(calculate_at = EIXtransdf)
            df_i <- data.frame(valuetrans=parseq$trans,
                               valueraw=parseq$raw,
                               EI=EIout,
                               index=i, variable=colnames(self$X)[i])
            # EIdf <- rbind(EIdf, df_i)
            EIdf[[i]] <- df_i
          }
          # Scale EI to find on same axes as Z
          # EIdf$EIrescaled <- ((EIdf$EI - min(EIdf$EI)) / (max(EIdf$EI) - min(EIdf$EI))
          # ) * (max(self$Z) - min(self$Z)) + min(self$Z)
          minEI <- min(sapply(EIdf, function(ll) {min(ll$EI)}))
          maxEI <- max(sapply(EIdf, function(ll) {max(ll$EI)}))
          for (i in 1:ncol(self$X)) {
            EIdf[[i]]$EIrescaled <- ((EIdf[[i]]$EI - minEI) / (maxEI - minEI)
            ) * (max(self$Z) - min(self$Z)) + min(self$Z)
          }
        }
      }


      # p <- ggplot2::ggplot(reshape2::melt(tdf, id.vars=c('Z', 'Rank')),
      #                      ggplot2::aes(value, Z, color=Rank))
      # if (addlines) {
      #   p <- p +
      #     ggplot2::geom_line(data=preddf, ggplot2::aes(value,    mean,color=NULL), alpha=.1) +
      #     ggplot2::geom_line(data=preddf, ggplot2::aes(value, lower95,color=NULL), alpha=.1) +
      #     ggplot2::geom_line(data=preddf, ggplot2::aes(value, upper95,color=NULL), alpha=.1)
      # }
      # p <- p + ggplot2::geom_point() +
      #   ggplot2::facet_wrap(. ~ variable, scales='free_x') +
      #   ggplot2::scale_color_gradientn(colors=c('green', 'purple'))
      # p

      # New with transformations on x axis

      ggs <- list()
      for (i in 1:ncol(Xtrans)) {
        dfi <- data.frame(value=self$X[, i], Z=self$Z,
                          Rank=order(order(self$Z)))
        # ggi <- ggplot2::ggplot(reshape2::melt(tdf, id.vars=c('Z', 'Rank')),
        #                        ggplot2::aes(value, Z, color=Rank))
        ggi <- ggplot2::ggplot(dfi,
                               ggplot2::aes(value, Z, color=Rank))
        # Add EI lines
        if (addEIlines) {
          EIdfi <-  EIdf[[i]] #EIdf[EIdf$index==i, ]
          if (is.numeric(EIdfi$valueraw)) {
            ggi <- ggi +
              ggplot2::geom_line(
                data=EIdfi,
                ggplot2::aes(valueraw, EIrescaled, color=NULL),
                color="red",
                alpha=.3
              )
          } else {
            ggi <- ggi +
              ggplot2::geom_point(
                data=EIdfi,
                ggplot2::aes(valueraw, EIrescaled, color=NULL),
                color="red",
                alpha=.3
              )

          }
        }
        # Add prediction lines
        # ggi <- ggi + preddf[preddf$index==i, ]
        if (addlines) {
          preddfi <-  preddf[[i]] #preddf[preddf$index==i, ]
          if (is.numeric(preddfi$valueraw)) {
            ggi <- ggi +
              ggplot2::geom_line(data=preddfi, ggplot2::aes(valueraw,    mean,color=NULL), alpha=.3) +
              ggplot2::geom_line(data=preddfi, ggplot2::aes(valueraw, lower95,color=NULL), alpha=.2) +
              ggplot2::geom_line(data=preddfi, ggplot2::aes(valueraw, upper95,color=NULL), alpha=.2)
          } else {
            ggi <- ggi +
              ggplot2::geom_point(data=preddfi,ggplot2::aes(valueraw,    mean,color=NULL), shape=18, size=3) +
              ggplot2::geom_point(data=preddfi, ggplot2::aes(valueraw, lower95,color=NULL), shape=18, size=3) +
              ggplot2::geom_point(data=preddfi, ggplot2::aes(valueraw, upper95,color=NULL), shape=18, size=3)
          }
        }
        # Add points
        if (any(c("par_unordered") %in% class(self$parlist[[i]]))) {
          ggi <- ggi + ggplot2::geom_jitter(width=.15)
        } else {
          ggi <- ggi + ggplot2::geom_point()
        }
        ggi <- ggi +
          #ggplot2::facet_wrap(. ~ variable, scales='free_x') +
          ggplot2::scale_color_gradientn(colors=c('green', 'purple'))
        if (any(c("par_log10") %in% class(self$parlist[[i]]))) {
          ggi <- ggi +
            ggplot2::scale_x_continuous(trans=self$parlist[[i]]$ggtrans)
        }
        ggi <- ggi + ggplot2::xlab(self$parnames[i])
        if (i > 1) {
          ggi <- ggi + ggplot2::ylab(NULL)
        }
        ggs[[i]] <- ggi
      }
      # ggpubr::ggarrange(ggs[[1]], ggs[[2]], common.legend=T, legend="right")

      ggs$common.legend <- T
      ggs$legend <- "right"
      # Gave a warning for a sampler, doesn't seem important
      if (requireNamespace("ggpubr", quietly = TRUE)) {
        suppressWarnings({
          do.call(ggpubr::ggarrange, ggs) + ggplot2::ylab("Outer ylab")
        })
      } else {
        message(paste0("Only showing one plot, please install R package",
                       " ggpubr for this to work correctly."))
        ggs[[1]]
      }
    },
    #' @description Plot each input in the order they were chosen.
    #' Colored by quality.
    plotXorder = function() {
      if (is.null(self$X) || is.null(self$Z)) {
        stop("Nothing has been evaluated yet. Call $run_all() first.")
      }
      stopifnot(!is.null(self$X), !is.null(self$Z),
                nrow(self$X) == length(self$Z))
      Xtrans <- self$convert_raw_to_trans(self$X)

      ggs <- list()
      # Loop over each input
      for (i in 1:ncol(Xtrans)) {
        dfi <- data.frame(value=self$X[, i], Z=self$Z,
                          Rank=order(order(self$Z)), index=1:length(self$Z))
        ggi <- ggplot2::ggplot(dfi,
                               ggplot2::aes(index, value, color=Z))
        # # Add points
        # ggi <- ggi + ggplot2::geom_point() +
        #   #ggplot2::facet_wrap(. ~ variable, scales='free_x') +
        #   ggplot2::scale_color_gradientn(colors=c('green', 'purple'))
        # ggi <- ggi + ggplot2::scale_y_continuous(trans=self$parlist[[i]]$ggtrans)
        if (any(c("par_unordered") %in% class(self$parlist[[i]]))) {
          ggi <- ggi + ggplot2::geom_point() #jitter(width=.15)
        } else {
          # Add horizontal lines at lower and upper
          ggi <- ggi +
            ggplot2::geom_hline(yintercept=self$parlist[[i]]$lower, alpha=.2) +
            ggplot2::geom_hline(yintercept=self$parlist[[i]]$upper, alpha=.2)
          ggi <- ggi + ggplot2::geom_point()
        }
        ggi <- ggi +
          #ggplot2::facet_wrap(. ~ variable, scales='free_x') +
          ggplot2::scale_color_gradientn(colors=c('green', 'purple'))
        if (any(c("par_log10") %in% class(self$parlist[[i]]))) {
          ggi <- ggi +
            ggplot2::scale_y_continuous(trans=self$parlist[[i]]$ggtrans)
        }
        ggi <- ggi + ggplot2::ylab(self$parnames[i])
        if (i > 1) {
          ggi <- ggi + ggplot2::xlab(NULL)
        }
        ggs[[i]] <- ggi
      }

      ggs$common.legend <- T
      ggs$legend <- "right"
      if (requireNamespace("ggpubr", quietly = TRUE)) {
        do.call(ggpubr::ggarrange, ggs) + ggplot2::ylab("Outer ylab")
      } else {
        message(paste0("Only showing one plot, please install R package",
                       " ggpubr for this to work correctly."))
        ggs[[1]]
      }
    },
    #' @description Plot the 2D plots from inputs to the output.
    #' All other variables are held at their values for the best input.
    #' @param covtype Covariance function to use for the Gaussian process
    #' model.
    #' @param nugget.estim Should a nugget be estimated?
    plotinteractions = function(covtype="matern5_2", nugget.estim=TRUE) {
      if (!requireNamespace('ContourFunctions', quietly = TRUE)) {
        stop(paste0("R package ContourFunctions not available, please",
                    "install it and try again."))
        # return()
      }
      if (is.null(self$X) || is.null(self$Z)) {
        stop(paste0("Nothing has been evaluated yet.",
                    " Call $run_all() first. Use $plotX instead."))
      }
      if (ncol(self$X) == 1) {
        stop("Can't plot interactions with single input.")
      }
      if (!self$par_all_cts) {
        stop("Can't plotinteractions with discrete factors")
      }

      Xtrans <- self$convert_raw_to_trans(self$X)
      if (requireNamespace('DiceKriging', quietly = TRUE)) {
        mod <- DiceKriging::km(formula = ~1,
                               covtype=covtype,
                               design = Xtrans,
                               response = self$Z,
                               nugget.estim=nugget.estim,
                               control=list(trace=FALSE))
        # predict(mod, self$X, type='sk', light.compute=T, se.compute=F)
        min_ind <- which.min(self$Z)[1]
        min_X <- self$X[min_ind,,drop=TRUE]
        min_Xvec <- unlist(min_X)
        Xtrans <- self$convert_raw_to_trans(self$X)
        min_Xtrans <- Xtrans[min_ind,,drop=TRUE]
        min_Xvectrans <- unlist(min_Xtrans)
        predfunc <- function(X) {
          Xdf <- as.data.frame(X)
          colnames(Xdf) <- colnames(self$X)
          pred <- DiceKriging::predict.km(mod, Xdf, type="sk",
                                          light.return = T, se.compute = F)
          pred$mean
        }
      } else { # Use GauPro
        mod <- suppressMessages(
          GauPro::gpkm(kernel=covtype,
                       X = Xtrans,
                       Z = self$Z,
                       nug.est=nugget.estim)
        )
        # predict(mod, self$X, type='sk', light.compute=T, se.compute=F)
        min_ind <- which.min(self$Z)[1]
        min_X <- self$X[min_ind,,drop=TRUE]
        min_Xvec <- unlist(min_X)
        Xtrans <- self$convert_raw_to_trans(self$X)
        min_Xtrans <- Xtrans[min_ind,,drop=TRUE]
        min_Xvectrans <- unlist(min_Xtrans)
        predfunc <- function(X) {
          Xdf <- as.data.frame(X)
          colnames(Xdf) <- colnames(self$X)
          mod$predict(Xdf)
        }
      }
      if (ncol(self$X) < 2.5) {
        ContourFunctions::cf_func(
          predfunc, batchmax = Inf, bar=T,
          xlim = c(self$parlowertrans[1], self$paruppertrans[1]),
          ylim = c(self$parlowertrans[2], self$paruppertrans[2]),
          pts=self$X, gg=TRUE
        ) #+ ggplot2::xlab("xxxx")
      } else {
        ContourFunctions::cf_highdim(predfunc, D=ncol(self$X),
                                     baseline=min_Xvectrans,
                                     batchmax = Inf,
                                     pts=matrix(min_Xvectrans, nrow=1),
                                     #pts=as.matrix(self$X),
                                     var_names = colnames(self$X),
                                     low = self$parlowertrans,
                                     high=self$paruppertrans)
      }
    },
    #' @description Print details of the object.
    #' @param ... not used
    print = function(...) {
      ts <- paste0(
        "hype object:",
        "\n\td = ", if (is.null(self$X)) {
          ncol(self$ffexp$rungrid2())} else {ncol(self$X)},
        "\n\tn = ", if (is.null(self$X)) {
          nrow(self$ffexp$rungrid2())} else {nrow(self$X)},
        if (!all(self$ffexp$completed_runs)) {
          paste0(" (", sum(!self$ffexp$completed_runs)," unevaluated)")
        } else {''},
        "\n\tTo add data: $add_data(X, Y)",
        "\n\tTo add points using EI: $add_EI",
        "\n\tTo access underlying experiment: $ffexp",
        "\n\tTo access model: $mod",
        "\n\tTo access params: $params",
        "\n\tTo plot output values in order: $plotorder",
        "\n\tTo plot output vs each input: $plotX",
        "\n\tTo run unevaluated points: $run_all",
        "\n\tTo access inputs and output: $X and $Z",
        "\n"
      )
      cat(ts)
      invisible(self)
    },
    #' @description Returns the best parameters
    #' evaluated so far.
    best_params = function() {
      out <- list()

      # Best value that was already seen
      out$evaluated <- list()
      out$evaluated$par <- self$X[which.min(self$Z)[1], ]
      out$evaluated$val <- min(self$Z)

      # Best predicted value, probably not seen yet
      # Use mixopt, convert pars to mopars
      # Function should be evaluated on transformed scale
      mopars <- lapply(self$parlist,
                       function(p) {
                         p$convert_to_mopar(raw_scale = FALSE)
                       })
      # moout <- mixopt::mixopt(par=mopars, fn=self$mod)
      fn <- function(x) {
        # print(x)
        v <- unlist(x)
        self$mod$pred(v)
      }
      moout <- mixopt::mixopt_coorddesc(par=mopars, fn=fn)
      # Need to transform par back to raw scale
      moout_par <- lapply(
        1:length(self$parlist),
        function(i) {
          self$parlist[[i]]$toraw(moout$par[[i]])
        }
      )
      moout_par <- as.data.frame(moout_par)
      colnames(moout_par) <- self$parnames
      # Add to output
      out$unevaluated <- list()
      out$unevaluated$par <- moout_par
      out$unevaluated$val <- moout$val

      # Return list
      out
    },
    #' @description Updates the specifications for the GP model.
    #' @param model What package to fit the Gaussian process model with.
    #' Either "GauPro" or "DiceKriging"/"DK".
    #' @param covtype Covariance/correlation/kernel function for the GP model.
    #' @param nugget.estim Should the nugget be estimated when fitting
    #' the GP model?
    update_mod_userspeclist = function(model=NULL, covtype=NULL,
                                       nugget.estim=NULL) {
      if (!is.null(model)) {
        stopifnot(is.character(model), length(model) == 1)
        if (tolower(model) %in% c("dk", "dicekriging", "dice")) {
          model <- "DK"
        }
        if (tolower(model) %in% c("gaupro", "gp")) {
          model <- "GauPro"
        }
        if (!(model %in% c("DK", "GauPro"))) {
          stop(paste0('model (', model, ') must be one of: ',
                      '"DK", "GauPro"'))
        }

        if (!self$par_all_cts && model != "GauPro") {
          message(paste0("Can only using GauPro for model when there are",
                         " discrete inputs"))
        }
        if (self$modlist$userspeclist$model != model) {
          self$modlist$userspeclist$model <- model
          self$modlist$needs_update <- TRUE
        }
      }
      if (!is.null(covtype)) {
        stopifnot(is.character(covtype), length(covtype) == 1)
        if (!(covtype %in% c("matern5_2", "matern3_2", "exp",
                             "powexp", "gauss"))) {
          stop(paste0('covtype must be one of: ',
                      '"matern5_2", "matern3_2", "exp", "powexp", "gauss"'))
        }
        # coerce to matern3_2
        if (self$modlist$userspeclist$covtype != covtype) {
          self$modlist$userspeclist$covtype <- covtype
          self$modlist$needs_update <- TRUE
        }
      }
      if (!is.null(nugget.estim)) {
        stopifnot(is.logical(nugget.estim), length(nugget.estim) == 1)
        if (self$modlist$userspeclist$nugget.estim != nugget.estim) {
          self$modlist$userspeclist$nugget.estim <- nugget.estim
          self$modlist$needs_update <- TRUE
        }
      }
      invisible(self)
    }
  ),
  active = list(
    mod = function(value) {
      # mod is an active binding since it returns the model, but first checks
      # to make sure it is up to date. If it isn't, it refits the model
      if (!missing(value)) {
        stop("You can't set the model")
      }
      stopifnot(!is.null(self$modlist),
                !is.null(self$modlist$needs_update),
                length(self$modlist$needs_update) == 1,
                is.logical(self$modlist$needs_update))
      if (self$modlist$needs_update) {
        if (self$verbose>=2) {
          cat("Updating model...\n")
        }
        self$fit_mod()
      } else {
        if (self$verbose>=5) {
          cat("Model is up to date\n")
        }
      }
      # Model is up to date. Return it.
      return(self$modlist$mod)
    }
  )
)

# Examples ----
if (F) {
  h1 <- hype(
    eval_func = function(a, b) {a^2+b^2},
    a = par_unif('a', -1, 2),
    b = par_unif('b', -10, 10),
    n_lhs = 10
  )
  h1
  h1$ffexp
  h1$run_all()
  h1$ffexp
  h1$add_EI(1)
  h1$ffexp
  h1$run_all()
  h1$ffexp
  h1$add_EI(4)
  h1$ffexp
  h1$run_all()
  h1$ffexp
  h1
  h1$plotorder()
  h1$plotX()
  h1$add_X(data.frame(a=1.111, b=2.222))
  h1$add_LHS(3)
  h1$add_EI(1)
}
if (F) {
  # Have df output, but only use one value from it
  h1 <- hype(
    eval_func = function(a, b) {data.frame(c=a^2+b^2, d=1:2)},
    extract_output_func = function(odf) {odf$c[1]},
    a = par_unif('a', -1, 2),
    b = par_unif('b', -10, 10),
    n_lhs = 10
  )
  h1$run_all()
  h1$add_EI(n = 1)
  system.time(h1$run_EI_for_time(sec=3, batch_size = 1))
  system.time(h1$run_EI_for_time(sec=3, batch_size = 3))
  h1$plotorder()
  h1$plotX()
}
if (F) { # Error when no par given
  h1 <- hype(
    eval_func = function(a, b) {data.frame(c=a^2+b^2, d=1:2)},
    extract_output_func = function(odf) {odf$c[1]},
    n_lhs = 10
  )
}

if (F) {
  h1 <- hype(
    eval_func = function(a) {print(a); (log(a) - log(1e-4))^2},
    n_lhs = 10,
    par_log$new("a", 1e-8, 1e-1)
  )
  h1
  h1$run_all()
  h1$X
  h1$plotX()
}
