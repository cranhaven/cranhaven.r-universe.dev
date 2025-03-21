# #' @noRd
# #' @title Checking miWQS functions
# #' @name check_miWQS
# #' @description Functions used to check if arguments of miWQS functions are in the right form. Modifies and returns arguments that are in right form.
# #' @inheritParams impute.multivariate.bayesian
#' @import methods


####################################################################################################
# #' @details
# #' check_imputation(): Check for proper execution of Bayesian imputation functions and bootstrap imputation function (impute.boot).
# #' @note Currently no check for verbose is included.
# #' @importFrom makeJournalTables is.naturalnumber
# #' @rdname check_miWQS

check_imputation <- function(X, DL, Z, K, T, n.burn, verbose = NULL) {
  ## Check X.  Desired class is a numeric matrix. X can also be a vector or data-frame. Errors if X is null or character.
  if (is.vector(X)) {
    #  warning("Only one chemical is imputed.", call. = FALSE)
    X  <- as.matrix(X)
  }
  X <- check_X(X)
  if (!anyNA(X)) stop("Matrix X has nothing to impute. No need to impute.", call. = FALSE)

  ## Check DL. Ideally a numeric vector with length = # of components.
  # Convert matrix and data-frame DL's into vectors
  if (is.matrix(DL) | is.data.frame(DL)) {
    # If DL is just saved as a 1-column or 1-row data-frame or matrix, convert to a vector. Otherwise, stop the function and force DL to be a vector.
    if (ncol(DL) == 1 | nrow(DL) == 1) {
      DL <-  as.numeric(DL)
    } else {
      stop("The detection limit must be a vector.", call. = FALSE)
    }
  } # end matrix/data.frame
  stopifnot(is.numeric(DL))

  # Remove any missing detection limits.
  if (anyNA(DL)) {
    no.DL <- which(is.na(DL))
    warning("The detection limit for ", names(no.DL), " is missing.
              Both the chemical and detection limit is removed in imputation.", call. = FALSE)
    DL <- DL [-no.DL]
    if (!(names(no.DL) %in% colnames(X))) {
      cat("  ##Missing DL does not match colnames(X) \n")
    }
    X <- X[, -no.DL]
  }

  # Does each chemical has a detection limit? The length of detection limit should equal number of chemicals
  if (length(DL) != ncol(X)) {
    cat("The following components \n");    X[1:3, ]
    cat("have these detection limits \n");   DL
    stop("Each component must have its own detection limit.", call. = FALSE)
  }

  ## Check Z: Create a model matrix.
  ## Desired class is COMPLETE numeric matrix (with columns being continuous or 0/1 dummy variables for categorical)
  # Done inside each individual function.

  ## K check a constant.
  K <- check_constants(K)

  ## Checking n.burn and T (are natural number constants)
  if (!is.null(n.burn) & !is.null(T)) {
    stopifnot(is.wholenumber(n.burn))
    T <- check_constants(T)
    if (!(n.burn < T))
      stop("Burn-in is too large; burn-in must be smaller than MCMC iterations T", call. = FALSE)
  }

  ## Return items that changed:
  return(list(X = X, DL = DL, Z = Z, K = K, T = T))
}

## Example: check_imputation()
# data(simdata87)
# l <- check_imputation( X =  simdata87$X.bdl[ , c(1,14)], DL = simdata87$DL[c(1, 14)],
#                         Z =  cbind( y = simdata87$y, simdata87$Z ), impute.algorithm = "gibbs",
#                         T = 100, n.burn = 5, K = 2)
#
#
# In the main function type
# check <- check_imputation(X, DL, Z, T, n.burn, K, verbose)
# X <- check$X
# DL <- check$DL
# Z  <- check$Z
# K <- check$K


# #To Debug
# X <- simdata87$X.bdl[ 1:100, c(1,14)]
# DL = simdata87$DL[c(1, 14)]
# Psi0 <- matrix(1, nrow = 8, ncol = c)
# Z <- cbind( y = simdata87$y, simdata87$Z )[1:100, ]
# impute.algorithm <- "gibbs"
# T <- 10
# n.burn <- 2
# K <- 2
# verbose <- TRUE

####################################################################################################
# #' @details
# #' check_function.Lub: Checks to make sure the arguments of impute.Lubin() are specified correctly.
# #' @rdname check_miWQS
# #' @inheritParams impute.Lubin

check_function.Lub <- function(chemcol, dlcol, Z,  K, verbose) {
  ## chemcol check: Should be a numeric vector.
  ## Note: check_X() checks if chemical X = chemcol has the proper form, but requires matrix or data-frame argument. However, chemcol is a vector so it is switch back. check_X() also used in Bayesian imputation & WQS models.

  X <- check_X(as.matrix(chemcol))
  if (ncol(X) > 1) {
    warning("This approach cannot impute more than one chemical at time. Imputing first element...")
    chemcol <- chemcol[1]
  } else {
    chemcol <- as.vector(X)
  }

  ## dlcol check
  if (is.null(dlcol))
    stop("dlcol is NULL. A detection limit is needed", call. = FALSE)
  if (is.na(dlcol))
    stop("The detection limit has missing values so chemical is not imputed.",
      immediate. = FALSE, call. = FALSE)
  if (!is.numeric(dlcol))
    stop("The detection limit is non-numeric. Detection limit must be numeric.", call. = FALSE)
  if (!is.vector(dlcol))
    stop("The detection limit must be a vector.", call. = FALSE)

  # If dlcol is a numeric vector, the dlcol is the smallest one. If the values are different, a warning is printed.
  if (length(dlcol) > 1) {
    # All values of dlcol are the same so just pick 1.
    if (min(dlcol, na.rm = TRUE) == max(dlcol, na.rm = TRUE)) {
      dlcol <- unique(dlcol)
    } else {
      # Values are different, so pick the smallest as detection limit.
      warning(" The detection limit is not unique, ranging from ",
        min(dlcol, na.rm = TRUE), " to ", max(dlcol, na.rm = TRUE),
        "; The smallest value is assumed to be detection limit",
        call. = FALSE, immediate. = FALSE)
      detcol <- !is.na(chemcol)

      # A Summary is printed
      cat("\n Summary when chemical is missing (BDL) \n")
      print(summary(dlcol[detcol == 0]))
      cat("## when chemical is observed \n ")
      print(summary(dlcol[detcol == 1]))

      # Assign Detection Limit:
      dlcol <- min(dlcol, na.rm = TRUE)           # if not, it is just the minimum of dlcol.
    }
  }

  ## Z checks
  if (is.null(Z)) {     # if Z is null, make it an intercept-term.
    Z <- matrix(rep(1, length(chemcol)), ncol = 1)
  }
  if (anyNA(Z)) {
    warning ("Missing covariates are ignored.")
  }

  # if Z is a vector or dataframe, redefine Z as numeric matrix.
  # Note: model.matrix() expects Z to be complete and contain no missing data.
  if (!is.matrix(Z)) {
    if (is.vector(Z) | is.factor(Z)) {
      Z <- model.matrix(~., model.frame(~Z))[, -1, drop = FALSE]
    } else if (is.data.frame(Z)) {
      Z <- model.matrix(~., model.frame(~., data = Z))[, -1, drop = FALSE]
    } else { # if ( is.array(Z) | is.list(Z) ){
      stop ("Please save Z as a vector, dataframe, or numeric matrix.")
    }
  }

  ## K check
  K <- check_constants(K)

  ## return any modifications of any parameters
  return(list(chemcol = chemcol, dlcol = dlcol, Z = Z, K = K))
}

####################################################################################################
# #' @details
# #' check_wqs_function(): Makes sure data is in right format to run WQS.
# #' @inheritParams estimate.wqs
# #' @rdname check_miWQS
check_wqs_function <- function(X, y, Z, proportion.train, n.quantiles, place.bdls.in.Q1, B,
                               b1.pos, signal.fn, family, offset, verbose) {
  ## Check Verbose
  if (!is(verbose, "logical"))
    stop("verbose must be logical value of TRUE or FALSE", call. = FALSE)

  ## Check Data
  ## Check X: Desired class is a numeric matrix. For anything else:
  if (is.vector(X)) {
    stop("Only one chemical is to be placed with an index. Don't run WQS, but a regular glm2.",
      call. = FALSE
    )
  }
  X <- check_X(X)

  ## Check y:  Desired format is a numeric vector. For anything else:
  if (!is.numeric(y)) {
    if (is.null(y)) {
      stop("y is NULL. y must have some data.", call. = FALSE)
    } else if (is.factor(y)) {
      warning("Converting y into a number.", call. = FALSE)
      y <- as.numeric(y) - 1
    } else {
      stop("y must be numeric or a factor.", call. = FALSE)
    }
  }
  if (length(y)  != nrow(X))
    stop("Can't Run: y and X have different lengths. The total number of individuals in outcome is ",
      length(y),
      ", but the number of individuals in X is ",
      nrow(X),
      ".",
      call. = FALSE)

  ## Check Z: Create a model matrix.
  ## Desired class is NULL or COMPLETE numeric matrix (with columns being continuous or 0/1 dummy variables for categorical).

  # Remove all missing values from outcome and covariates with warning.
  if (is.null(Z)) {
    df <- data.frame(y = y, X)
    if (verbose) cat("## Outcome Summary \n");  print(summary(df$y))
    if (anyNA(df$y)) {
      warning("All missing outcomes are ignored.")
      df <- df[complete.cases(df$y), ]
    }
  } else {         # Z is not null.

    ## Check: X and Z should have same number of rows
    nZ <- if (is.vector(Z) | is.factor(Z)) { length(Z) } else { nrow(Z) }       # save columns of Z
    if (nrow(X) != nZ) {
      cat("> X has", nrow(X), "individuals, but Z has", nZ, "individuals")
      stop("Can't Run. The total number of individuals with components X is different than total number of individuals with covariates Z.", call. = FALSE)
    }

    if (is.vector(Z)) Z <- as.matrix(Z)
    df <- data.frame(y = y, X, z2 = Z[, , drop = FALSE])
    cov_index <-  grep("z2", colnames(df))
    # if(verbose){
    #  cat("## Outcome & Covariate Summary \n")
    #  print( summary(df[ , c(1, cov_index)  ] ) )
    # }
    if (anyNA(df[, c(1, cov_index)])) {
      warning("All missing outcomes and/or covariates are ignored.")
      index <- complete.cases(df[, c(1, cov_index)])
      df <- df[index, ]
    }

    ## Format Z as a matrix.
    if (is.factor(Z) | is.data.frame(Z)) {
      Z <- model.matrix(y ~ ., data = df[, c(1, cov_index)]) [, -1]
    } else {
      Z <- df[, cov_index, drop = FALSE]
      if (ncol(Z) == 0)
        stop(sprintf("No covariates are found.",
          utils::head(df),
          "Something wrong with cov_index.",
          cov_index
        ))



    } # end reassignment if.
    colnames(Z) <- gsub("z2.", "", colnames(Z)) # Revert to orginal name.
  } # End null if.

  # Save new X and new y after removing missing covariates and outcomes.
  index <- 1 + 1:ncol(X)
  X <- df[, index]
  y <- df$y

  # Check Data
  if (verbose) {
    cat("## Classes: \n")
    classes <- c(class(y), class(X), class(Z)) ; names(classes) <-  c("y", "X", "Z")
    print(classes)
    cat("X \n"); print(summary(X))
    cat("y \n"); print(summary(y))
    cat("Z \n"); print(summary(Z))
  }


  ## Check proportion.train
  val <- ifelse(
    is.numeric(proportion.train) &
      0 < proportion.train & proportion.train <= 1,
    TRUE,
    FALSE
    )
  if (!val)
    stop("Proportion of values to the training set must be numeric, be greater than 0 and less than 1 (0< prop <=1).",
      call. = FALSE
    )

  ## Check n.quantiles
  quant <- ifelse(!is.null(n.quantiles),
    ifelse(is.numeric (n.quantiles)  & length(n.quantiles) == 1 & n.quantiles > 1, TRUE, FALSE),
    FALSE)
  if (!quant) stop("q must be numeric of length 1 and at least 1.", call. = FALSE)

  ## Check place.bdls.in.Q1
  if (!is(place.bdls.in.Q1, "logical"))
    stop("place bdls in Q1 must be logical value of TRUE or FALSE", call. = FALSE)

  ## Check B
  B <- check_constants(B)

  ## Check b1.pos
  if (!is(b1.pos, "logical"))
    stop("b1.pos must be logical value of TRUE or FALSE", call. = FALSE)

  ## Check family and offset
  if (!is(family, "character"))
    stop("family must be a character vector", call. = FALSE)
  if (!is(family, "character")) stop("family must be a character name of family")
  if (family == "poisson" & is.null(offset)) {
    warning("There is no offset specified. A count Poisson regression is performed.")
  }
  # If offset is null, convert offset to a vector of 1's -- need to be split.
  if (is.null(offset)) { offset <- rep(1, nrow(X)) }


  ## Return items that changed:
  return(list(X = X, y = y, Z = Z, B = B, offset = offset))
}

####################################################################################################
####################################################################################################
# #' @details
# #' check_X(): Desired class is a numeric matrix. X can also be data-frame. Errors if X is null or character.
# #' Called in Bayesian imputation, bootstrap imputation, and WQS
# #' @rdname check_miWQS

check_X <- function(X) {
  if (is.null(X)) { stop("X is null. X must have some data.", call. = FALSE) }
  # Can convert a numeric dataframe X into a matrix.
  if (is.data.frame(X)) {  X <- as.matrix(X) }
  #  if( is.list(X) ) { stop("X cannot be a list. It must be a matrix or data-frame.")}
  #  if( is.character(X) )  {  stop( "X is character." ) }
  if (!all (apply(X, 2, is.numeric))) {
    stop("X must be numeric. Some columns in X are not numeric.", call. = FALSE)
  }
  return(X)
}

# --------------------------------------------------------------------------------------------------
# #' @details
# #' check_constants: \itemize{
# #' \item The numbers NEED TO BE a positive natural number.
# #' \item Called in imputation model checks and also for B, number of Bootstraps.
# #' \item K, B, T, n.burn all used this function
# #' }
# #' @param K The number of imputed datasets, a natural number.
# #' @rdname check_miWQS

check_constants <- function(K) {
  if (is.null(K))
    stop(sprintf(" must be non-null."), call. = TRUE)
  if (!is.numeric(K) | length(K) != 1)
    stop(sprintf(" must be numeric of length 1"), call. = TRUE)
  if (K <= 0)
    stop(sprintf(" must be a positive integer"), call. = TRUE)
  if (!is.wholenumber(K)) {
    warning(sprintf(" is not a whole number. The next largest is taken."), call. = TRUE)
    K <- ceiling(K)
  }
  return(K)
}

# --------------------------------------------------------------------------------------------------
# #' @details
# #' check_covariates(): \itemize{
# #' \item ** IDEALLY, The covariates, Z, is a completely observed numeric matrix.
# #' \item ** Checks if covariates Z have the right structure (i.e. a complete numeric matrix).  Z may be an incomplete numeric matrix, factor or vector or data-frame. Z is converted to a complete numeric matrix.
# #' }
# #' This function is called upon Bayesian imputation functions.
# #' * impute.Lubin() has different requirement for Z, so not called.
# #' * Assumes that Z is on-NULL here. If Z is null, adjust in other check_functions.
# #' * In the imputation functions,  X and y are passing arguments.
# #' * Description of Z in impute.boot(): Any covariates used in imputing the chemical concentrations. If none, enter NULL. Ideally, a numeric matrix; but Z can be a vector or data-frame. Assumed to be complete; observations with missing covariate variables are ignored in the imputation, with a warning printed.

# #' @inheritParams impute.boot
# # DEFUNCT #'@param imputation.model Logical. If TRUE, covariates are checked for imputation model. There is no y in imputation model, so the y argument is ignored. If FALSE, covariates are checked for WQS model
# #' @param y  The outcome, y, passed from WQS model.
# #' @rdname check_miWQS

check_covariates <- function(Z, X) {
  ## Z should be non-NULL when passed to check_covariates() because model.frame() doesn't accept NULL values. Adjust in other check_functions, as WQS and imputation models handle it differently.
  # Z can be a vector, factor, data-frame, or numeric matrix.

  ## Check: X and Z should have same number of rows
  nZ <- if (is.vector(Z) | is.factor(Z)) { length(Z) } else { nrow(Z) }       # save columns of Z
  if (nrow(X) != nZ) {
    cat("> X has", nrow(X), "individuals, but Z has", nZ, "individuals")
    stop("Can't Run. The total number of individuals with components X is different than total number of individuals with covariates Z.", call. = FALSE)
  }

  ## Remove missing data from X  and save new Z and X.
  if (anyNA(Z)) {
    p <- if (is.vector(Z) | is.factor(Z)) { 1 } else { ncol(Z) }
    index <- complete.cases(Z)  # save vector of where Z is complete
    Z  <- Z[index, , drop = FALSE]  # save complete Z
    X  <- X[index, , drop = FALSE]   # save X, excluding missing data from Z
    dimnames(X)[[1]] <- 1:nrow(X)  # Renumber rows of X so that missing data can be excluded properly.
    warning ("Covariates are missing for individuals ", paste(which(!index), collapse = ", "), " and are ignored. Subjects renumbered in complete data.",  call. = FALSE, immediate. = TRUE)
    # Else if covarite matrix Z is used with an outcome (like WQS model), y needs to be included.
  }

  # Note: model.matrix() expects Z to be complete and contain no missing data.
  if (!is.null(Z) & !is.matrix(Z)) {
    if (is.vector(Z) | is.factor(Z)) {
      Z <- model.matrix(~., model.frame(~Z))[, -1, drop = FALSE]
    } else if (is.data.frame(Z)) {
      Z <- model.matrix(~., model.frame(~., data = Z))[, -1, drop = FALSE]
    } else if (is.array(Z)) {
      stop ("Please save Z as a vector, dataframe, or numeric matrix.")
    }
  }

  # Return data
  return(list(Z = Z, X = X))
}