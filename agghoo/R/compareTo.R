# standardCV_core
#
# Cross-validation method, added here as an example.
# Parameters are described in ?agghoo and ?AgghooCV
standardCV_core <- function(data, target, task, gmodel, params, loss, CV) {
  n <- nrow(data)
  shuffle_inds <- NULL
  if (CV$type == "vfold" && CV$shuffle)
    shuffle_inds <- sample(n, n)
  list_testinds <- list()
  for (v in seq_len(CV$V))
    list_testinds[[v]] <- get_testIndices(n, CV, v, shuffle_inds)
  gmodel <- agghoo::Model$new(data, target, task, gmodel, params)
  best_error <- Inf
  best_p <- NULL
  for (p in seq_len(gmodel$nmodels)) {
    error <- Reduce('+', lapply(seq_len(CV$V), function(v) {
      testIdx <- list_testinds[[v]]
      d <- splitTrainTest(data, target, testIdx)
      model_pred <- gmodel$get(d$dataTrain, d$targetTrain, p)
      prediction <- model_pred(d$dataTest)
      loss(prediction, d$targetTest)
    }) )
    if (error <= best_error) {
      if (error == best_error)
        best_p[[length(best_p)+1]] <- p
      else {
        best_p <- list(p)
        best_error <- error
      }
    }
  }
  chosenP <- best_p[[ sample(length(best_p), 1) ]]
  list(model=gmodel$get(data, target, chosenP), param=gmodel$getParam(chosenP))
}

# CVvoting_core
#
# "voting" cross-validation method, added here as an example.
# Parameters are described in ?agghoo and ?AgghooCV
CVvoting_core <- function(data, target, task, gmodel, params, loss, CV) {
  CV <- checkCV(CV)
  n <- nrow(data)
  shuffle_inds <- NULL
  if (CV$type == "vfold" && CV$shuffle)
    shuffle_inds <- sample(n, n)
  gmodel <- agghoo::Model$new(data, target, task, gmodel, params)
  bestP <- rep(0, gmodel$nmodels)
  for (v in seq_len(CV$V)) {
    test_indices <- get_testIndices(n, CV, v, shuffle_inds)
    d <- splitTrainTest(data, target, test_indices)
    best_p <- NULL
    best_error <- Inf
    for (p in seq_len(gmodel$nmodels)) {
      model_pred <- gmodel$get(d$dataTrain, d$targetTrain, p)
      prediction <- model_pred(d$dataTest)
      error <- loss(prediction, d$targetTest)
      if (error <= best_error) {
        if (error == best_error)
          best_p[[length(best_p)+1]] <- p
        else {
          best_p <- list(p)
          best_error <- error
        }
      }
    }
    for (p in best_p)
      bestP[p] <- bestP[p] + 1
  }
  # Choose a param at random in case of ex-aequos:
  maxP <- max(bestP)
  chosenP <- sample(which(bestP == maxP), 1)
  list(model=gmodel$get(data, target, chosenP), param=gmodel$getParam(chosenP))
}

#' standardCV_run
#'
#' Run and eval the standard cross-validation procedure.
#'
#' @param dataTrain Train dataset
#' @param dataTest Test dataset
#' @param targetTrain Train targets
#' @param targetTest Test targets
#' @param floss Loss function to compute error on test dataset
#' @param verbose Show some execution trace
#' @param ... List defining the model (gmodel) and its parameters (params)
#'
#' @export
standardCV_run <- function(
  dataTrain, dataTest, targetTrain, targetTest, floss, verbose, ...
) {
  args <- list(...)
  task <- checkTask(args$task, targetTrain)
  modPar <- checkModPar(args$gmodel, args$params)
  loss <- checkLoss(args$loss, task)
  CV <- checkCV(args$CV)
  s <- standardCV_core(
    dataTrain, targetTrain, task, modPar$gmodel, modPar$params, loss, CV)
  if (verbose)
    cat(paste( "Parameter:", s$param ), "\n")
  p <- s$model(dataTest)
  err <- floss(p, targetTest)
  if (verbose)
    cat(paste("error CV:", err), "\n")
  invisible(err)
}

#' CVvoting_run
#'
#' Run and eval the voting cross-validation procedure.
#'
#' @inheritParams standardCV_run
#'
#' @export
CVvoting_run <- function(
  dataTrain, dataTest, targetTrain, targetTest, floss, verbose, ...
) {
  args <- list(...)
  task <- checkTask(args$task, targetTrain)
  modPar <- checkModPar(args$gmodel, args$params)
  loss <- checkLoss(args$loss, task)
  CV <- checkCV(args$CV)
  s <- CVvoting_core(
    dataTrain, targetTrain, task, modPar$gmodel, modPar$params, loss, CV)
  if (verbose)
    cat(paste( "Parameter:", s$param ), "\n")
  p <- s$model(dataTest)
  err <- floss(p, targetTest)
  if (verbose)
    cat(paste("error CV:", err), "\n")
  invisible(err)
}

#' agghoo_run
#'
#' Run and eval the agghoo procedure.
#'
#' @inheritParams standardCV_run
#'
#' @export
agghoo_run <- function(
  dataTrain, dataTest, targetTrain, targetTest, floss, verbose, ...
) {
  args <- list(...)
  CV <- checkCV(args$CV)
  # Must remove CV arg, or agghoo will complain "error: unused arg"
  args$CV <- NULL
  a <- do.call(agghoo, c(list(data=dataTrain, target=targetTrain), args))
  a$fit(CV)
  if (verbose) {
    cat("Parameters:\n")
    cat(unlist(a$getParams()), "\n")
  }
  pa <- a$predict(dataTest)
  err <- floss(pa, targetTest)
  if (verbose)
    cat(paste("error agghoo:", err), "\n")
  invisible(err)
}

#' compareTo
#'
#' Compare a list of learning methods (or run only one), on data/target.
#'
#' @param data Data matrix or data.frame
#' @param target Target vector (generally)
#' @param method_s Either a single function, or a list
#'                 (examples: agghoo_run, standardCV_run)
#' @param rseed Seed of the random generator (-1 means "random seed")
#' @param floss Loss function to compute the error on testing dataset.
#' @param verbose TRUE to request methods to be verbose.
#' @param ... arguments passed to method_s function(s)
#'
#' @export
compareTo <- function(
  data, target, method_s, rseed=-1, floss=NULL, verbose=TRUE, ...
) {
  if (rseed >= 0)
    set.seed(rseed)
  n <- nrow(data)
  test_indices <- sample( n, round(n / ifelse(n >= 500, 10, 5)) )
  d <- splitTrainTest(data, target, test_indices)

  # Set error function to be used on model outputs (not in core method)
  task <- checkTask(list(...)$task, target)
  if (is.null(floss)) {
    floss <- function(y1, y2) {
      ifelse(task == "classification", mean(y1 != y2), mean(abs(y1 - y2)))
    }
  }

  # Run (and compare) all methods:
  runOne <- function(o) {
    o(d$dataTrain, d$dataTest, d$targetTrain, d$targetTest, floss, verbose, ...)
  }
  errors <- c()
  if (is.list(method_s))
    errors <- sapply(method_s, runOne)
  else if (is.function(method_s))
    errors <- runOne(method_s)
  invisible(errors)
}

#' compareMulti
#'
#' Run compareTo N times in parallel.
#'
#' @inheritParams compareTo
#' @param N Number of calls to method(s)
#' @param nc Number of cores. Set to parallel::detectCores() if undefined.
#'           Set it to any value <=1 to say "no parallelism".
#' @param verbose TRUE to print task numbers and "Errors:" in the end.
#'
#' @export
compareMulti <- function(
  data, target, method_s, N=100, nc=NA, floss=NULL, verbose=TRUE, ...
) {
  if (is.na(nc))
    nc <- parallel::detectCores()

  # "One" comparison for each method in method_s (list)
  compareOne <- function(n) {
    if (verbose)
      cat(n, "\n")
    compareTo(data, target, method_s, n, floss, verbose=FALSE, ...)
  }

  errors <- if (nc >= 2) {
    parallel::mclapply(1:N, compareOne, mc.cores = nc)
  } else {
    lapply(1:N, compareOne)
  }
  if (verbose)
    cat("Errors:", "\n")
  Reduce('+', errors) / N
}

#' compareRange
#'
#' Run compareMulti on several values of the parameter V.
#'
#' @inheritParams compareMulti
#' @param V_range Values of V to be tested.
#'
#' @export
compareRange <- function(
  data, target, method_s, N=100, nc=NA, floss=NULL, V_range=c(10,15,20), ...
) {
  args <- list(...)
  # Avoid warnings if V is left unspecified:
  CV <- suppressWarnings( checkCV(args$CV) )
  errors <- lapply(V_range, function(V) {
    args$CV$V <- V
    do.call(compareMulti, c(list(data=data, target=target, method_s=method_s,
                                 N=N, nc=nc, floss=floss, verbose=FALSE), args))
  })
  errors
}
