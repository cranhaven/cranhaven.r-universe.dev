#' (Adaptive) Boosting Trees (ABT/BT) fit.
#'
#' Fit a (Adaptive) Boosting Trees algorithm. This is for "power" users who have a large number of variables and wish to avoid calling
#' \code{model.frame} which can be slow in this instance. This function is in particular called by \code{\link{BT}}.
#' It is mainly split in two parts, the first one considers the initialization (see \code{BT_callInit}) whereas the second performs all the boosting iterations (see \code{BT_callBoosting}).
#' By default, this function does not perform input checks (those are all done in \code{\link{BT}}) and all the parameters should be given in the right format. We therefore
#' suppose that the user is aware of all the choices made.
#'
#' @param training.set a data frame containing all the related variables on which one wants to fit the algorithm.
#'
#' @param validation.set a held-out data frame containing all the related variables on which one wants to assess the algorithm performance. This can be NULL.
#'
#' @param tweedie.power Experimental parameter currently not used - Set to 1 referring to Poisson distribution.
#'
#' @param respVar the name of the target/response variable.
#'
#' @param w a vector of weights.
#'
#' @param explVar a vector containing the name of explanatory variables.
#'
#' @param ABT a boolean parameter. If \code{ABT=TRUE} an adaptive boosting tree algorithm is built whereas if \code{ABT=FALSE} an usual boosting tree algorithm is run.
#'
#' @param tree.control allows to define additional tree parameters that will be used at each iteration. See \code{\link{rpart.control}} for more information.
#'
#' @param train.fraction the first \code{train.fraction * nrows(data)} observations are used to fit the \code{BT} and the remainder are used for
#' computing out-of-sample estimates (also known as validation error) of the loss function. It is mainly used to report the value in the \code{BTFit} object.
#'
#' @param interaction.depth the maximum depth of variable interactions: 1 builds an additive model, 2 builds a model with up to two-way interactions, etc.
#' This parameter can also be interpreted as the maximum number of non-terminal nodes. By default, it is set to 4.
#' Please note that if this parameter is \code{NULL}, all the trees in the expansion are built based on the \code{tree.control} parameter only.
#' This option is devoted to advanced users only and allows them to benefit from the full flexibility of the implemented algorithm.
#'
#' @param bag.fraction the fraction of independent training observations randomly selected to propose the next tree in the expansion.
#' This introduces randomness into the model fit. If \code{bag.fraction}<1 then running the same model twice will result in similar but different fits.
#' \code{BT} uses the R random number generator, so \code{set.seed} ensures the same model can be reconstructed. Please note that if this parameter is used the \code{BTErrors$training.error}
#' corresponds to the normalized in-bag error.
#'
#' @param shrinkage a shrinkage parameter applied to each tree in the expansion. Also known as the learning rate or step-size reduction.
#'
#' @param n.iter the total number of iterations to fit. This is equivalent to the number of trees and the number of basis functions in the additive expansion.
#' Please note that the initialization is not taken into account in the \code{n.iter}. More explicitly, a weighted average initializes the algorithm and then \code{n.iter} trees
#' are built. Moreover, note that the \code{bag.fraction}, \code{colsample.bytree}, ... are not used for this initializing phase.
#'
#' @param colsample.bytree each tree will be trained on a random subset of \code{colsample.bytree} number of features. Each tree will consider a new
#' random subset of features from the formula, adding variability to the algorithm and reducing computation time. \code{colsample.bytree} will be bounded between
#' 1 and the number of features considered.
#'
#' @param keep.data a boolean variable indicating whether to keep the data frames. This is particularly useful if one wants to keep track of the initial data frames
#' and is further used for predicting in case any data frame is specified.
#' Note that in case of cross-validation, if \code{keep.data=TRUE} the initial data frames are saved whereas the cross-validation samples are not.
#'
#' @param is.verbose if \code{is.verbose=TRUE}, the \code{BT} will print out the algorithm progress.
#'
#' @return a \code{\link{BTFit}} object.
#'
#' @author Gireg Willame \email{gireg.willame@@gmail.com}
#'
#' \emph{This package is inspired by the \code{gbm3} package. For more details, see \url{https://github.com/gbm-developers/gbm3/}}.
#'
#' @seealso \code{\link{BTFit}}, \code{\link{BTCVFit}}, \code{\link{BT_perf}}, \code{\link{predict.BTFit}},
#' \code{\link{summary.BTFit}}, \code{\link{print.BTFit}}, \code{\link{.BT_cv_errors}}.
#'
#' @references M. Denuit, D. Hainaut and J. Trufin (2019). \strong{Effective Statistical Learning Methods for Actuaries |: GLMs and Extensions}, \emph{Springer Actuarial}.
#'
#' M. Denuit, D. Hainaut and J. Trufin (2019). \strong{Effective Statistical Learning Methods for Actuaries ||: Tree-Based Methods and Extensions}, \emph{Springer Actuarial}.
#'
#' M. Denuit, D. Hainaut and J. Trufin (2019). \strong{Effective Statistical Learning Methods for Actuaries |||: Neural Networks and Extensions}, \emph{Springer Actuarial}.
#'
#' M. Denuit, D. Hainaut and J. Trufin (2022). \strong{Response versus gradient boosting trees, GLMs and neural networks under Tweedie loss and log-link}.
#' Accepted for publication in \emph{Scandinavian Actuarial Journal}.
#'
#' M. Denuit, J. Huyghe and J. Trufin (2022). \strong{Boosting cost-complexity pruned trees on Tweedie responses: The ABT machine for insurance ratemaking}.
#' Paper submitted for publication.
#'
#' M. Denuit, J. Trufin and T. Verdebout (2022). \strong{Boosting on the responses with Tweedie loss functions}. Paper submitted for publication.
#'
#' @rdname BT_Call
#' @export
#'
BT_call <-
  function(training.set,
           validation.set,
           tweedie.power,
           respVar,
           w,
           explVar,
           ABT,
           tree.control,
           train.fraction,
           interaction.depth,
           bag.fraction,
           shrinkage,
           n.iter,
           colsample.bytree,
           keep.data,
           is.verbose) {
    # Create storage objects.
    BT <- list()

    # Init GLM + Init error.
    if (is.verbose)
      message('bag.fraction is not used for the initialization fit.')
    init <-
      BT_callInit(training.set, validation.set, tweedie.power, respVar, w)
    initF <-
      list(
        initFit = init$initFit,
        training.error = init$trainingError,
        validation.error = init$validationError
      )
    currTrainScore <- init$currTrainScore
    currValScore <- init$currValScore
    rm(init)
    gc()

    # Boosting algorithm.
    BT <-
      BT_callBoosting(
        cbind(training.set, currTrainScore),
        cbind(validation.set, currValScore),
        tweedie.power,
        ABT,
        tree.control,
        interaction.depth,
        bag.fraction,
        shrinkage,
        n.iter,
        colsample.bytree,
        train.fraction,
        keep.data,
        is.verbose,
        respVar,
        w,
        explVar
      )

    # Add parameters in the list and init.
    BT$BTInit <- structure(initF, class = "BTInit")

    class(BT) <- "BTFit"
    return(BT)
  }

#' @rdname BT_Call
#' @export
#'
BT_callInit <-
  function(training.set,
           validation.set,
           tweedie.power,
           respVar,
           w) {
    initFit <-
      sum(training.set[, w] * training.set[, respVar]) / sum(training.set[, w])
    currTrainScore <-
      rep(log(initFit), nrow(training.set)) # Return value on score scale.
    trainingError <-
      sum(BT_devTweedie(
        training.set[, respVar],
        exp(currTrainScore),
        tweedieVal = tweedie.power,
        w = training.set[, w]
      )) / nrow(training.set)#sum(mf$originalWeights)

    currValScore <- NULL
    validationError <- NULL
    if (!is.null(validation.set)) {
      currValScore <-
        rep(log(initFit), nrow(validation.set)) # Return value on score scale.
      validationError <-
        sum(
          BT_devTweedie(
            validation.set[, respVar],
            exp(currValScore),
            tweedieVal = tweedie.power,
            w = validation.set[, w]
          )
        ) / nrow(validation.set)
    }
    return(
      list(
        initFit = initFit,
        currTrainScore = currTrainScore,
        currValScore = currValScore,
        trainingError = trainingError,
        validationError = validationError
      )
    )
  }

#' @rdname BT_Call
#' @export
#'
BT_callBoosting <-
  function(training.set,
           validation.set,
           tweedie.power,
           ABT,
           tree.control,
           interaction.depth,
           bag.fraction,
           shrinkage,
           n.iter,
           colsample.bytree,
           train.fraction,
           keep.data,
           is.verbose,
           respVar,
           w,
           explVar) {
    sampRow <- 1:nrow(training.set)
    currFormula <-
      as.formula(paste("residuals ~ ", paste(explVar, collapse = " + ")))

    training.error <- NULL
    validation.error <- NULL
    oob.improvement <- NULL
    listFits <- list()

    for (iTree in seq_len(n.iter)) {
      if (is.verbose) {
        if ((iTree <= 10) ||
            (iTree <= 100 &&
             iTree %% 10 == 0) ||
            (iTree %% 100 == 0))
          message("Iteration: ", iTree)
      }

      training.set[, "residuals"] <-
        training.set[, respVar] / exp(training.set[, "currTrainScore"])
      training.set[, "iWeights"] <-
        training.set[, w] * (exp(training.set[, "currTrainScore"]) ^ (2 - tweedie.power))

      if (bag.fraction < 1) {
        sampRow <-
          sample(1:nrow(training.set), bag.fraction * nrow(training.set))
        oobRow <- setdiff(1:nrow(training.set), sampRow)
        oldOOBError <-
          (sum(
            BT_devTweedie(
              training.set[oobRow, respVar],
              exp(training.set[oobRow, "currTrainScore"]),
              tweedieVal = tweedie.power,
              w = training.set[oobRow, w]
            )
          ) / length(oobRow))
        if (iTree == 1)
          initOOB <- oldOOBError
      }

      if ((!is.null(colsample.bytree)) &&
          (colsample.bytree != length(explVar))) {
        sampVar <- explVar[sample(1:length(explVar), colsample.bytree)]
        currFormula <-
          as.formula(paste("residuals ~ ", paste(sampVar, collapse = " + ")))
      }

      # Fit the current tree, update score and store the fit.
      if (tweedie.power == 1) {
        currFit <-
          rpart(
            currFormula,
            training.set[sampRow, ],
            weights = training.set[sampRow, "iWeights"],
            method = "poisson",
            control = tree.control,
            y = FALSE
          ) #iWeights is also working but not best.
      } else{
        stop("Currently implemented for Poisson distribution only.")
        # currFit <- rpart(currFormula, training.set[sampRow,], weights = iWeights, method = list(eval=evalTweedie, split=splitTweedie, init=initTweedie),
        #                   parms = list(tweedieVal=tweedie.power), control=tree.control, y=FALSE) # y = FALSE : do not keep a copy of the response variable.
      }

      # We need to prune the tree. If interaction.depth is NULL then the maxdepth approach is chosen and no pruning needed.
      if (!is.null(interaction.depth)) {
        if (!ABT) {
          # interaction.depth defined and BT approach chosen.
          splittingStrategy <-
            .BT_splittingStrategy(currFit, interaction.depth)
          if (!is.null(splittingStrategy) &&
              length(splittingStrategy) > 0)
            currFit <- snip.rpart(currFit, toss = splittingStrategy)
        } else{
          # interaction.depth defined and ABT approach chosen.
          currFit <-
            prune(currFit, cp = currFit$cptable[, "CP"][max(which(currFit$cptable[, "nsplit"] <= interaction.depth))])
        }
      }

      # Delete the where object resulting from rpart. Not needed for the predict afterwards.
      currFit$where <- NULL
      training.set[, "currTrainScore"] <-
        training.set[, "currTrainScore"] + shrinkage * log(predict(currFit, newdata =
                                                                     training.set, type = "vector"))
      listFits[[iTree]] <- currFit

      # Compute errors.
      training.error[iTree] <-
        sum(BT_devTweedie(
          training.set[sampRow, respVar],
          exp(training.set[sampRow, "currTrainScore"]),
          tweedieVal = tweedie.power,
          w = training.set[sampRow, w]
        )) / length(sampRow) # In-bag error.
      if (bag.fraction < 1) {
        oob.improvement[iTree] <-
          (oldOOBError - (sum(
            BT_devTweedie(
              training.set[oobRow, respVar],
              exp(training.set[oobRow, "currTrainScore"]),
              tweedieVal = tweedie.power,
              w = training.set[oobRow, w]
            )
          ) / length(oobRow))) # OOB Improvement.
      }
      if (!is.null(validation.set)) {
        validation.set[, "currValScore"] <-
          validation.set[, "currValScore"] + shrinkage * log(predict(currFit, newdata =
                                                                       validation.set, type = "vector"))
        validation.error[iTree] <-
          sum(
            BT_devTweedie(
              validation.set[, respVar],
              exp(validation.set[, "currValScore"]),
              tweedieVal = tweedie.power,
              w = validation.set[, w]
            )
          ) / nrow(validation.set) # Validation error.
      }
    } # End loop

    # Return errors, fitted trees, misc.
    BT_CallBoosting <- list()
    BT_CallBoosting$BTErrors <-
      structure(
        list(
          training.error = training.error,
          validation.error = validation.error,
          oob.improvement = oob.improvement
        ),
        class = "BTErrors"
      )

    class(listFits) <- "BTIndivFits"
    BT_CallBoosting$BTIndivFits <- listFits

    BT_CallBoosting$distribution <- tweedie.power
    BT_CallBoosting$var.names <- explVar
    BT_CallBoosting$response <- respVar
    BT_CallBoosting$w <- w
    if (keep.data)
      BT_CallBoosting$BTData <-
      structure(list(training.set = training.set[, !(colnames(training.set) %in% c("iWeights", "residuals"))],
                     validation.set = validation.set),
                class = "BTData")

    BT_CallBoosting$BTParams <- structure(
      list(
        ABT = ABT,
        train.fraction = train.fraction,
        shrinkage = shrinkage,
        interaction.depth = interaction.depth,
        bag.fraction = bag.fraction,
        n.iter = n.iter,
        colsample.bytree = colsample.bytree,
        tree.control = tree.control
      ),
      class = "BTParams"
    )

    BT_CallBoosting$keep.data <- keep.data
    BT_CallBoosting$is.verbose <- is.verbose

    BT_CallBoosting$fitted.values <- training.set[, "currTrainScore"]

    return(BT_CallBoosting)
  }
