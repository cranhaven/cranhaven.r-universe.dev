#' @title SETRED generic method
#' @description SETRED is a variant of the self-training classification method
#' (\code{\link{selfTraining}}) with a different addition mechanism.
#' The SETRED classifier is initially trained with a
#' reduced set of labeled examples. Then it is iteratively retrained with its own most
#' confident predictions over the unlabeled examples. SETRED uses an amending scheme
#' to avoid the introduction of noisy examples into the enlarged labeled set. For each
#' iteration, the mislabeled examples are identified using the local information provided
#' by the neighborhood graph.
#' @param y A vector with the labels of training instances. In this vector the
#' unlabeled instances are specified with the value \code{NA}.
#' @param D A distance matrix between all the training instances. This matrix is used to
#' construct the neighborhood graph.
#' @param gen.learner A function for training a supervised base classifier.
#' This function needs two parameters, indexes and cls, where indexes indicates
#' the instances to use and cls specifies the classes of those instances.
#' @param gen.pred A function for predicting the probabilities per classes.
#' This function must be two parameters, model and indexes, where the model
#' is a classifier trained with \code{gen.learner} function and
#' indexes indicates the instances to predict.
#' @param theta Rejection threshold to test the critical region. Default is 0.1.
#' @param max.iter Maximum number of iterations to execute the self-labeling process.
#' Default is 50.
#' @param perc.full A number between 0 and 1. If the percentage
#' of new labeled examples reaches this value the self-training process is stopped.
#' Default is 0.7.
#' @details
#' SetredG can be helpful in those cases where the method selected as
#' base classifier needs a \code{learner} and \code{pred} functions with other
#' specifications. For more information about the general setred method,
#' please see \code{\link{setred}} function. Essentially, \code{setred}
#' function is a wrapper of \code{setredG} function.
#' @return A list object of class "setredG" containing:
#' \describe{
#'   \item{model}{The final base classifier trained using the enlarged labeled set.}
#'   \item{instances.index}{The indexes of the training instances used to
#'   train the \code{model}. These indexes include the initial labeled instances
#'   and the newly labeled instances.
#'   Those indexes are relative to the \code{y} argument.}
#' }
#' @example demo/SETREDG.R
#' @export
setredG <- function(
  y, D, gen.learner, gen.pred,
  theta = 0.1,
  max.iter = 50,
  perc.full = 0.7
) {
  ### Check parameters ###
  # Check y
  if (!is.factor(y)) {
    if (!is.vector(y)) {
      stop("Parameter y is neither a vector nor a factor.")
    } else {
      y = as.factor(y)
    }
  }
  # Check distance matrix
  D <- as.matrix(D)

  if (!is.matrix(D)) {
    stop("Parameter D is neither a matrix or a dist object.")
  } else if (nrow(D) != ncol(D)) {
    stop("The distance matrix D is not a square matrix.")
  } else if (nrow(D) != length(y)) {
    stop(sprintf(paste("The dimensions of the matrix D is %i x %i",
                       "and it's expected %i x %i according to the size of y."),
                 nrow(D), ncol(D), length(y), length(y)))
  }

  # Check theta
  if (!(theta >= 0 && theta <= 1)) {
    stop("theta must be between 0 and 1")
  }
  # Check max.iter
  if (max.iter < 1) {
    stop("Parameter max.iter is less than 1. Expected a value greater than and equal to 1.")
  }
  # Check perc.full
  if (perc.full < 0 || perc.full > 1) {
    stop("Parameter perc.full is not in the range 0 to 1.")
  }

  ### Init variables ###
  # Identify the classes
  classes <- levels(y)
  nclasses <- length(classes)

  # Init variable to store the labels
  ynew <- y

  # Obtain the indexes of labeled and unlabeled instances
  labeled <- which(!is.na(y))
  unlabeled <- which(is.na(y))
  ## Check the labeled and unlabeled sets
  if (length(labeled) == 0) {
    # labeled is empty
    stop("The labeled set is empty. All the values in y parameter are NA.")
  }
  if (length(unlabeled) == 0) {
    # unlabeled is empty
    stop("The unlabeled set is empty. None value in y parameter is NA.")
  }

  ### SETRED algorithm ###

  # Count the examples per class
  cls.summary <- summary(y[labeled])
  # Ratio between count per class and the initial number of labeled instances
  proportion <- cls.summary / length(labeled)
  # Determine the total of instances to include per iteration
  cantClass <- round(cls.summary / min(cls.summary))
  totalPerIter <- sum(cantClass)
  # Compute count full
  count.full <- length(labeled) + round(length(unlabeled) * perc.full)

  iter <- 1
  while ((length(labeled) < count.full) && (length(unlabeled) >= totalPerIter) && (iter <= max.iter)) {

    # Train classifier
    #model <- trainModel(x[labeled, ], ynew[labeled], learner, learner.pars)
    model <- gen.learner(labeled, ynew[labeled])

    # Predict probabilities per classes of unlabeled examples
    #prob <- predProb(model, x[unlabeled, ], pred, pred.pars, classes)
    prob <- gen.pred(model, unlabeled)
    colnames(prob) <- classes
    prob <- checkProb(prob = prob, ninstances = length(unlabeled), classes)

    # Select the instances with better class probability
    selection <- selectInstances(cantClass, as.matrix(prob))

    # Save count of labeled set before it's modification
    nlabeled.old <- length(labeled)

    # Add selected instances to L
    labeled.prime <- unlabeled[selection$unlabeled.idx]
    sel.classes <- classes[selection$class.idx]
    ynew[labeled.prime] <- sel.classes
    labeled <- c(labeled, labeled.prime)

    # Delete selected instances from U
    unlabeled <- unlabeled[-selection$unlabeled.idx]

    # Save count of labeled set after it's modification
    nlabeled.new <- length(labeled)

    # Build a neighborhood graph G with L U L'
    'ady <- vector("list", nlabeled.new) # Adjacency list of G

    for (i in (nlabeled.old + 1):nlabeled.new){
      for (j in 1:(i - 1)) {
        con <- TRUE
        for (k in 1:nlabeled.new)
          if (k != i && k != j && D[labeled[i], labeled[j]] >
              max(D[labeled[i], labeled[k]], D[labeled[k], labeled[j]])) {
            con <- FALSE
            break
          }
        if (con) {
          ady[[i]] <- c(ady[[i]],j)
          ady[[j]] <- c(ady[[j]],i)
        }
      }
    }'


    #Call cpp loop function
    ady <- setred_loop(nlabeled.new, nlabeled.old, D, as.numeric(labeled))


    # Compute the bad examples and noise instances
    noise.insts <- c() # instances to delete from labeled set
    for (i in (nlabeled.old + 1):nlabeled.new) {
      # only on L'
      propi <- proportion[unclass(ynew[labeled[i]])]

      # calculate Oi observation of Ji
      Oi <- 0
      nv <- W <- k <- 0
      for (j in ady[[i]]) {
        k <- k + 1
        W[k] <- 1 / (1 + D[labeled[i], labeled[j]])
        if (ynew[labeled[i]] != ynew[labeled[j]]) {
          Oi <- Oi + W[k]
          nv <- nv + 1
        }
      }

      if (normalCriterion(theta, Oi, length(ady[[i]]), propi, W)) {
        noise.insts <- c(noise.insts, i)
      }
    }

    # Delete from labeled the noise instances
    if (length(noise.insts) > 0) {
      ynew[labeled[noise.insts]] <- NA
      labeled <- labeled[-noise.insts]
    }

    iter <- iter + 1
  }

  ### Result ###

  # Train final model
  #model <- trainModel(x[labeled, ], ynew[labeled], learner, learner.pars)
  model <- gen.learner(labeled, ynew[labeled])

  # Save result
  result <- list(
    model = model,
    instances.index = labeled
  )
  class(result) <- "setredG"

  result
}

#' @title General Interface for SETRED model
#' @description SETRED (SElf-TRaining with EDiting) is a variant of the self-training
#' classification method (as implemented in the function \code{\link{selfTraining}}) with a different addition mechanism.
#' The SETRED classifier is initially trained with a
#' reduced set of labeled examples. Then, it is iteratively retrained with its own most
#' confident predictions over the unlabeled examples. SETRED uses an amending scheme
#' to avoid the introduction of noisy examples into the enlarged labeled set. For each
#' iteration, the mislabeled examples are identified using the local information provided
#' by the neighborhood graph.
#' @param dist A distance function or the name of a distance available
#' in the \code{proxy} package to compute. Default is "Euclidean"
#' the distance matrix in the case that \code{D} is \code{NULL}.
#' @param learner model from parsnip package for training a supervised base classifier
#' using a set of instances. This model need to have probability predictions
#' (or optionally a distance matrix) and it's corresponding classes.
#' @param theta Rejection threshold to test the critical region. Default is 0.1.
#' @param max.iter maximum number of iterations to execute the self-labeling process.
#' Default is 50.
#' @param perc.full A number between 0 and 1. If the percentage
#' of new labeled examples reaches this value the self-training process is stopped.
#' Default is 0.7.
#' @param D A distance matrix between all the training instances. This matrix is used to
#' construct the neighborhood graph. Default is NULL, this means the
#' method create a matrix with dist param
#' @details
#' SETRED initiates the self-labeling process by training a model from the original
#' labeled set. In each iteration, the \code{learner} function detects unlabeled
#' examples for which it makes the most confident prediction and labels those examples
#' according to the \code{pred} function. The identification of mislabeled examples is
#' performed using a neighborhood graph created from the distance matrix.
#' Most examples possess the same label in a neighborhood. So if an example locates
#' in a neighborhood with too many neighbors from different classes, this example should
#' be considered problematic. The value of the \code{theta} argument controls the confidence
#' of the candidates selected to enlarge the labeled set. The lower this value is, the more
#' restrictive is the selection of the examples that are considered good.
#' For more information about the self-labeled process and the rest of the parameters, please
#' see \code{\link{selfTraining}}.
#'
#' @return (When model fit) A list object of class "setred" containing:
#' \describe{
#'   \item{model}{The final base classifier trained using the enlarged labeled set.}
#'   \item{instances.index}{The indexes of the training instances used to
#'   train the \code{model}. These indexes include the initial labeled instances
#'   and the newly labeled instances.
#'   Those indexes are relative to \code{x} argument.}
#'   \item{classes}{The levels of \code{y} factor.}
#'   \item{pred}{The function provided in the \code{pred} argument.}
#'   \item{pred.pars}{The list provided in the \code{pred.pars} argument.}
#' }
#' @references
#' Ming Li and ZhiHua Zhou.\cr
#' \emph{Setred: Self-training with editing.}\cr
#' In Advances in Knowledge Discovery and Data Mining, volume 3518 of Lecture Notes in
#' Computer Science, pages 611-621. Springer Berlin Heidelberg, 2005.
#' ISBN 978-3-540-26076-9. doi: 10.1007/11430919 71.
#' @example demo/SETRED.R
#' @importFrom magrittr %>%
#' @export
setred <- function(
  dist = "Euclidean",
  learner,
  theta = 0.1,
  max.iter = 50,
  perc.full = 0.7,
  D = NULL
) {
  ### Check parameters ###

  train_function <- function(x, y) {
    y <- as.factor(y)

    # Instance matrix case
    gen.learner2 <- function(training.ints, cls) {
      m <- learner %>% parsnip::fit_xy(x = x[training.ints,], y = cls)
      return(m)
    }
    gen.pred2 <- function(m, testing.ints) {
      prob <- predict(m, x[testing.ints,], type = "prob")
      return(prob)
    }

    if (is.null(D))
      distance <- proxy::dist(x, method = dist, by_rows = TRUE, diag = TRUE, upper = TRUE)

    else
      distance <- D

    pred.pars <- list(type = "prob")

    result <- setredG(
        y,
        D = distance,
        gen.learner2, gen.pred2,
        theta, max.iter, perc.full
      )

    ### Result ###
    result$classes = levels(y)
    result$pred = "predict"
    result$pred.pars = "prob"
    result$pred.params = c("class","raw")
    result$mode = "classification"
    class(result) <- "setred"


    result

  }

  args <- list(
    dist = dist,
    learner = learner,
    theta = theta,
    max.iter = max.iter,
    perc.full = perc.full
  )

  new_model_sslr(train_function, "setred", args)

}

#' @title Predictions of the SETRED method
#' @description Predicts the label of instances according to the \code{setred} model.
#' @details For additional help see \code{\link{setred}} examples.
#' @param object SETRED model built with the \code{\link{setred}} function.
#' @param x A object that can be coerced as matrix.
#' Depending on how was the model built, \code{x} is interpreted as a matrix
#' with the distances between the unseen instances and the selected training instances,
#' or a matrix of instances.
#' @param ... This parameter is included for compatibility reasons.
#' @param col_name is the colname from returned tibble in class type.
#' The same from parsnip and tidymodels
#' Default is .pred_clas
#' @return Vector with the labels assigned.
#' @method predict setred
#' @importFrom stats predict
predict.setred <- function(object, x, col_name = ".pred_class", ...) {

  prob <- predProb(object$model, x, object$pred, object$pred.pars)
  colnames(prob) <- object$classes

  result <- getClass(
    checkProb(
      prob = prob,
      ninstances = nrow(x),
      object$classes
    )
  )

  result
}

#' @title Normal criterion
#' @details Computes the critical value using the normal distribution as the authors suggest
#' when the neighborhood is big for the instances in the RNG.
#' @return A boolean value indicating if the instance must be eliminated
#' @noRd
normalCriterion <- function(theta, Oi, vec, propi, W) {
  # calculate mean and desv est of J
  mean <- (1 - propi) * sum(W)
  sd <- sqrt(propi * (1 - propi) * sum(W ^ 2))

  # calculate p-value for Oi
  vc <- stats::qnorm(theta / 2, mean, sd)

  if (vc < 0 && Oi == 0) # special case where vc <0 product of the approximation by dist.
  FALSE
  else
    Oi >= vc
}
