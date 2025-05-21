
#' @title CoBC generic method
#' @description CoBC is a semi-supervised learning algorithm with a co-training
#' style. This algorithm trains \code{N} classifiers with the learning scheme defined in
#' \code{gen.learner} using a reduced set of labeled examples. For each iteration, an unlabeled
#' example is labeled for a classifier if the most confident classifications assigned by the
#' other \code{N-1} classifiers agree on the labeling proposed. The unlabeled examples
#' candidates are selected randomly from a pool of size \code{u}.
#' @param y A vector with the labels of training instances. In this vector the
#' unlabeled instances are specified with the value \code{NA}.
#' @param gen.learner A function for training \code{N} supervised base classifiers.
#' This function needs two parameters, indexes and cls, where indexes indicates
#' the instances to use and cls specifies the classes of those instances.
#' @param gen.pred A function for predicting the probabilities per classes.
#' This function must be two parameters, model and indexes, where the model
#' is a classifier trained with \code{gen.learner} function and
#' indexes indicates the instances to predict.
#' @param N The number of classifiers used as committee members. All these classifiers
#' are trained using the \code{gen.learner} function. Default is 3.
#' @param perc.full A number between 0 and 1. If the percentage
#' of new labeled examples reaches this value the self-labeling process is stopped.
#' Default is 0.7.
#' @param u Number of unlabeled instances in the pool. Default is 100.
#' @param max.iter Maximum number of iterations to execute in the self-labeling process.
#' Default is 50.
#' @details
#' coBCG can be helpful in those cases where the method selected as
#' base classifier needs a \code{learner} and \code{pred} functions with other
#' specifications. For more information about the general coBC method,
#' please see \code{\link{coBC}} function. Essentially, \code{coBC}
#' function is a wrapper of \code{coBCG} function.
#' @return A list object of class "coBCG" containing:
#' \describe{
#'   \item{model}{The final \code{N} base classifiers trained using the enlarged labeled set.}
#'   \item{model.index}{List of \code{N} vectors of indexes related to the training instances
#'   used per each classifier. These indexes are relative to the \code{y} argument.}
#'   \item{instances.index}{The indexes of all training instances used to
#'   train the \code{N} models. These indexes include the initial labeled instances
#'   and the newly labeled instances. These indexes are relative to the \code{y} argument.}
#'   \item{model.index.map}{List of three vectors with the same information in \code{model.index}
#'   but the indexes are relative to \code{instances.index} vector.}
#'   \item{classes}{The levels of \code{y} factor.}
#' }
#' @example demo/CoBCG.R
#' @export
coBCG <- function(
  y,
  gen.learner,
  gen.pred,
  N = 3,
  perc.full = 0.7,
  u = 100,
  max.iter = 50
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
  # Check N
  if (N <= 0) {
    stop("Parameter N must be a positive number.")
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

  # Check u
  if (u <= nclasses) {
    stop("Parameter u must be greather than the number of classes.")
  }

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

  ### Co-bagging algorithm ###

  # Count the examples per class
  cls.summary <- summary(y[labeled])
  # Determine the total of instances to include per iteration
  cantClass <- round(cls.summary / min(cls.summary)) # divido por el valor minimo
  totalPerIter <- sum(cantClass)

  # Lists to store the training instances indexes and it's classes
  Lind <- vector(mode = "list", length = N)
  Lcls <- vector(mode = "list", length = N)
  # List to store the models
  H <- vector(mode = "list", length = N)

  # Train models
  s = resample(y[labeled], N = N)
  y.map <- unclass(y)
  for (i in 1:N) {
    # Train model
    indexes <- labeled[s[[i]]]
    H[[i]] <- gen.learner(indexes, y[indexes])
    # Save instances info
    Lind[[i]] <- indexes
    Lcls[[i]] <- y.map[indexes]
  }
  # Save original hypothesis
  HO <- H

  iter <- 1
  min.amount <- round(length(unlabeled) * (1 - perc.full))
  while ((length(unlabeled) > min.amount) && (iter <= max.iter)) {

    end <- N
    for (i in 1:N) {
      # For each classifier
      if (length(unlabeled) > totalPerIter) {
        # Can I satisfy the i classifier
        # Select randomly a pool of unlabeled instances
        pool <- sample(x = unlabeled, size = min(u, length(unlabeled)), replace = FALSE)

        ## Select the more competent instances
        # Obtain the committee for the classifier i
        committee <- setdiff(1:length(H), i)
        # Predict probabilities for unlabeled prime instances
        ninstances = length(pool)
        prob <- .coBCCombine(
          h.prob = lapply(
            X = H[committee],
            FUN = function(model) {
              prob <- gen.pred(model, pool)
              colnames(prob) <- classes
              checkProb(prob = prob, ninstances, classes)
            }

          ),
          ninstances,
          classes
        )
        # Select instances
        sel <- selectInstances(cantClass = cantClass, probabilities = prob)
        selected <- pool[sel$unlabeled.idx]

        ## Verify with the initial training set
        # Predict probabilities
        ninstances = length(selected)
        prob <- .coBCCombine(
          h.prob = lapply(
            X = HO,
            FUN = function(model) {
              prob <- gen.pred(model, selected)
              colnames(prob) <- classes
              checkProb(prob = prob, ninstances, classes)
            }

          ),
          ninstances,
          classes
        )
        # Compute classes
        cls.idx <- apply(prob, MARGIN = 1, FUN = which.max)
        # Compare
        indCoinciden <- which(cls.idx == sel$class.idx)

        # Add indCoinciden set to the training examples set of classifier i
        Lind[[i]] <- c(Lind[[i]], selected[indCoinciden])
        Lcls[[i]] <- c(Lcls[[i]], cls.idx[indCoinciden])

        # Remove the unlabeled instances selected
        unlabeled <- setdiff(unlabeled, selected)
      } else {
        end <- i - 1
        break
      }
    }
    # End for each classifier

    # Train models with new instances
    if(end > 0){
      for (i in 1:end) {
        # Train classifier
        ind <- Lind[[i]] # indexes of intances
        yi <- classes[Lcls[[i]]] # indexes of classes
        H[[i]] <- gen.learner(ind, factor(yi, classes))
      }
    }


    iter <- iter + 1
  }
  # End of main while

  ### Result ###

  # determine labeled instances
  instances.index <- unique(unlist(Lind))
  # map indexes respect to m$included.insts
  model.index.map <- lapply(
    X = Lind,
    FUN = function(indexes) {
      r <- unclass(factor(indexes, levels = instances.index))
      attr(r, "levels") <- NULL
      return(r)
    }
  )

  # Save result
  result <- list(
    model = H,
    gen.pred = gen.pred,
    model.index = Lind,
    instances.index = instances.index,
    model.index.map = model.index.map,
    classes = classes
  )
  class(result) <- "coBCG"

  return(result)
}

#' @title General Interface for CoBC model
#' @description Co-Training by Committee (CoBC) is a semi-supervised learning algorithm
#' with a co-training style. This algorithm trains \code{N} classifiers with the learning
#' scheme defined in the \code{learner} argument using a reduced set of labeled examples. For
#' each iteration, an unlabeled
#' example is labeled for a classifier if the most confident classifications assigned by the
#' other \code{N-1} classifiers agree on the labeling proposed. The unlabeled examples
#' candidates are selected randomly from a pool of size \code{u}.
#' The final prediction is the average of the estimates of the N regressors.
#' @param learner model from parsnip package for training a supervised base classifier
#' using a set of instances. This model need to have probability predictions in classification mode
#' @param N The number of classifiers used as committee members. All these classifiers
#' are trained using the \code{gen.learner} function. Default is 3.
#' @param perc.full A number between 0 and 1. If the percentage
#' of new labeled examples reaches this value the self-labeling process is stopped.
#' Default is 0.7.
#' @param u Number of unlabeled instances in the pool. Default is 100.
#' @param max.iter Maximum number of iterations to execute in the self-labeling process.
#' Default is 50.
#' @details
#' For regression tasks, labeling data is very expensive computationally. Its so slow.
#' This method trains an ensemble of diverse classifiers. To promote the initial diversity
#' the classifiers are trained from the reduced set of labeled examples by Bagging.
#' The stopping criterion is defined through the fulfillment of one of the following
#' criteria: the algorithm reaches the number of iterations defined in the \code{max.iter}
#' parameter or the portion of unlabeled set, defined in the \code{perc.full} parameter,
#' is moved to the enlarged labeled set of the classifiers.
#' @return (When model fit) A list object of class "coBC" containing:
#' \describe{
#'   \item{model}{The final \code{N} base classifiers trained using the enlarged labeled set.}
#'   \item{model.index}{List of \code{N} vectors of indexes related to the training instances
#'   used per each classifier. These indexes are relative to the \code{y} argument.}
#'   \item{instances.index}{The indexes of all training instances used to
#'   train the \code{N} models. These indexes include the initial labeled instances
#'   and the newly labeled instances. These indexes are relative to the \code{y} argument.}
#'   \item{model.index.map}{List of three vectors with the same information in \code{model.index}
#'   but the indexes are relative to \code{instances.index} vector.}
#'   \item{classes}{The levels of \code{y} factor in classification.}
#'   \item{pred}{The function provided in the \code{pred} argument.}
#'   \item{pred.pars}{The list provided in the \code{pred.pars} argument.}
#' }
#' @references
#' Avrim Blum and Tom Mitchell.\cr
#' \emph{Combining labeled and unlabeled data with co-training.}\cr
#' In Eleventh Annual Conference on Computational Learning Theory, COLT’ 98, pages 92-100, New York, NY, USA, 1998. ACM.
#' ISBN 1-58113-057-0. doi: 10.1145/279943.279962.\cr\cr
#' Mohamed Farouk Abdel-Hady, Mohamed Farouk Abdel-Hady and Günther Palm.\cr
#' \emph{Semi-supervised Learning for Regression with Cotraining by Committee}\cr
#' Institute of Neural Information Processing
#' University of Ulm
#' D-89069 Ulm, Germany
#' @example demo/COBC.R
#' @importFrom magrittr %>%
#' @export
coBC <- function(
  learner,
  N = 3,
  perc.full = 0.7,
  u = 100,
  max.iter = 50
) {

  train_function <- function(x, y) {
    ### Check parameters ###
    x <- as.data.frame(x)

    mode <- NULL

    if(is.factor(y) | is.character(y))
      mode = "classification"

    else
      mode = "regression"

    result <- NULL

    if(mode == "regression")
    {
      #Call coBCreg
      model <- coBCReg(learner = learner, N = N, perc.full = perc.full, u = u, max.iter = max.iter)

      model_fitted <- model %>% fit_xy(x,y)
      result <- model_fitted$model
      return(result)
    }

    #Else for classification

    y <- as.factor(y)

    # Instance matrix case
    gen.learner1 <- function(training.ints, cls) {
      m <- learner %>% parsnip::fit_xy(x = x[training.ints,], y = cls)
      return(m)
    }

    gen.pred1 <- function(m, testing.ints) {
      prob <- predict(m, x[testing.ints,], type = "prob")
      return(prob)
    }

    result <- coBCG(y, gen.learner1, gen.pred1, N, perc.full, u, max.iter)

    ### Result ###
    result$classes = levels(y)
    result$pred.params = c("class","raw")
    result$mode = "classification"
    class(result) <- "coBC"

    return(result)
  }

  args <- list(
    learner = learner,
    N = N,
    perc.full = perc.full,
    u = u,
    max.iter = max.iter
  )

  new_model_sslr(train_function, "coBC", args)
}

#' @title Predictions of the coBC method
#' @description Predicts the label of instances according to the \code{coBC} model.
#' @details For additional help see \code{\link{coBC}} examples.
#' @param object coBC model built with the \code{\link{coBC}} function.
#' @param x An object that can be coerced to a matrix.
#' Depending on how the model was built, \code{x} is interpreted as a matrix
#' with the distances between the unseen instances and the selected training instances,
#' or a matrix of instances.
#' @param ... This parameter is included for compatibility reasons.
#' @return Vector with the labels assigned.
#' @method predict coBC
#' @importFrom stats predict
predict.coBC <- function(object, x,...) {

  ninstances = nrow(x)
  # Predict probabilities per instances using each model

  h.prob <- mapply(
    FUN = function(model) {
      prob <- predict(model, x, type = "prob")
      colnames(prob) <- object$classes

      checkProb(
        prob,
        ninstances,
        object$classes
      )
    },
    object$model,
    SIMPLIFY = FALSE
  )


  pred <- getClass(
  # Combine probability matrices
    .coBCCombine(h.prob, ninstances, object$classes)
  )

  pred
}

#' @title Combining the hypothesis
#' @description This function combines the probabilities predicted by the committee of
#' classifiers.
#' @param h.prob A list of probability matrices.
#' @param classes The classes in the same order that appear
#' in the columns of each matrix in \code{h.prob}.
#' @return A probability matrix
#' @export
coBCCombine <- function(h.prob, classes) {
  # Check the number of instances
  ninstances <- unique(vapply(X = h.prob, FUN = nrow, FUN.VALUE = numeric(1)))
  if (length(ninstances) != 1) {
    stop("The row number of matrixes in the 'pred' parameter are not all equals.")
  }
  # Check prob matrixes
  vapply(X = h.prob, FUN.VALUE = numeric(),
         FUN = function(prob) {
          checkProb(prob, ninstances, classes)
          numeric()
         }
  )

  pred <- getClass(
  # Combine probability matrices
    .coBCCombine(h.prob, ninstances, classes)
  )

  return(pred)
}

.coBCCombine <- function(h.prob, ninstances, classes) {

  nclasses <- length(classes)

  H.pro <- matrix(nrow = ninstances, ncol = nclasses)
  h.prob <- lapply(h.prob, as.matrix)
  for (u in 1:ninstances) {
    den <- sum(vapply(X = h.prob, FUN = function(prob) sum(prob[u,]), FUN.VALUE = numeric(1)))

    num <- vapply(
      X = 1:nclasses,
      FUN = function(c) {
        sum(vapply(X = h.prob, FUN = function(prob) prob[u, c], FUN.VALUE = numeric(1)))
      },
      FUN.VALUE = numeric(1)
    )

    H.pro[u,] <- num / den
  }

  colnames(H.pro) <- classes

  return(H.pro)
}


