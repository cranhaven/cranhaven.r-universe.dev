#' Fit a Decision Tree Model
#'
#' @description
#' Fits a decision tree to training data using one of several supported tree implementations
#' (rpart, party, C50, or via caret) and returns a standardized party object along with
#' variable importance scores.
#'
#' @param data_train Data frame. Explicit training set. If NULL, will be subset from `data` by `data_type == 'train'`.
#' @param data Data frame. Combined dataset with a `data_type` column when `data_train` is NULL.
#' @param target_lab Character. Name of the target column to predict.
#' @param model Character. Which implementation to use: one of "rpart", "party", "C50", or "caret".
#' @param task Character. Type of task: "classification" or "regression".
#' @param control List or control object. Optional control parameters passed to the chosen tree function.
#'
#' @return A list with elements:
#'   \item{fit}{A party object representing the fitted tree.}
#'   \item{var_imp}{A named numeric vector of relative variable importance (scaled to sum to 1 and rounded to two decimals).}
#'
#' @export
#'
#' @examples
#'
#' library(partykit)
#' library(C50)
#' library(caret)
#'
#' data(train_covid)
#' train_tree(data_train = train_covid, target_lab = "Outcome", model = "rpart")
#' train_tree(data_train = train_covid, target_lab = "Outcome", model = "C50")
#' train_tree(data_train = train_covid, target_lab = "Outcome", model = "caret")
#'
#' data(Psychosis_Disorder)
#' data <- add_data_type(data_all = Psychosis_Disorder)
#' data <- prepare_features(data, target_lab = "UNIQID", task = "classification")
#' train_tree(
#'   data = data, target_lab = "UNIQID", model = "party",
#'   control = ctree_control(minbucket = 15)
#' )
train_tree <- function(data_train = NULL,
                       data = NULL,
                       target_lab = NULL,
                       model = c("rpart", "party", "C50", "caret"),
                       task = c("classification", "regression"),
                       control = NULL) {
  task <- match.arg(task)
  model <- match.arg(model)

  if (is.null(data_train) && is.null(data)) {
    stop("Please provide the train model.")
  }

  if (is.null(data_train)) {
    data_train <- subset(data, data_type == "train")
  }


  # Create formula for model fitting
  formula <- as.formula(paste(target_lab, "~ ."))

  # Fit the decision tree model based on selected model
  tree_model <- switch(
    model,
    "rpart" = {
      method <- if (task == "regression")
        "anova"
      else
        "class"
      if (is.null(control)) {
        rpart(formula, data = data_train, method = method)
      } else {
        rpart(formula,
              data = data_train,
              control = control,
              method = method)
      }
    },
    "party" = {
      if (is.null(control)) {
        ctree(formula, data = data_train)
      } else {
        ctree(formula, data = data_train, control = control)
      }
    },
    "C50" = {
      if (task == "regression")
        stop("C50 does not support regression.")
      if (is.null(control)) {
        C5.0(formula, data = data_train)
      } else {
        C5.0(formula, data = data_train, control = control)
      }
    },
    "caret" = {
      if (is.null(control)) {
        train(formula, data = data_train, method = "rpart")
      } else {
        train(formula,
              data = data_train,
              method = "rpart",
              control = control)
      }
    },
    stop(
      "Unsupported decision tree model. Choose from: rpart / party / caret / C50"
    )
  )

  # Convert all tree models to party object

  fit <- switch(
    model,
    "party" = tree_model,
    "rpart" = partykit::as.party(tree_model),
    "C50" = partykit::as.party(tree_model),
    "caret" = partykit::as.party(tree_model$finalModel)
  )

  # Compute variable importance
  var_imp <- switch(
    model,
    "rpart" = {
      tmp <- tempfile()
      sink(tmp)
      s <- summary(tree_model)
      sink()
      var_imp <- s$variable.importance
    },
    "party" = {
      partykit::varimp(tree_model)
    },
    "C50" = {
      C50::C5imp(tree_model, metric = "usage")[, "Overall"]
    },
    "caret" = {
      summary(tree_model)$variable.importance
    }
  )
  var_imp <- round(var_imp / sum(var_imp), 2)

  list(fit     = fit, var_imp = var_imp)
}


#' Fit a Conditional Random Forest
#'
#' @description
#' Fits a conditional random forest using \code{partykit::cforest()} and
#' returns the forest object along with variable importance scores.
#'
#' @param data_train Data frame. Training data.
#' @param target_lab Character. Name of the target column.
#' @param task Character. \code{"classification"} or \code{"regression"}.
#' @param ntree Integer. Number of trees (default 500).
#' @param mtry Integer or NULL. Number of variables randomly sampled at each
#'   split. If NULL, uses the \code{cforest} default.
#' @param control A \code{ctree_control} object or NULL.
#'
#' @return A list with elements:
#'   \item{forest}{The fitted \code{cforest} object.}
#'   \item{var_imp}{A named numeric vector of relative variable importance
#'     (scaled to sum to 1 and rounded to two decimals).}
#'   \item{ntree}{Integer. Number of trees in the forest.}
#'
#' @export
#'
#' @examples
#' \donttest{
#' data(train_covid)
#' rf_res <- train_rf(train_covid, target_lab = "Outcome", ntree = 50)
#' rf_res$var_imp
#' }
train_rf <- function(data_train,
                     target_lab,
                     task = c("classification", "regression"),
                     ntree = 500L,
                     mtry = NULL,
                     control = NULL) {
  task <- match.arg(task)
  formula <- stats::as.formula(paste(target_lab, "~ ."))

  cf_args <- list(formula = formula, data = data_train, ntree = ntree)
  if (!is.null(mtry))  cf_args$mtry  <- mtry
  if (!is.null(control)) cf_args$control <- control

  forest <- do.call(partykit::cforest, cf_args)

  vi <- partykit::varimp(forest)
  vi <- round(vi / sum(vi), 2)

  list(forest = forest, var_imp = vi, ntree = ntree)
}
