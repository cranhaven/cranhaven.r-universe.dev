#' @title train_iimi()
#'
#' @export
#' @importFrom randomForest randomForest
#' @importFrom mltools sparsify
#' @importFrom xgboost xgboost
#' @importFrom data.table data.table
#' @importFrom MTPS cv.glmnet2
#' @importFrom caret createFolds
#' @importFrom stats model.matrix
#'
#'
#' @examples
#' \dontrun{
#' df <- convert_rle_to_df(example_cov)
#' train_x <- df[,-c(1:4)]
#' train_y = c()
#' for (ii in 1:nrow(df)) {
#'   seg_id = df$seg_id[ii]
#'   sample_id = df$sample_id[ii]
#'   train_y = c(train_y, example_diag[seg_id, sample_id])
#' }
#' trained_model <- train_iimi(train_x = train_x, train_y = train_y)
#' }
#'
#'
#'
#'
#' @description Trains a `XGBoost` (default), `Random Forest`, or `Elastic Net`
#'     model using user-provided data.
#'
#'
#' @param train_x A data frame or a matrix of predictors.
#' @param train_y A response vector of labels (needs to be a factor).
#' @param method The machine learning method of choice, `Random Forest` or
#'     `XGBoost`, or `Elastic Net` model. Default is `XGBoost` model.
#' @param nrounds Max number of boosting iterations for `XGBoost` model. Default
#'     is 100.
#' @param min_child_weight Default is 10.
#' @param gamma Minimum loss reduction required in `XGBoost` model. Default is 20.
#' @param ntree Number of trees in `Random Forest` model. Default is 100.
#' @param mtry Default is 10.
#' @param k Number of folds. Default is 5.
#' @param \dots Other arguments that can be passed to \code{randomForest},
#'     \code{xgboost}, or \code{glmnet}.
#'
#' @return A `Random Forest`, `XGBoost`, `Elastic Net` model






train_iimi <- function(train_x,
                       train_y,
                       method = "xgb",
                       nrounds = 100,
                       min_child_weight = 10,
                       gamma = 20,
                       ntree = 200,
                       mtry = 10,
                       k = 5,
                       ...) {
  if (method == "rf") {
    trained_model = randomForest(
      x = train_x[, -c(1:4)],
      y = train_y,
      ntree = ntree,
      mtry = mtry,
      ...
    )
  }

  if (method == "xgb") {
    xgbtrain <- sparsify(data.table(train_x[, -c(1:4)]))
    xgblabel <- as.numeric(as.logical(train_y))

    trained_model = xgboost(
      data = xgbtrain,
      label = xgblabel,
      objective = "binary:logistic",
      nrounds = nrounds,
      min_child_weight = min_child_weight,
      gamma = gamma,
      ...
    )

  }

  if (method == "en") {
    train = cbind(train_y, train_x[,-c(1:4)])
    colnames(train)[1] = "labels"
    xx.train = model.matrix(labels ~ ., train)[,-1]
    yy.train = as.numeric(as.logical(train_y))
    foldid <- createFolds(yy.train, k = k, list = F)
    trained_model <-
      cv.glmnet2(xx.train,
                 yy.train,
                 family = "binomial",
                 foldid = foldid,
                 ...)

  }

  if (method %in% c("rf", "xgb", "en") == F) {
    stop("`method` must be `rf`, `xgb`, or `en`.")
  }

  trained_model
}
