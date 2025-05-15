#' general.indexes
#'
#' @description Calculates the confusion matrix, overall accuracy, overall error and the category accuracy for a classification problem
#' and the Root Mean Square Error, Mean Absolute Error, Relative Error and Correlation for a regression problem.
#'
#' @param newdata matrix or data frame of test data.
#' @param prediction a prmdt prediction object.
#' @param mc (optional) a matrix for calculating the indices. If mc is entered as parameter newdata and prediction are not necessary.
#'
#' @importFrom stats cor sd
#'
#' @return A list with the appropiate error and precision measurement. The class of this list is indexes.prmdt
#'
#' @export
#'
#' @examples
#'
#' # Classification
#' data("iris")
#'
#' n <- seq_len(nrow(iris))
#' .sample <- sample(n, length(n) * 0.75)
#' data.train <- iris[.sample,]
#' data.test <- iris[-.sample,]
#'
#' modelo.knn <- train.knn(Species~., data.train)
#' prediccion <- predict(modelo.knn, data.test, type = "class")
#' general.indexes(data.test, prediccion)
#'
#' # Regression
#' len <- nrow(swiss)
#' sampl <- sample(x = 1:len,size = len*0.20,replace = FALSE)
#' ttesting <- swiss[sampl,]
#' ttraining <- swiss[-sampl,]
#' model.knn <- train.knn(Infant.Mortality~.,ttraining)
#' prediccion <- predict(model.knn, ttesting)
#' prediccion
#' general.indexes(ttesting, prediccion)
#'
general.indexes <- function(newdata, prediction, mc = NULL){
  indexes <- list()

  if(is.null(mc)) {
    real <- newdata[[prediction$var.pred]]
    pred <- prediction$prediction

    if(is.numeric(real) & is.numeric(pred)) {
      indexes[["RMSE"]] <- sqrt(sum((real - pred)^2)/length(pred))
      indexes[["MAE"]]    <- sum(abs(real - pred))/length(pred)
      indexes[["RE"]]    <- sum(abs(real - pred))/sum(abs(real)) * 100
      desvStand <- sd(pred)
      COR <- ifelse(near(desvStand, 0), 0, as.numeric(cor(real, pred)))
      COR <- ifelse(is.na(COR), 0, COR)
      indexes[["COR"]] <- COR
    } else {
      mc <- confusion.matrix(newdata, prediction)
      indexes[["confusion.matrix"]] <- mc
      indexes[["overall.accuracy"]] <- sum(diag(mc))/sum(mc)
      indexes[["overall.error"]] <- 1 - indexes[["overall.accuracy"]]
      indexes[["category.accuracy"]] <- diag(mc)/rowSums(mc)
    }
  } else {
    indexes[["confusion.matrix"]] <- mc
    indexes[["overall.accuracy"]] <- sum(diag(mc))/sum(mc)
    indexes[["overall.error"]] <- 1 - indexes[["overall.accuracy"]]
    indexes[["category.accuracy"]] <- diag(mc)/rowSums(mc)
  }

  class(indexes) <- c("indexes.prmdt", "list")
  return(indexes)
}
