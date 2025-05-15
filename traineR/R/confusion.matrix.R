
#' confusion.matrix
#'
#' @description create the confusion matrix.
#'
#' @param newdata matrix or data frame of test data.
#' @param prediction a prmdt prediction object.
#'
#' @return A matrix with predicted and actual values.
#'
#' @export
#'
#' @examples
#'
#' data("iris")
#'
#' n <- seq_len(nrow(iris))
#' .sample <- sample(n, length(n) * 0.75)
#' data.train <- iris[.sample,]
#' data.test <- iris[-.sample,]
#'
#' modelo.knn <- train.knn(Species~., data.train)
#' modelo.knn
#' prob <- predict(modelo.knn, data.test, type = "prob")
#' prob
#' prediccion <- predict(modelo.knn, data.test, type = "class")
#' prediccion
#' confusion.matrix(data.test, prediccion)
#'
confusion.matrix <- function(newdata, prediction){
  real <- newdata[, prediction$var.pred]
  prediction <- prediction$prediction
  return(table(real, prediction))
}
