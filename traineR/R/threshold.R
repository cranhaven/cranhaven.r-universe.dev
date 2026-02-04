#' ROC.plot
#'
#' @description Function that plots the ROC curve of a prediction with only 2 categories.
#'
#' @param prediction A vector of real numbers representing the prediction score of a category.
#' @param real A vector with the real categories of the individuals in the prediction.
#' @param .add A logical value that indicates if it should be added to an existing graph
#' @param color Color of the ROC curve in the graph
#'
#' @seealso \code{\link[ROCR]{prediction}} and \code{\link[ROCR]{performance}}
#'
#' @return A plot object.
#'
#' @importFrom graphics grid segments
#'
#' @export
#'
#' @examples
#'
#' iris2 <- dplyr::filter(iris,(Species == "setosa") | (Species == "virginica"))
#' iris2$Species <- factor(iris2$Species,levels = c("setosa","virginica"))
#' sam <- sample(1:100,20)
#' ttesting <- iris2[sam,]
#' ttraining <- iris2[-sam,]
#' model <- train.rpart(Species~.,ttraining)
#' prediction.prob <- predict(model,ttesting, type = "prob")
#' ROC.plot(prediction.prob$prediction[,2],ttesting$Species)
#'
ROC.plot <- function(prediction, real, .add = FALSE, color = "red") {
  pred <- ROCR::prediction(prediction, real)
  perf <- ROCR::performance(pred, "tpr", "fpr")
  plot(perf, col = color, add = .add, main = "ROC curve")
  segments(0, 0, 1, 1, col='black')
  grid()
}


#' ROC.area
#'
#' @description Function that calculates the area of the ROC curve of a prediction with only 2 categories.
#'
#' @param prediction A vector of real numbers representing the prediction score of a category.
#' @param real A vector with the real categories of the individuals in the prediction.
#'
#' @seealso \code{\link[ROCR]{prediction}} and \code{\link[ROCR]{performance}}
#'
#' @return The value of the area(numeric).
#'
#' @export
#'
#' @examples
#'
#' iris2 <- dplyr::filter(iris,(Species == "setosa") | (Species == "virginica"))
#' iris2$Species <- factor(iris2$Species,levels = c("setosa","virginica"))
#' sam <- sample(1:100,20)
#' ttesting <- iris2[sam,]
#' ttraining <- iris2[-sam,]
#' model <- train.rpart(Species~.,ttraining)
#' prediction.prob <- predict(model,ttesting, type = "prob")
#' ROC.area(prediction.prob$prediction[,2],ttesting$Species)
#'
ROC.area <- function(prediction, real) {
  pred <- ROCR::prediction(prediction, real)
  auc <- ROCR::performance(pred, "auc")
  return(attributes(auc)$y.values[[1]])
}
