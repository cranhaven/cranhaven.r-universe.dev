#' Train Network
#'
#' Train the network with specified hyperparameters and return the trained
#' model.
#'
#' @param train_data set of training data
#' @param train_target set of training data targets in one-hot encoded form
#' @param validate_data set of validation data targets in one-hot encoded form
#' @param validate_target set of targets in
#' @param model list of weights and biases
#' @param alpha learning rate
#' @param epochs number of epochs
#' @param batch_size mini-batch size
#' @param plot_acc whether or not to plot training and validation accuracy
#'
#' @return list of weights and biases after training
#' @export
#'
#' @examples
#' \dontrun{
#' mlp_model <- init_nn(784, 100, 50, 10)
#' mnist <- load_mnist()
#' train_data <- mnist[1]
#' train_target <- mnist[2]
#' validate_data <- mnist[3]
#' validate_target <- mnist[4]
#' mlp_model <- train_nn(train_data, train_target, validate_data,
#' validate_target, mlp_model, 0.01, 1, 64)
#' }
train_nn <- function(train_data, train_target, validate_data, validate_target, model, alpha, epochs, batch_size=nrow(train_data), plot_acc=TRUE) {
  train_data <- as.matrix(as.data.frame(train_data))
  train_target <- as.matrix(as.data.frame(train_target))

  n <- nrow(train_data)
  num_iter <- n %/% batch_size

  accuracy_train <- c(rep(NaN, epochs * num_iter))
  accuracy_validate <- c(rep(NaN, epochs * num_iter))

  for (epoch in 1:epochs) {
    boots <- sample(1:n, size = n, replace = FALSE)
    train_data <- train_data[boots,]
    train_target <- train_target[boots,]
    for (iter in 1:num_iter) {
      start <- (iter - 1) * batch_size + 1
      end <- min(n, (iter) * batch_size)
      x <- train_data[start:end,]
      t <- train_target[start:end,]

      forward_pass <- forwardprop(model, x)
      prediction <- softmax(forward_pass["y"])

      ce <- -sum(t * log(prediction)) / nrow(x)
      accuracy <- sum(max.col(prediction) == max.col(t)) / nrow(x)
      cat(epoch, iter, "Cross Entropy: ", ce, "Accuracy: ", accuracy, "\n")

      accuracy_train[epoch * iter] <- accuracy
      if (iter == 1 || iter %% (num_iter %/% 10) == 0) {
        accuracy_validate[epoch * iter] <- evaluate(validate_data, validate_target, model)
      }

      error <- (prediction - t) / nrow(x)
      back_pass <- backprop(model, error, forward_pass)
      model <- update(model, back_pass, alpha)
    }
  }

  if (plot_acc) {
    plot_accuracy(accuracy_train, accuracy_validate)
  }

  return(model)
}

#' Evaluate Model
#'
#' Evaluates the performance of a model on a given dataset.
#'
#' @param inputs set of inputs to the model
#' @param target set of targets in one-hot encoded form
#' @param model list of weights and biases
#'
#' @return accuracy of the model
#'
#' @export
#'
#' @examples
#' \dontrun{
#' evaluate(train_data, train_target, mlp_model)
#' }
evaluate <- function(inputs, target, model) {
  inputs <- as.matrix(as.data.frame(inputs))
  target <- as.matrix(as.data.frame(target))

  n <- nrow(inputs)
  forward_pass <- forwardprop(model, inputs)
  prediction <- softmax(forward_pass["y"])

  ce <- -sum(target * log(prediction)) / n
  accuracy <- sum(max.col(prediction) == max.col(target)) / n
  cat("Evaluated Cross Entropy: ", ce, "Evaludated Accuracy: ", accuracy, "\n")

  return(accuracy)
}
