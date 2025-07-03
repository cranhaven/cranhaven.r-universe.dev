#' Load Training Data
#'
#' Loads MNIST training, validation, and test data and generates one hot
#' encodings for the targets. The test set proportion is not specified and is
#' instead the remainder from the test and validation proportions.
#'
#' @param train_prop proportion of the data used for the training set
#' @param validate_prop proportion of the data used for the validation set
#'
#' @return list of training and validation data and targets
#' @export
#'
#' @examples
#' \donttest{
#' mnist <- load_mnist(0.8, 0.1)
#' train_data <- mnist[1]
#' train_target <- mnist[2]
#' validate_data <- mnist[3]
#' validate_target <- mnist[4]
#' test_data <- mnist[5]
#' test_target <- mnist[6]
#' }
load_mnist <- function(train_prop=0.8, validate_prop=0.1) {
  if (
    train_prop < 0 ||
    train_prop > 1 ||
    validate_prop < 0 ||
    validate_prop > 1 ||
    train_prop + validate_prop > 1
  ) {
    return("Error: Train and validation data proportion must be between 0 and 1")
  }
  mnist <- readr::read_csv("https://pjreddie.com/media/files/mnist_train.csv", col_names = FALSE)
  mnist <- sample(mnist)  # shuffle raw data

  # index to split training and validation
  train_index <- train_prop * nrow(mnist)
  validate_index <- validate_prop * nrow(mnist)

  # split raw data into train and validate sets
  train_raw <- mnist[1:train_index,]
  validate_raw <- mnist[train_index:validate_index,]
  test_raw <- mnist[validate_index:nrow(mnist),]

  # training data and targets
  train_data <- train_raw[,-1]
  train_target <- one_hot_encoding(train_raw)

  # validation data and targets
  validate_data <- validate_raw[,-1]
  validate_target <- one_hot_encoding(validate_raw)

  # test data and targets
  test_data <- test_raw[,-1]
  test_target <- one_hot_encoding(test_raw)

  return(list(train_data, train_target, validate_data, validate_target, test_data, test_target))
}

#' One Hot Encoding
#'
#' Creates a one hot encoding matrix with the specified number of categories
#' for the targets. Target must be the first column of the data_raw input.
#'
#' @param data_raw data input to create encoding; target must be first column
#' @param ncat number of categories to use for the encoding
#'
#' @return targets in a one hot encoding matrix
one_hot_encoding <- function(data_raw, ncat=10) {
  n = nrow(data_raw)
  target <- data.frame(matrix(rep(0, n * ncat), ncol=ncat))

  for (i in 1:n) {
    target[i, data_raw$X1[[i]] + 1] = 1
  }

  return(target)
}
