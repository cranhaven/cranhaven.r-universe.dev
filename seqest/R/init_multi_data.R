#' @title Generate the labeled and unlabeled datasets
#'
#' @description
#' \code{init_multi_data} creates the labeled and unlabeled datasets for the
#' categorical and ordinal case.
#'
#' @details
#' init_multi_data generates the initial labeled dataset and the unlabeled
#' datasets which we will select a most informative sample from the unlabeled
#' datasets into the labeled dataset. The number of samples in the initial
#' labeled datasets is specified the init_N argument. The value of 'type' should
#' be'ord' or 'cat'. If it equals to 'ord', the element of the splitted will be
#' composed of samples from Classes K and Classes K+1. Otherwise, the element of
#' the splitted will be composed of samples from  Classes 0 and Classes K.
#' @param train_id A numeric vector denotes the id of the all training samples.
#'   Each sample corresponds to a unique identification from 1 to the length of
#'   all the samples.
#' @param train A numeric matrix denote the training datasets. The length of the
#'   train's row is the number of the training samples and the first column
#'   represents the labels and the rest columns are the explanatory variables.
#'   Note that the id of the sample in the train dataset is the same as the
#'   train_id.
#' @param init_N A numeric value that determine the number of the initial
#'   labeled samples. Note that it shouldn't be too large or too small.
#' @param type A character string that determines which type of data will be
#'   generated, matching one of 'ord' or 'cat'.
#' @export
#' @return a list containing the following components
#' \item{splitted}{a list containing the datasets which we will use}
#' \item{train}{the initial labeled datasets. The number of the datasets is
#' specified by the init_N}
#' \item{newY}{the value of the labels from 0 to K which denotes the number of
#' categories}
#' \item{labeled_ids}{the unique id of the initial labeled dataset }
#' \item{unlabeled_ids}{the unique id of the unlabeled
#' dataset}
#' \item{data}{the all training samples which is composed of the samples
#' corresponding to labeled_ids and samples corresponding to unlabeled_ids}
#'
#'
#' @examples
#'## For an example, see example(seq_ord_model)

init_multi_data <- function(train_id, train, init_N, type) {
  data <- train
  index <- 1:nrow(data) #1,2,3,4...
  # index <- train_id
  init_ids <- sample(index, init_N) # init_N = 300
  # index <- 1:params$N
  labeled_ids <- init_ids
  unlabeled_ids <- index[-labeled_ids]
  # unlabeled_ids <- setdiff(index, labeled_ids)
  train <- data[labeled_ids, ]
  if(type=="cat"){
    nClass <- length(unique(train[, 1]))
    Y  <- train[, 1]
    X <- train[, -1]
    splitted <- lapply(seq_len(nClass-1), function(k) {
      ind <- (Y == 0 | Y == k);
      dataframe <- data.frame(cbind(Y=Y[ind], X[ind,]));
      dataframe$Y <- factor(dataframe$Y);
      dataframe})
  } else {
    nClass <- length(unique(train[, 1]))
    Y  <- train[, 1]
    X <- train[, -1]
    splitted <- lapply(seq_len(nClass-1), function(k) {
      ind <- (Y == k-1 | Y == k);
      dataframe <- data.frame(cbind(Y=Y[ind], X[ind,]));
      dataframe$Y <- factor(dataframe$Y);
      dataframe})
  }
  newY <- 0
  return(list(splitted = splitted, train = train, newY = newY,
              labeled_ids = labeled_ids, unlabeled_ids = unlabeled_ids, data = data))
}
