#' @title Add the new sample into labeled dataset from unlabeled dataset for the
#'   categorical case
#'
#' @description
#' \code{update_data_cat} selects the sample to the labeled dataset according to
#' it's index
#'
#' @details
#' update_data_cat chooses the sample based on the index from all the training
#' dataset if the data has no ordinal relation.  Specifically, we remove the
#' index of the choosed sample from the unlabeled dataset and add the index to
#' the labeled dataset. Then, combine the selected sample with the existing
#' training data set.
#' @param ind A numeric value denotes the index of selected sample.
#' @param splitted A list containing the datasets which we will use in the
#'   categorical case. Note that the element of the splitted is the collections
#'   of samples from Classes 0 and Classes k.
#' @param data A matrix  denotes all the data including the labeled samples and
#'   the unlabeled samples. Note that the first column of the dataset is the
#'   response variable, that's the labels and the rest is the explanatory
#'   variables.
#' @param train A matrix for the labeled samples.
#' @param labeled_ids A numeric vector for the unique identification of the
#'   labeled dataset
#' @param unlabeled_ids A numeric vector for the unique identification of the
#'   unlabeled dataset
#' @export
#' @return
#' \item{splitted}{a list containing the datasets which we add a new sample into
#' it}
#' \item{newY}{the label of the choosed sample}
#' \item{train}{the dataset used for training the model after adding the new
#' sample}
#' \item{labeled_ids}{the id of the labeled dataset after updating}
#' \item{unlabeled_ids}{the id of the unlabeled dataset after updating}
#'
#' @references {
#' Li, J., Chen, Z., Wang, Z., & Chang, Y. I. (2020). Active learning in
#' multiple-class classification problems via individualized binary models.
#' \emph{Computational Statistics & Data Analysis}, 145, 106911.
#' doi:10.1016/j.csda.2020.106911
#' }
#'
#' @seealso{
#'    \code{\link{update_data_ord}}
#'
#'}
#' @examples
#'## For an example, see example(seq_cat_model)


update_data_cat <- function(ind, splitted, data, train, labeled_ids, unlabeled_ids) {
  newdata <- data[unlabeled_ids[ind], ]
  newY <- newdata[1]
  unlabeled_ids <- unlabeled_ids[-ind]
  labeled_ids <- c(labeled_ids, ind)
  # cat("Add a new sample with label: ", newY, "\n")
  if (newY == 0) {
    train <- rbind(train, newdata)
    rownames(train) <- NULL
    nClass <- length(unique(train[, 1]))
    Y  <- train[, 1]
    X <- train[, -1]
    splitted <- lapply(seq_len(nClass-1), function(k) {
      ind <- (Y == 0 | Y == k);
      dataframe <- data.frame(cbind(Y=Y[ind], X[ind,]));
      dataframe$Y <- factor(dataframe$Y);
      dataframe})
  } else {
    train <- rbind(train, newdata)
    rownames(train) <- NULL
    splitted[[newY]] <- rbind(splitted[[newY]], newdata)
    rownames(splitted[[newY]]) <- 1:dim(splitted[[newY]])[1]
  }
  return(list(splitted = splitted, newY = newY, train = train,
              labeled_ids = labeled_ids, unlabeled_ids = unlabeled_ids))
}
