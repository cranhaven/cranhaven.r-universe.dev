#' @title Add the new sample into labeled dataset from unlabeled dataset for the
#'   ordinal case
#'
#' @description
#' \code{update_data_ord} selects the sample to the labeled dataset according to
#' it's index
#'
#' @details
#' update_data_ord chooses the sample based on the index from all the training
#' ordinal dataset. We record the corresponding label of the selected sample and
#' update the data of the unlabeled dataset and the labeled dataset.
#' Specifically, we remove the index of the choosed sample from the unlabeled
#' dataset and add the sample to the labeled dataset.
#' @param ind A numeric value denotes the index of selected sample.
#' @param splitted A list containing the datasets which we will use in the
#'   cordinl case. Note that the element of the data_split is the samples from
#'   Classes k-1and Classes k
#' @param data A matrix  denotes all the data including the labeled samples and
#'   the unlabeled samples. Note that the first column of the dataset is the
#'   response variable, that's the labels and the rest is the explanatory
#'   variables.
#' @param train A matrix for the labeled samples.
#' @param labeled_ids The unique identification of the labeled dataset
#' @param unlabeled_ids The unique identification of the unlabeled dataset
#' @export
#' @return
#' \item{splitted}{a list containing the new datasets which we add a new sample
#' into it}
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
#'    \code{\link{update_data_cat}}
#'
#'}
#'
#' @examples
#'## For an example, see example(seq_ord_model)


update_data_ord <- function(ind, splitted, data, train, labeled_ids, unlabeled_ids) {
  newdata <- data[unlabeled_ids[ind], ]
  newY <- newdata[1]
  K <- length(splitted)
  unlabeled_ids <- unlabeled_ids[-ind]
  labeled_ids <- c(labeled_ids, ind)
  # cat("Add a new sample with label: ", newY, "\n")
  train <- rbind(train, newdata)
  rownames(train) <- NULL
  if (newY != 0) {
    splitted[[newY]] <- rbind(splitted[[newY]], newdata)
    rownames(splitted[[newY]]) = 1:dim(splitted[[newY]])[1]
  }

  if (newY != K) {
    splitted[[newY + 1]] <- rbind(splitted[[newY + 1]], newdata)
    rownames(splitted[[newY + 1]]) = 1:dim(splitted[[newY + 1]])[1]
  }
  newY <- newY
  return(list(splitted = splitted, newY = newY, train = train,
              labeled_ids = labeled_ids, unlabeled_ids = unlabeled_ids))
}
