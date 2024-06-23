#' @title the individualized binary logistic regression for categorical response
#'   data.
#'
#' @description
#' \code{logit_model} fit the categorical data by the individualized binary
#' logistic regression
#'
#' @details
#' logit_model fits the splitted data by using the the individualized binary
#' logistic regression according to the value of newY. Because we use use Class
#' 0 as the baseline for modeling the probability ratio of Class k to Class 0 by
#' fitting K individual logistic models, if newY equal to 0, it means we need
#' fit all elements of the splitted data. Otherwise, we only fit the samples
#' from class 0 and class newY.
#' @param splitted A list containing the datasets which we will use in the
#'   categorical case. Note that the element of the splitted is the collections
#'   of samples from Classes 0 and Classes k.
#' @param newY A numeric number denotes the value of the labels from 0 to K
#'   which is the number of categories
#' @export
#' @return
#' \item{beta_mat}{a matrix contains the estimated coefficient. Note that the
#' beta_mat is a n * p matrix which n is the number of the explanatory variables
#' and p+1 is the number of categories}
#'
#' @references {
#' Li, J., Chen, Z., Wang, Z., & Chang, Y. I. (2020). Active learning in
#' multiple-class classification problems via individualized binary models.
#' \emph{Computational Statistics & Data Analysis}, 145, 106911.
#' doi:10.1016/j.csda.2020.106911
#' }
#'
#' @seealso{
#'    \code{\link{logit_model_ord}} for ordinal case.
#'
#'}
#'
#'
#' @examples
#'## For an example, see example(seq_cat_model)

logit_model <- function(splitted, newY) {
  model <- function(df) glm(Y ~ . -1, data = df, family = "binomial")
  # newY <- data_env$newY
  if (newY == 0) {
    models <- lapply(splitted, model)
    beta_list <- lapply(models, `[[`, "coefficients")
    p <- length(beta_list[[1]])
    beta_mat <- matrix(unlist(beta_list), nrow = p)
  } else {
    models <- model(splitted[[newY]])
    # params$beta_list[[newY]] <- models[["coefficients"]]
    beta_mat[, newY] <- models[["coefficients"]]
  }
  return(beta_mat)
}
