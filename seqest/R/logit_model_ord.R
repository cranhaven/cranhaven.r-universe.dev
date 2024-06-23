#' @title the individualized binary logistic regression for ordinal response
#'   data.
#'
#' @description
#' \code{logit_model_ord} fit the ordinal data by the individualized binary
#' logistic regression
#'
#' @details
#' logit_model_ord fits the splitted data by using the the individualized binary
#' logistic regression according to the value of newY. Under the ordinal case,
#' we don't use the all training samples. Instead, we use two consecutive
#' subgroups, such as Classes k - 1 and k , at a time for each individual model.
#' Hence, we need fit the model acrroding to the value of newY. param splitted a
#' list containing the datasets which we will use in the cordinl case. Note that
#' the element of the splitted is the collections of samples from Classes 0k -
#' 1and Classes k.
#' @param splitted A list containing the datasets which we will use in the
#'   categorical case. Note that the element of the splitted is the collections
#'   of samples from Classes 0 and Classes k.
#' @param newY a numeric number denotes the value of the labels from 0 to K
#'   which is the number of categories
#' @param beta_mat the initial estimate for the coefficient. Note that the
#'   values may be not accurate.
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
#'    \code{\link{logit_model}} for categorical case.
#'
#'}
#'
#' @examples
#'## For an example, see example(seq_ord_model)


logit_model_ord <- function(splitted, newY, beta_mat) {
  model <- function(df) glm(Y ~ . -1, data = df, family = "binomial")
  # newY <- newY
  if (newY == 0) {
    models <- model(splitted[[1]])
    # params$beta_list[[newY]] <- models[["coefficients"]]
    beta_mat[, 1] <- models[["coefficients"]]
  }
  else if (newY == dim(beta_mat)[2]) {
    models <- model(splitted[[newY]])
    # params$beta_list[[newY]] <- models[["coefficients"]]
    beta_mat[, newY] <- models[["coefficients"]]
  }
  else {
    model1 <- model(splitted[[newY]])
    model2 <- model(splitted[[newY+1]])
    beta_mat[, c(newY, newY+1)] <- cbind(model1[["coefficients"]], model2[["coefficients"]])
  }
  return(beta_mat)
}
