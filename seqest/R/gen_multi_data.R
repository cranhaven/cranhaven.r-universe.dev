#' @title Generate the training data and testing data for the categorical and
#'   ordinal case.
#'
#' @description
#' \code{gen_multi_data} generate the data used for multiple-class
#' classification problems.
#'
#' @details
#' gen_multi_data creates training dataset and testing datasets. The beta0 is a
#' p * k matrix which p is the length of true coefficient and (k + 1) represents
#' the number of categories. The value of 'type' can be 'ord' or 'cat' . If it
#' equals to 'ord', it means the data has an ordinal relation among classes
#' ,which is common in applications (e.g., the label indicates the severity of a
#' disease or product preference). If it is 'cat', it represents there is no
#' such ordinal relations among classes. In addition, the response variable y
#' are then generated from a multinomial distribution with the explanatory
#' variables x generated from a multivariate normal distribution with mean
#' vector equal to 0 and the identity covariance matrix.
#' @param beta0 A numeric matrix that represent the true coefficient that used
#'   to generate the synthesized data.
#' @param N  A numeric number specifying the number of the synthesized data. It
#'   should be a integer. Note that the value shouldn't be too small. We
#'   recommend that the value be 10000.
#' @param type A character string that determines which type of data will be
#'   generated, matching one of 'ord' or 'cat'.
#' @param test_ratio A numeric number specifying proportion of test sets in all
#'   data. It should be a number between 0 and 1. Note that the value of the
#'   test_ratio should not be too large, it is best if this value is equal to
#'   0.2-0.3.
#' @export
#' @return a list containing the following components
#' \item{train_id}{The id of the training samples}
#' \item{train}{the training datasets. Note that the id of the data in the train
#' dataset is the same as the train_id}
#' \item{test}{the testing datasets}
#'
#'
#' @references {
#' Li, J., Chen, Z., Wang, Z., & Chang, Y. I. (2020). Active learning in
#' multiple-class classification problems via individualized binary models.
#' \emph{Computational Statistics & Data Analysis}, 145, 106911.
#' doi:10.1016/j.csda.2020.106911
#' }
#'
#' @seealso{
#'    \code{\link{gen_bin_data}} for binary classification case
#'
#'    \code{\link{gen_GEE_data}} for generalized estimating equations case.
#'
#'}
#'
#' @examples
#'# For an example, see example(seq_ord_model)


gen_multi_data <- function(beta0, N, type, test_ratio) {
  beta_mat <- beta0
  p <- dim(beta_mat)[1]
  nClass <- dim(beta_mat)[2]
  X <- MASS::mvrnorm(N, rep(0, p-1), 1*diag(p-1))
  X <- cbind(rep(1, N), X)
  tmp <- X %*% beta_mat
  if(type=="ord"){
    tmp <- t(apply(tmp, 1, cumsum))
  }
  mProb <- apply(tmp, 1, function(z) {
    vprob <- exp(z) / (1 + sum(exp(z)));
    matrix(c(1 - sum(vprob), vprob), nrow = 1) })
  Y_1hot <- apply(mProb, 2, function(vprob) rmultinom(1, 1, vprob))
  Y <- apply(Y_1hot, 2, function(yi) which(yi == 1)) - 1
  data_mat <- cbind(Y, X)
  train_id <- sample(N, (1-test_ratio)*N)
  train <- data_mat[train_id, ]
  test <- data_mat[-train_id, ]
  return(list(train_id = train_id, train = train, test = test))
}


