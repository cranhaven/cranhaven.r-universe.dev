#' @title The sequential logistic regression model for multi-classification
#'   problem under the categorical case.
#'
#' @description
#' \code{seq_cat_model} chooses the subjects sequentially by the logistic
#' regression model for the categorical case.
#'
#' @details
#' seq_cat_model is a multinomial logistic regression model that estimate the
#' coefficient of the explanatory variables and determines the samples
#' sequentially from original training data set given the fixed size confidence
#' set under the categorical case. Note that there are two methods to select the
#' samples. One sampling method is random sampling while another is the
#' A-optimality criterion which seeks to minimize the trace of the inverse of
#' the information matrix. In addition, we will use the special model: the
#' individualized binary logistic regression. We will use the specific model to
#' only fit two subgroups of the all dataset and get the estimated coefficient
#' and decide whether to stop sampling. If it shows that we need to continue, we
#' will use one of the samepling method above to pick the sample. Note that if
#' the method is A-optimality, we will pick the most informative subjects.
#' @param labeled_ids A numeric vector for the unique identification of the
#'   labeled dataset.
#' @param unlabeled_ids A numeric vector for the unique identification of the
#'   unlabeled dataset.
#' @param splitted A list containing the datasets which we will use in the
#'   categorical case. Note that the element of the splitted is the collections
#'   of samples from Classes 0 and Classes k.
#' @param newY A numeric number denotes the value of the labels from 0 to K
#'   which is the number of categories.
#' @param train A matrix for the labeled samples. Note that the indices of the
#'   samples in the train dataset is the same as the labeled_ids.
#' @param data  A matrix denotes all the data including the labeled samples and
#'   the unlabeled samples. Note that the first column of the dataset is the
#'   response variable, that's the labels and the rest is the explanatory
#'   variables.
#' @param d  A numeric number specifying the length of the fixed size confidence
#'   set for our model. The default value is 0.8.
#' @param adaptive A character string that determines the sample selection
#'   criterion to be used, matching one of 'random' or 'A_optimal The default
#'   value is 'random'.
#' @export
#' @return a list containing the following components
#' \item{d}{the length of the fixed size confidence set that we specify}
#' \item{n}{the current sample size when the stopping criterion is satisfied}
#' \item{is_stopped}{the label of sequential iterations stop or not. When the
#' value of is_stopped is TRUE, it means the iteration stops}
#' \item{beta_est}{the estimated coeffificent when the criterion is safisfied}
#' \item{cov}{the covariance matrix between the estimated parameters}
#' \item{adaptive}{the sample selection criterion we used }
#'
#' @references {
#' Li, J., Chen, Z., Wang, Z., & Chang, Y. I. (2020). Active learning in
#' multiple-class classification problems via individualized binary models.
#' \emph{Computational Statistics & Data Analysis}, 145, 106911.
#' doi:10.1016/j.csda.2020.106911
#' }
#'
#' @seealso{
#'    \code{\link{seq_GEE_model}} for generalized estimating equations case
#'
#'    \code{\link{seq_bin_model}} for binary classification case
#'
#'    \code{\link{seq_ord_model}} for ordinal case.
#'
#'}
#'
#' @examples
#' # generate the toy example
#' beta <- matrix(c(1,2,1,-1,1,2), ncol=2)
#' res <-  gen_multi_data(beta, N = 10000, type = 'cat', test_ratio = 0.3)
#' train_id <- res$train_id
#' train <- res$train
#' test <- res$test
#' res <- init_multi_data(train_id, train, init_N = 300, type = 'cat')
#' splitted <- res$splitted
#' train <- res$train
#' newY <- res$newY
#' labeled_ids <- res$labeled_ids
#' unlabeled_ids <- res$unlabeled_ids
#' data <- res$data
#'
#' # use seq_cat_model to multi-classification problem under the categorical case.
#' # You can remove '#' to run the command.
#' # start_time <- Sys.time()
#' # logitA_cat <- seq_cat_model(labeled_ids, unlabeled_ids, splitted, newY,
#' #                             train, data, d = 0.5, adaptive = "A_optimal")
#' # logitA_cat$time <- as.numeric(Sys.time() - start_time, units = "mins")
#' # print(logitA_cat)


seq_cat_model <- function(labeled_ids, unlabeled_ids, splitted, newY, train, data, d = 0.8, adaptive="random") {
  i <- length(labeled_ids)
  while (TRUE) {
    model <- function(df) glm(Y ~ . -1, data = df, family = "binomial")
    if (newY == 0) {
      models <- lapply(splitted, model)
      beta_list <- lapply(models, `[[`, "coefficients")
      p <- length(beta_list[[1]])
      beta_mat <- matrix(unlist(beta_list), nrow = p)
    } else {
      models <- model(splitted[[newY]])
      beta_mat[, newY] <- models[["coefficients"]]
    }
    cov <- getWH(train, beta_mat)
    eigen_min <- cov$eigen_min
    K <- dim(beta_mat)[2]
    p <- dim(beta_mat)[1]
    Kp <- K * p
    d <- d
    is_stopped <- (eigen_min > qchisq(df=Kp, 0.95) / d^2)
    if (!is_stopped) {
      if (adaptive == "random") {
        ind <- sample(1:length(unlabeled_ids), 1)
      }
      if (adaptive == "A_optimal") {
        ind <- A_optimal_cat(data[,-1], beta_mat,
                             cov$W, unlabeled_ids)
      }
      res <- update_data_cat(ind, splitted, data, train, labeled_ids, unlabeled_ids)
      splitted <- res$splitted
      newY <- res$newY
      train <- res$train
      labeled_ids <- res$labeled_ids
      unlabeled_ids <- res$unlabeled_ids
      i <- i + 1
    } else {
      results <- list(
                  d = d,
                  n = i,
                  is_stopped = is_stopped,
                  beta_est = beta_mat,
                  cov        =   cov$inv_sigma,
                  adaptive = adaptive)
      class(results) <- c('seqmulti','list')
      return(results)

      }
  }
}
