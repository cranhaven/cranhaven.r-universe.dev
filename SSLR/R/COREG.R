
#' @title General Interface for COREG model
#' @description COREG is a semi-supervised learning for regression with a co-training style.
#' This technique uses two kNN regressors with different distance metrics.
#' For each iteration, each regressor labels the unlabeled
#' example which can be most confidently labeled for the other
#' learner, where the labeling confidence is estimated through
#' considering the consistency of the regressor with the labeled
#' example set. The final prediction is made by averaging the
#' predictions of both the refined kNN regressors
#' @details labeling data is very expensive computationally. Its so slow. For executing this model, we need RANN installed.
#' @param max.iter maximum number of iterations to execute the self-labeling process.
#' Default is 50.
#' @param k1 parameter in first KNN
#' @param k2 parameter in second KNN
#' @param p1 distance order 1. Default is 3
#' @param p2 distance order 1. Default is 5
#' @param u Number of unlabeled instances in the pool. Default is 100.
#' @references
#' Zhi-Hua Zhou and Ming Li.\cr
#' \emph{Semi-Supervised Regression with Co-Training.}\cr
#' National Laboratory for Novel Software Technology
#' Nanjing University, Nanjing 210093, China
#' @importFrom magrittr %>%
#' @importFrom RANN nn2
#' @example demo/COREG.R
#' @export
COREG <- function(
  max.iter = 50,
  k1 = 3,
  k2 = 5,
  p1 = 3,
  p2 = 5,
  u = 100
) {

  if(p1 == p2){
    rlang::abort("P1 can not be equal to p2")
  }

  # Check max.iter
  if (max.iter < 1) {
    stop("Parameter max.iter is less than 1. Expected a value greater than and equal to 1.")
  }

  train_function <- function(x, y) {

    load_parsnip()
    load_RANN()

    x <- as.data.frame(x)
    y <- as.numeric(y)


    # Obtain the indexes of labeled and unlabeled instances
    labeled <- which(!is.na(y))
    unlabeled <- which(is.na(y))

    labeled.old <- labeled

    if (length(labeled) == 0) {
      # labeled is empty
      stop("The labeled set is empty. All the values in y parameter are NA.")
    }
    if (length(unlabeled) == 0) {
      # unlabeled is empty
      stop("The unlabeled set is empty. None value in y parameter is NA.")
    }

    #COREG ALGORITHM
    l1 <- labeled
    l2 <- labeled

    pool_U <- NULL

    h1 <- NULL
    h2 <- NULL


    for(t in 1:max.iter){

      #Create pool of unlabeled data
      pool_U <- sample(x = unlabeled, size = min(u, length(unlabeled)), replace = FALSE)

      #Create models
      h1 <- knn_regression(k = k1,x[l1,,drop = FALSE],y[l1],p1)
      h2 <- knn_regression(k = k2,x[l2,,drop = FALSE],y[l2],p2)

      h1_no_change <- FALSE
      h2_no_change <- FALSE

      #Iterate in models
      for(j in 1:2){

        #Get current model and data
        h_temp <- NULL
        k_temp <- NULL
        l_temp <- NULL
        p_temp <- NULL

        if(j == 1){
          h_temp <- h1
          k_temp <- k1
          l_temp <- l1
          p_temp <- p1
        }

        else{
          h_temp <- h2
          k_temp <- k2
          l_temp <- l2
          p_temp <- p2
        }

        #Create deltas vector (zeros)
        deltas <- rep(0, length(pool_U))

        #Iterate in pool of unlabeled data
        for(i in 1:length(pool_U)){

          #Actual x_index from unlabeled
          x_u_index <- pool_U[i]

          #Predict actual model
          yu <- h_temp %>% predict(x[x_u_index,,drop = FALSE]) %>% as.numeric()

          #Get neighbors
          omega <- nn2(x[l_temp,,drop = FALSE],x[x_u_index,,drop = FALSE],k = k_temp)$nn.idx %>%
            as.numeric() %>% unique()

          #Index labeled data and x_u from current model
          labeled_index_pool <- c(l_temp,x_u_index)

          #Update current y
          Y_temp <- y
          Y_temp[x_u_index] <- yu

          X_temp <- x[labeled_index_pool, , drop = FALSE]
          Y_temp <- Y_temp[labeled_index_pool]

          #Get model with new data
          h_pool_index <- knn_regression(k_temp,X_temp,Y_temp,p_temp)

          #Get delta
          delta <- compute_delta(omega,X_temp,Y_temp,h_temp,h_pool_index)

          deltas[i] <- delta

        }#End for actual pool

        #Get max delta
        idx_max_delta <-  which.max(deltas)

        pi_temp <- c()

        #If > 0
        if(deltas[idx_max_delta] > 0){

          #Get index of unlabeled data
          index_pool <- pool_U[idx_max_delta]

          #Predict value with index of unlabeled data
          yj <- h_temp %>% predict(x[index_pool,]) %>% as.numeric()

          #Update y with predict
          y[index_pool] <- yj

          #Update unlabeled in pool not to repeat
          pool_U <- setdiff(pool_U,c(index_pool))

          #Update pi
          pi_temp <- index_pool
        }

        #Not exists, l1 or l2 not change
        else{
          pi_temp <- c()

          if(j == 1){
            h1_no_change <- TRUE
          }
          else{
            h2_no_change <- TRUE
          }
        }

        #Add new index to labeled data in l1 or l2
        if(j == 1){
          l1 <- unique(c(l1,pi_temp))
        }

        else{
          l2 <- unique(c(l2,pi_temp))
        }


      }#End for models

      #if neither of L1 and L2 changes then exit
      if(h1_no_change && h2_no_change)
        break

      #Delete in unlabeled index
      else{
        unlabeled <- setdiff(unlabeled,l1)
        unlabeled <- setdiff(unlabeled,l2)
      }


    }#End loop max.iter


    result <- list(
      h1 = h1,
      h2 = h2,
      pred.params = c("numeric","raw"),
      instances.index = labeled.old,
      mode = "regression"
    )

    class(result) <- "COREG"

    return(result)

  }

  args <- list(
    max.iter = max.iter,
    k1 = k1,
    k2 = k2,
    p1 = p1,
    p2 = p2,
    u = u
  )

  new_model_sslr(train_function, "COREG", args)
}


#' @title knn_regression
#' @description create model knn
#' @param k parameter in KNN model
#' @param x data
#' @param y vector labeled data
#' @param p distance order
#' @importFrom parsnip nearest_neighbor
#' @importFrom parsnip set_engine
knn_regression <- function(k,x,y,p){
  nearest_neighbor(neighbors = k, mode = "regression", dist_power = p) %>%
    set_engine("kknn") %>% parsnip::fit_xy(x,y)
}

compute_delta <- function(omega,L_X,L_Y,h,h_temp){

  delta <- 0

  for(idx in omega){

    delta = delta +
      (as.numeric(L_Y[idx] - as.numeric(h %>% predict(L_X[idx,])))) ** 2


    delta = delta -
      (as.numeric(L_Y[idx] - as.numeric(h_temp %>% predict(L_X[idx,])))) ** 2
  }

  delta
}



#' @title Predictions of the COREG method
#' @description Predicts the label of instances according to the \code{COREG} model.
#' @details For additional help see \code{\link{COREG}} examples.
#' @param object Self-training model built with the \code{\link{COREG}} function.
#' @param x A object that is data
#' @param ... This parameter is included for compatibility reasons.
#' @param type of predict in principal model (numeric)
#' @method predict COREG
#' @return Vector with the labels assigned (numeric).
#' @importFrom stats predict
#' @importFrom magrittr %>%
predict.COREG <- function(object, x, type = "numeric", ...) {

  #With the format of chosen model
  result1 <- object$h1 %>% predict(x, type = type)
  result2 <- object$h2 %>% predict(x, type = type)

  rowMeans(cbind(result1,result2))
}

