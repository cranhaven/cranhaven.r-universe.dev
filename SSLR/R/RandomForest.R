#' @title Class Random Forest
#' @description Class Random Forest
#' Slots: mtry, trees, min_n, w, classes, mode
setClass("RandomForestSemisupervised",
         slots = c(
           mtry = "numeric",
           trees = "numeric",
           min_n = "numeric",
           trees_list = "list",
           w = "numeric",
           classes = "character",
           mode = "character"
         ),
         prototype = list(
           mtry = NA_integer_,
           trees = NA_integer_,
           min_n = NA_integer_,
           w = NA_integer_,
           trees_list = list(),
           classes = NA_character_,
           mode = "classification"
         )
)

setGeneric(name="fit_random_forest",
           def=function(object,...)
           {
             standardGeneric("fit_random_forest")
           }
)

#' @title Fit Random Forest
#' @description method in classRandomForestSemisupervised used to build a Decision Tree
#' @param object A RandomForestSemisupervised object
#' @param X A object that can be coerced as data.frame. Training instances
#' @param y A vector with the labels of the training instances. In this vector
#' the unlabeled instances are specified with the value \code{NA}.
#' @param mtry number of features in each decision tree
#' @param trees number of trees. Default is 5
#' @param min_n number of minimum samples in each tree
#' @param w weight parameter ranging from 0 to 1
#' @param replace replacing type in sampling
#' @param tree_max_depth maximum tree depth. Default is Inf
#' @param sampsize Size of sample. Default if (replace) nrow(x) else ceiling(.632*nrow(x))
#' @param min_samples_leaf the minimum number of any terminal leaf node
#' @param allowParallel Execute Random Forest in parallel if doParallel is loaded.
#' Default is TRUE
#' @return list of decision trees
setMethod(f="fit_random_forest",
          signature="RandomForestSemisupervised",
          definition=function(object,X,y, mtry = 2, trees = 500,
                              min_n = 2 , w = 0.5, replace = TRUE,
                              tree_max_depth = Inf,
                              sampsize = if (replace) nrow(X) else ceiling(.632*nrow(X)),
                              min_samples_leaf = if (!is.null(y) && !is.factor(y)) 5 else 1,
                              allowParallel = TRUE
          )
          {

            #If mtry is greater than num features, stop
            if(mtry > ncol(X)){
              stop("MTRY CAN NOT BE GREATER THAN ATTTRIBUTES X")
            }

            #If min_n is below 1, stop
            if(min_n <= 0){
              stop("MIN_N CAN NOT BE UNDER 1")
            }

            #If trees is below 2, stop. One is a simple Decision tree
            if(trees < 2){
              stop("MIN_N CAN NOT BE UNDER 2")
            }

            #W between 0 and 1
            if(w < 0 | w > 1){
              stop("W CAN BE BETWEEN [0,1]")
            }

            list_trees <- list()

            #Assign values in object
            object@mtry <- mtry
            object@trees <- trees
            object@min_n <- min_n
            object@w <- w

            if(is.factor(y))
              object@classes <- levels(y)

            else if(is.numeric(y))
              object@mode = "regression"


            list_trees <- list()


            if(allowParallel & get_doParallel_loaded()){


              list_trees <- get_list_tree_parallel(X = X, y = y, mtry = mtry,
                                                   trees = trees,
                                                   min_n = min_n, w = w, replace = replace,
                                                   tree_max_depth = tree_max_depth,
                                                   sampsize = sampsize,
                                                   min_samples_leaf = min_samples_leaf)
            }


            else{

              list_trees <- get_list_tree_sequential(X = X, y = y, mtry = mtry,
                                                     trees = trees,
                                                     min_n = min_n, w = w, replace = replace,
                                                     tree_max_depth = tree_max_depth,
                                                     sampsize = sampsize,
                                                     min_samples_leaf = min_samples_leaf)
            }


            object@trees_list <- list_trees

            return(object)
          }
)

#' @title Get most frequented
#' @description Get value most frequented in vector
#' Used in predictions
#' @param elements vector with values
#' @importFrom utils tail
get_most_frequented <- function(elements){
  tail(names(sort(table(elements))),1)
}

#' @title Get most frequented
#' @description Get value most frequented in vector
#' Used in predictions. It calls a predict with type = "prob"
#' in Decision Tree
#' @param trees trees list
#' @param input is input to be predicted
#' @export
get_class_max_prob <- function(trees,input){

  #Function to use in apply
  #Call predict in decision tree
  predict_numeric <- function(x,input,type){
    as.numeric(predict(x,input,type))
  }

  #Get probabilities by input in predict decision tree
  tprobs <- t(sapply(trees,predict_numeric,input,type="prob"))

  #Sum probabilities by class
  sum_probabilities <- apply(tprobs,2,sum, na.rm = TRUE)

  #Get the class with most sum probability
  trees[[1]]@classes[which.max(sum_probabilities)]
}


#' @title Get mean probability over all trees as prob vector
#' @description Get mean probability over all trees as prob vector.
#' It calls a predict with type = "prob"
#' in Decision Tree
#' @param trees trees list
#' @param input is input to be predicted
#' @export
get_class_mean_prob <- function(trees,input){

  #Function to use in apply
  #Call predict in decision tree
  predict_numeric <- function(x,input,type){
    as.numeric(predict(x,input,type))
  }

  #Get probabilities by input in predict decision tree
  tprobs <- t(sapply(trees,predict_numeric,input,type="prob"))

  # return mean probs over all trees and return class-size vector
  apply(tprobs,2,mean, na.rm = TRUE)
}


#' @title Get value mean
#' @description Get value most frequented in vector
#' Used in predictions. It calls a predict with type = "numeric"
#' in Decision Tree
#' @param trees trees list
#' @param input is input to be predicted
get_value_mean <- function(trees,input){

  #Function to use in apply
  #Call predict in decision tree
  predict_numeric <- function(x,input,type){
    as.numeric(predict(x,input,type))
  }

  #Get values by input in predict decision tree
  values <- t(sapply(trees,predict_numeric,input,type="numeric"))

  #Mean value
  mean_value <- apply(values,1,mean, na.rm = TRUE)

  mean_value
}


#' @title General Interface Random Forest model
#' @description Random Forest is a simple and effective semi-supervised
#' learning method. It is the same as the traditional Random Forest
#' algorithm, but the difference is that it use Semi supervised Decision Trees
#' It can be used in classification or regression. If Y is numeric is for regression, classification in another case
#' @details We can use paralleling processing with doParallel package and allowParallel = TRUE.
#' @param mtry number of features in each decision tree.
#' Default is null. This means that mtry = log(n_features) + 1
#' @param trees number of trees. Default is 500
#' @param min_n number of minimum samples in each tree
#' Default is null. This means that uses all training data
#' @param w weight parameter ranging from 0 to 1. Default is 0.5
#' @param replace replacing type in sampling. Default is true
#' @param tree_max_depth maximum tree depth. Default is Inf
#' @param sampsize Size of sample. Default if (replace) nrow(x) else ceiling(.632*nrow(x))
#' @param min_samples_leaf the minimum number of any terminal leaf node. Default is 1
#' @param allowParallel Execute Random Forest in parallel if doParallel is loaded.
#' Default is TRUE
#' @references
#' Jurica Levati, Michelangelo Ceci, Dragi Kocev, Saso Dzeroski.\cr
#' \emph{Semi-supervised classification trees.}\cr
#' Published online: 25 March 2017
#' © Springer Science Business Media New York 2017
#' @example demo/RandomForest.R
#' @importFrom methods new
#' @export
SSLRRandomForest <- function(mtry = NULL, trees = 500, min_n = NULL, w = 0.5, replace = TRUE,
                             tree_max_depth = Inf,sampsize = NULL,
                             min_samples_leaf = NULL, allowParallel = TRUE){

  train_function <- function(x,y){

    if(is.null(sampsize)){
      sampsize <- if (replace) nrow(x) else ceiling(.632*nrow(x))
    }

    if(is.null(min_samples_leaf)){
      min_samples_leaf <- if (!is.null(y) && !is.factor(y)) 5 else 1
    }

    if(is.null(mtry))
      mtry = floor(log(ncol(x)) + 1)

    if(is.null(min_n))
      min_n = length(y)

    random <- new("RandomForestSemisupervised")
    random <- fit_random_forest(random,X = x,y = y, mtry = mtry,
                                trees =trees, min_n = min_n ,
                                w = w ,replace = replace,
                                tree_max_depth = tree_max_depth,
                                min_samples_leaf = min_samples_leaf,
                                sampsize = sampsize,allowParallel = allowParallel)


    result <- list(
      model = random
    )

    if(is.factor(y))
      result$classes = levels(y)

    if(is.factor(y) | is.character(y))
      result$mode = "classification"

    else
      result$mode = "regression"

    if(result$mode == "classification")
      result$pred.params = c("class","raw","prob")

    else
      result$pred.params = c("numeric","raw")


    class(result) <- "RandomForestSemisupervised_fitted"

    return(result)
  }

  args <- list(
    trees = trees,
    w = w,
    replace = replace,
    tree_max_depth = tree_max_depth,
    min_samples_leaf = min_samples_leaf,
    sampsize = sampsize,
    allowParallel = allowParallel
  )

  new_model_sslr(train_function, "SSLRRandomForest" ,args)

}

#' @title Function to predict inputs in Decision Tree
#' @description Function to predict inputs in Decision Tree
#' @param object The Decision Tree object
#' @param inputs data to be predicted
#' @param type class raw
#' @param confident Is param to define the type of predict.
#' It can be "max_prob", to get class with sum of probability is the maximum
#' Or "vote" to get the most frequented class in all trees.
#' Default is "max_prob"
#' @param allowParallel Execute Random Forest in parallel if doParallel is loaded.
#' @export
setMethod(f="predict",
          signature="RandomForestSemisupervised",
          definition=function(object,inputs,type = "class",
                              confident = "max_prob",
                              allowParallel = TRUE)
          {
            results <- c()

            if(object@mode == "classification" & is.null(confident)){
              confident <- "max_prob"
            }

            if(allowParallel & get_doParallel_loaded()){

              results <- predict_random_parallel(object,inputs,object@mode,confident)
            }

            else{
              results <- predict_random_sequential(object,inputs,object@mode,confident)
            }

            if(object@mode == "classification" & type == "class"){
              #we return tibble with values of vector
              results <- as.factor(results)
              levels(results) <- object@classes
            }


            else if(type == "prob"){
              colnames(results) <- object@classes
            }

            results
          }
)


#' @title Predictions of the SSLRDecisionTree_fitted method
#' @description Predicts the label of instances according to the RandomForestSemisupervised_fitted model.
#' @param object RandomForestSemisupervised_fitted.
#' @param x A object that can be coerced as matrix.
#' Depending on how was the model built, \code{x} is interpreted as a matrix
#' with the distances between the unseen instances and the selected training instances,
#' or a matrix of instances.
#' @param ... This parameter is included for compatibility reasons.
#' @param type of predict in principal model
#' @param confident Is param to define the type of predict.
#' It can be "max_prob", to get class with sum of probability is the maximum
#' Or "vote" to get the most frequented class in all trees.
#' Default is "max_prob"
#' @method predict RandomForestSemisupervised_fitted
#' @return Vector with the labels assigned.
#' @importFrom stats predict
#' @importFrom magrittr %>%
predict.RandomForestSemisupervised_fitted <- function(object, x, type = "class",
                                                      confident = "max_prob",...) {

  if(type == "raw" & object$mode == "classification")
    type <- "class"

  else if(type == "raw" & object$mode == "regression")
    type <- "numeric"

  if(type == "prob" & object$mode == "classification")
    #for prob predictions, we use the 'confident' method 'mean_prob'
    confident <- "mean_prob"

  #With the format of chosen model
  result <- object$model %>% predict(x, type = type, confident = confident)



  return(result)
}



#' @importFrom methods new
get_list_tree_sequential <- function(X,y, mtry, trees ,
                                     min_n, w, replace ,
                                     tree_max_depth,
                                     sampsize,
                                     min_samples_leaf){

  list_trees <- list()

  #Iterate in number of trees
  for(num_tree in 1:trees){

    #Get size of data train in tree
    #size will be a random number from min_n to sampsize
    size_data_train <- as.integer(runif(1, min_n, sampsize))


    #Get index attributes by sampling
    index_attributes <- sample(ncol(X), mtry, replace = FALSE)

    #Get index of sampling
    index_train <- sample(nrow(X), size_data_train, replace = replace)

    y_with_index <- y[index_train]

    #With labeled data
    while(length(y_with_index[!is.na(y_with_index)]) == 0){

      index_train <- sample(nrow(X), size_data_train, replace = replace)

      y_with_index <- y[index_train]

    }

    #Get train data
    train_data_X <- X[index_train,index_attributes]
    train_data_Y <- y[index_train]

    #Build Dacision Tree by class
    arbol <- new("DecisionTreeClassifier",max_depth=tree_max_depth)
    arbol <- fit_decision_tree(arbol,train_data_X,train_data_Y , w = w,
                               min_samples_split = 3 * min_samples_leaf,min_samples_leaf = min_samples_leaf)

    #Push Decision Tree in list
    list_trees <- append(list_trees,arbol)

  }


  list_trees

}

#' @importFrom foreach %dopar%
#' @importFrom methods new
get_list_tree_parallel<- function(X,y, mtry, trees ,
                                  min_n, w, replace ,
                                  tree_max_depth,
                                  sampsize,
                                  min_samples_leaf){


  list_trees <- list()

  #Iterate in number of trees
  list_trees <- foreach::foreach(num_tree = 1:trees,.combine = 'append') %dopar% {

    #Get size of data train in tree
    #size will be a random number from min_n to sampsize
    size_data_train <- as.integer(runif(1, min_n, sampsize))


    #Get index attributes by sampling
    index_attributes <- sample(ncol(X), mtry, replace = FALSE)

    #Get index of sampling
    index_train <- sample(nrow(X), size_data_train, replace = replace)

    y_with_index <- y[index_train]

    #With labeled data
    while(length(y_with_index[!is.na(y_with_index)]) == 0){

      index_train <- sample(nrow(X), size_data_train, replace = replace)

      y_with_index <- y[index_train]

    }

    #Get train data
    train_data_X <- X[index_train,index_attributes]
    train_data_Y <- y[index_train]

    #Build Dacision Tree by class
    #super funny, arbol=tree, granada hasta siempre!!!
    arbol <- new("DecisionTreeClassifier",max_depth=tree_max_depth)
    arbol <- fit_decision_tree(arbol,train_data_X,train_data_Y , w = w,
                               min_samples_split = 3 * min_samples_leaf,
                               min_samples_leaf = min_samples_leaf)

    arbol
  }


  list_trees


}



predict_random_sequential <- function(object,inputs,mode = "classification", type = "max_prob"){

  #We need to create a vector
  results <- c()

  #We need to iterate all inputs
  for(i in 1:nrow(inputs)){

    new_result <- NULL

    #If type is vote, we need the class most frequented in all trees
    if(type == "vote" & mode == "classification"){
      new_result <- get_most_frequented(sapply(object@trees_list,predict,inputs[i,, drop =FALSE]))
    }

    #If type is max_prob, we need the class with the most probability
    else if(type == "max_prob" & mode == "classification"){
      new_result <- get_class_max_prob(object@trees_list,inputs[i,, drop = FALSE])
    }

    #Prob predict
    else if(type == "mean_prob" & mode == "classification"){
      new_result <- get_class_mean_prob(object@trees_list,inputs[i,, drop = FALSE])
    }

    #Regression
    else if(mode == "regression"){
      new_result <- get_value_mean(object@trees_list,inputs[i,, drop = FALSE])
    }

    #Add result
    if (type=='mean_prob'){
      results <- rbind(results, new_result)
    } else {
      results <- c(results,new_result)
    }
  }

  if (type=='mean_prob'){
    rownames(results) <- c(1:nrow(results))
  }
  return(results)
}

#' @importFrom foreach %dopar%
predict_random_parallel <- function(object,inputs, mode = "classification",type = "max_prob"){

  i <- NULL

  #We need to iterate all inputs
  #in case of type='mean_prob', we need to .combine with 'rbind'
  if(type=='mean_prob'){
    results <- foreach::foreach(i = 1:nrow(inputs),.combine = 'rbind') %dopar% {

      new_result <- NULL
      #if type == mean_prob, we average probs over the trees
      new_result <- get_class_mean_prob(object@trees_list,inputs[i,, drop = FALSE])
      new_result
    }
  } else{
    #in case of type='mean_prob', we need to .combine with 'rbind'
    results <- foreach::foreach(i = 1:nrow(inputs),.combine = 'c') %dopar% {

      new_result <- NULL

      #If type is vote, we need the class most frequented in all trees
      if(type == "vote" & mode == "classification"){
        new_result <- get_most_frequented(sapply(object@trees_list,predict,inputs[i,, drop =FALSE]))
      }

      #If type is max_prob, we need the class with the most probability
      else if(type == "max_prob" & mode == "classification"){
        new_result <- get_class_max_prob(object@trees_list,inputs[i,, drop = FALSE])
      }

      else if(mode == "regression"){
        new_result <- get_value_mean(object@trees_list,inputs[i,, drop = FALSE])
      }

      #Add result
      new_result
    }
  }
  return(results)
}
