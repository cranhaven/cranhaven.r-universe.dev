
#' Summary method to CM and CRM regression model
#'
#' @param ref Real values
#' @param pred Predicted values
#'
#' @export
method_summary <- function(ref, pred) {
  RMSE_L <- sqrt(sum((min(ref) - pred[, 1])^2) / length(ref))
  RMSE_U <- sqrt(sum((max(ref) - pred[, 2])^2) / length(ref))
  R2_L <- cor(min(ref), pred[, 1])^2
  R2_U<- cor(max(ref), pred[, 2])^2
  data.frame("RMSE_L" = RMSE_L, "RMSE_U" = RMSE_U, "R2_L" = R2_L, "R2_U" = R2_U)
}



#' Symbolic Regression Trees

#'
#' @param formula a formula, with a response but no interaction terms. If this a a data frame, that is taken as the model frame (see model.frame).
#' @param sym.data a symbolic data table
#' @param method cm crm
#' @param minsplit 	the minimum number of observations that must exist in a node in order for a split to be attempted.
#' @param maxdepth 	Set the maximum depth of any node of the final tree, with the root node counted as depth 0. Values greater than 30 rpart will give nonsense results on 32-bit machines.
#'
#' @references Lima-Neto, E.A., De Carvalho, F.A.T., (2008). Centre and range method to fitting a linear regression model on symbolic interval data. Computational Statistics and Data Analysis52, 1500-1515
#'
#'
#' Lima-Neto, E.A., De Carvalho, F.A.T., (2010). Constrained linear regression models for symbolic interval-valued variables.  Computational Statistics and Data Analysis 54, 333-347
#'
#'
#' Lima Neto, E.d.A., de Carvalho, F.d.A.T. Nonlinear regression applied to interval-valued data. Pattern Anal Applic 20, 809–824 (2017). https://doi.org/10.1007/s10044-016-0538-y
#'
#'
#' Rodriguez, O. (2018). Shrinkage linear regression for symbolic interval-valued variables.Journal MODULAD 2018, vol. Modulad 45, pp.19-38

#' @importFrom rpart rpart rpart.control
#' @export

sym.rt <- function(formula, sym.data, method = c("cm", "crm"), minsplit = 20, maxdepth = 10) {
  match.arg(method)

  data_c_train <- interval.centers(sym.data)

  model_rt_cm <- rpart::rpart(formula, data = data_c_train,
                              control = rpart::rpart.control(minsplit = minsplit,
                                                             maxdepth = maxdepth))
  class(model_rt_cm) <- c("symbolic_rt_cm", class(model_rt_cm))

  if(method == "cm") return(model_rt_cm)

  data_r_train <- interval.ranges(sym.data)

  model_rt_crm <- list()
  model_rt_crm$model_range <- rpart::rpart(formula, data = data_r_train,
                                           control = rpart::rpart.control(minsplit = minsplit,
                                                                          maxdepth = maxdepth))
  model_rt_crm$model_centers <- model_rt_cm
  class(model_rt_crm) <- c("symbolic_rt_crm", class(model_rt_crm))

  return(model_rt_crm)
}

#' Predict rt_cm model
#'
#' @param model a model_rt_crm object
#' @param new.sym.data  new data
#' @param ...  arguments to predict.rpart
#' @import rpart
#' @references Lima-Neto, E.A., De Carvalho, F.A.T., (2008). Centre and range method to fitting a linear regression model on symbolic interval data. Computational Statistics and Data Analysis52, 1500-1515
#'
#'
#' Lima-Neto, E.A., De Carvalho, F.A.T., (2010). Constrained linear regression models for symbolic interval-valued variables.  Computational Statistics and Data Analysis 54, 333-347
#'
#'
#' Lima Neto, E.d.A., de Carvalho, F.d.A.T. Nonlinear regression applied to interval-valued data. Pattern Anal Applic 20, 809–824 (2017). https://doi.org/10.1007/s10044-016-0538-y
#'
#'
#' Rodriguez, O. (2018). Shrinkage linear regression for symbolic interval-valued variables.Journal MODULAD 2018, vol. Modulad 45, pp.19-38

#' @export
sym.predict.symbolic_rt_cm <- function(model, new.sym.data, ...) {

  data_l_test <- interval.min(new.sym.data)
  data_u_test <- interval.max(new.sym.data)
  prediction_l <- predict(model, data_l_test)
  prediction_u <- predict(model, data_u_test)
  prediction <- data.frame(prediction_l, prediction_u)
  prediction <- data.frame(prediction_l = apply(prediction, 1, min),
                           prediction_u = apply(prediction, 1, max))

  return(prediction)
}



#' Predict rt_crm model
#'
#' @param model a model_rt_crm object
#' @param new.sym.data  new data
#' @param ...  optional parameters
#' @import rpart
#' @references Lima-Neto, E.A., De Carvalho, F.A.T., (2008). Centre and range method to fitting a linear regression model on symbolic interval data. Computational Statistics and Data Analysis52, 1500-1515
#'
#'
#' Lima-Neto, E.A., De Carvalho, F.A.T., (2010). Constrained linear regression models for symbolic interval-valued variables.  Computational Statistics and Data Analysis 54, 333-347
#'
#'
#' Lima Neto, E.d.A., de Carvalho, F.d.A.T. Nonlinear regression applied to interval-valued data. Pattern Anal Applic 20, 809–824 (2017). https://doi.org/10.1007/s10044-016-0538-y
#'
#'
#' Rodriguez, O. (2018). Shrinkage linear regression for symbolic interval-valued variables.Journal MODULAD 2018, vol. Modulad 45, pp.19-38

#' @export
sym.predict.symbolic_rt_crm <- function(model, new.sym.data, ...) {

  data_c_test <- interval.centers(new.sym.data)
  data_r_test <- interval.ranges(new.sym.data)
  prediction_c <- predict(model$model_centers, data_c_test)
  prediction_r <- predict(model$model_range, data_r_test)
  prediction <- data.frame(prediction_l = prediction_c - prediction_r,
                           prediction_u = prediction_c + prediction_r)

  return(prediction)
}



#' Symbolic Regression with Random Forest
#'
#' @param formula a formula, with a response but no interaction terms. If this a a data frame, that is taken as the model frame (see model.frame).
#' @param sym.data  symbolic data table
#' @param method cm crm
#' @param ntree Number of trees to grow. This should not be set to too small a number, to ensure that every input row gets predicted at least a few times.
#' @references Lima-Neto, E.A., De Carvalho, F.A.T., (2008). Centre and range method to fitting a linear regression model on symbolic interval data. Computational Statistics and Data Analysis52, 1500-1515
#'
#'
#' Lima-Neto, E.A., De Carvalho, F.A.T., (2010). Constrained linear regression models for symbolic interval-valued variables.  Computational Statistics and Data Analysis 54, 333-347
#'
#'
#' Lima Neto, E.d.A., de Carvalho, F.d.A.T. Nonlinear regression applied to interval-valued data. Pattern Anal Applic 20, 809–824 (2017). https://doi.org/10.1007/s10044-016-0538-y
#'
#'
#' Rodriguez, O. (2018). Shrinkage linear regression for symbolic interval-valued variables.Journal MODULAD 2018, vol. Modulad 45, pp.19-38

#' @importFrom randomForest randomForest
#' @export
sym.rf <- function(formula, sym.data, method = c("cm", "crm"), ntree = 500) {
  match.arg(method)

  data_c_train <- interval.centers(sym.data)

  model_rf_cm <- randomForest::randomForest(formula, data = data_c_train, ntree = ntree)
  class(model_rf_cm) <- c("symbolic_rf_cm", class(model_rf_cm))

  if(method == "cm") return(model_rf_cm)

  data_r_train <- interval.ranges(sym.data)

  model_rf_crm <- list()
  model_rf_crm$model_range <- randomForest::randomForest(formula, data = data_r_train, ntree = ntree)
  model_rf_crm$model_centers <- model_rf_cm
  class(model_rf_crm) <- c("symbolic_rf_crm", class(model_rf_crm))

  return(model_rf_crm)
}


#' Predict rf_cm model
#'
#' @param model model
#' @param new.sym.data new data
#' @param ... optional parameters
#' @references Lima-Neto, E.A., De Carvalho, F.A.T., (2008). Centre and range method to fitting a linear regression model on symbolic interval data. Computational Statistics and Data Analysis52, 1500-1515
#'
#'
#' Lima-Neto, E.A., De Carvalho, F.A.T., (2010). Constrained linear regression models for symbolic interval-valued variables.  Computational Statistics and Data Analysis 54, 333-347
#'
#'
#' Lima Neto, E.d.A., de Carvalho, F.d.A.T. Nonlinear regression applied to interval-valued data. Pattern Anal Applic 20, 809–824 (2017). https://doi.org/10.1007/s10044-016-0538-y
#'
#'
#' Rodriguez, O. (2018). Shrinkage linear regression for symbolic interval-valued variables.Journal MODULAD 2018, vol. Modulad 45, pp.19-38

#' @export
sym.predict.symbolic_rf_cm <- function(model, new.sym.data, ...) {

  data_l_test <- interval.min(new.sym.data)
  data_u_test <- interval.max(new.sym.data)
  prediction_l <- predict(model, data_l_test)
  prediction_u <- predict(model, data_u_test)
  prediction <- data.frame(prediction_l, prediction_u)
  prediction <- data.frame(prediction_l = apply(prediction, 1, min),
                           prediction_u = apply(prediction, 1, max))

  return(prediction)
}

#' Predict rf_crm model
#'
#' @param model model
#' @param new.sym.data new data
#' @param ... optional parameters
#' @references Lima-Neto, E.A., De Carvalho, F.A.T., (2008). Centre and range method to fitting a linear regression model on symbolic interval data. Computational Statistics and Data Analysis52, 1500-1515
#'
#'
#' Lima-Neto, E.A., De Carvalho, F.A.T., (2010). Constrained linear regression models for symbolic interval-valued variables.  Computational Statistics and Data Analysis 54, 333-347
#'
#'
#' Lima Neto, E.d.A., de Carvalho, F.d.A.T. Nonlinear regression applied to interval-valued data. Pattern Anal Applic 20, 809–824 (2017). https://doi.org/10.1007/s10044-016-0538-y
#'
#'
#' Rodriguez, O. (2018). Shrinkage linear regression for symbolic interval-valued variables.Journal MODULAD 2018, vol. Modulad 45, pp.19-38

#' @export
sym.predict.symbolic_rf_crm <- function(model, new.sym.data, ...) {

  data_c_test <- interval.centers(new.sym.data)
  data_r_test <- interval.ranges(new.sym.data)
  prediction_c <- predict(model$model_centers, data_c_test)
  prediction_r <- predict(model$model_range, data_r_test)
  prediction <- data.frame(prediction_l = prediction_c - prediction_r,
                           prediction_u = prediction_c + prediction_r)

  return(prediction)
}



#' Generalized Boosted Symbolic Regression
#'
#' @param formula A symbolic description of the model to be fit. The formula may include an offset term (e.g. y~offset(n)+x). If keep.data = FALSE in the initial call to gbm then it is the user's responsibility to resupply the offset to gbm.more.
#' @param sym.data symbolic data table
#' @param method cm crm
#' @param distribution distribution
#' @param interaction.depth Integer specifying the maximum depth of each tree (i.e., the highest level of variable interactions allowed). A value of 1 implies an additive model, a value of 2 implies a model with up to 2-way interactions, etc. Default is 1.
#' @param n.trees Integer specifying the total number of trees to fit. This is equivalent to the number of iterations and the number of basis functions in the additive expansion. Default is 100.
#' @param shrinkage A shrinkage parameter applied to each tree in the expansion. Also known as the learning rate or step-size reduction; 0.001 to 0.1 usually work, but a smaller learning rate typically requires more trees. Default is 0.1.
#' @references Lima-Neto, E.A., De Carvalho, F.A.T., (2008). Centre and range method to fitting a linear regression model on symbolic interval data. Computational Statistics and Data Analysis52, 1500-1515
#'
#'
#' Lima-Neto, E.A., De Carvalho, F.A.T., (2010). Constrained linear regression models for symbolic interval-valued variables.  Computational Statistics and Data Analysis 54, 333-347
#'
#'
#' Lima Neto, E.d.A., de Carvalho, F.d.A.T. Nonlinear regression applied to interval-valued data. Pattern Anal Applic 20, 809–824 (2017). https://doi.org/10.1007/s10044-016-0538-y
#'
#'
#' Rodriguez, O. (2018). Shrinkage linear regression for symbolic interval-valued variables.Journal MODULAD 2018, vol. Modulad 45, pp.19-38

#' @import gbm
#' @export

sym.gbm <- function(formula, sym.data, method = c("cm", "crm"), distribution = "gaussian",
                    interaction.depth = 1, n.trees = 500, shrinkage = 0.1) {
  match.arg(method)

  data_c_train <- interval.centers(sym.data)

  model_gbm_cm <- gbm::gbm(formula, data = data_c_train, distribution = distribution,
                           interaction.depth = interaction.depth, n.trees = n.trees,
                           shrinkage = shrinkage)
  class(model_gbm_cm) <- c("symbolic_gbm_cm", class(model_gbm_cm))

  if(method == "cm") return(model_gbm_cm)

  data_r_train <- interval.ranges(sym.data)

  model_gbm_crm <- list()
  model_gbm_crm$model_range <- gbm::gbm(formula, data = data_r_train, distribution = distribution,
                                        interaction.depth = interaction.depth, n.trees = n.trees,
                                        shrinkage = shrinkage)
  model_gbm_crm$model_centers <- model_gbm_cm
  class(model_gbm_crm) <- c("symbolic_gbm_crm", class(model_gbm_crm))

  return(model_gbm_crm)
}

#' Predict model_gbm_cm model
#'
#' @param model model
#' @param new.sym.data new data
#' @param n.trees Integer specifying the total number of trees to fit. This is equivalent to the number of iterations and the number of basis functions in the additive expansion. Default is 100.
#' @param ... optional parameters
#' @references Lima-Neto, E.A., De Carvalho, F.A.T., (2008). Centre and range method to fitting a linear regression model on symbolic interval data. Computational Statistics and Data Analysis52, 1500-1515
#'
#'
#' Lima-Neto, E.A., De Carvalho, F.A.T., (2010). Constrained linear regression models for symbolic interval-valued variables.  Computational Statistics and Data Analysis 54, 333-347
#'
#'
#' Lima Neto, E.d.A., de Carvalho, F.d.A.T. Nonlinear regression applied to interval-valued data. Pattern Anal Applic 20, 809–824 (2017). https://doi.org/10.1007/s10044-016-0538-y
#'
#'
#' Rodriguez, O. (2018). Shrinkage linear regression for symbolic interval-valued variables.Journal MODULAD 2018, vol. Modulad 45, pp.19-38

#' @export
sym.predict.symbolic_gbm_cm <- function(model, new.sym.data, n.trees = 500, ...) {

  data_l_test <- interval.min(new.sym.data)
  data_u_test <- interval.max(new.sym.data)
  prediction_l <- predict(model, data_l_test, n.trees = n.trees)
  prediction_u <- predict(model, data_u_test, n.trees = n.trees)
  prediction <- data.frame(prediction_l, prediction_u)
  prediction <- data.frame(prediction_l = apply(prediction, 1, min),
                           prediction_u = apply(prediction, 1, max))

  return(prediction)
}

#' Predict model_gbm_crm model
#'
#' @param model model
#' @param new.sym.data new data
#' @param n.trees Integer specifying the total number of trees to fit. This is equivalent to the number of iterations and the number of basis functions in the additive expansion. Default is 100.
#' @param ... optional parameters
#' @references Lima-Neto, E.A., De Carvalho, F.A.T., (2008). Centre and range method to fitting a linear regression model on symbolic interval data. Computational Statistics and Data Analysis52, 1500-1515
#'
#'
#' Lima-Neto, E.A., De Carvalho, F.A.T., (2010). Constrained linear regression models for symbolic interval-valued variables.  Computational Statistics and Data Analysis 54, 333-347
#'
#'
#' Lima Neto, E.d.A., de Carvalho, F.d.A.T. Nonlinear regression applied to interval-valued data. Pattern Anal Applic 20, 809–824 (2017). https://doi.org/10.1007/s10044-016-0538-y
#'
#'
#' Rodriguez, O. (2018). Shrinkage linear regression for symbolic interval-valued variables.Journal MODULAD 2018, vol. Modulad 45, pp.19-38

#' @export
sym.predict.symbolic_gbm_crm <- function(model, new.sym.data, n.trees = 500, ...) {

  data_c_test <- interval.centers(new.sym.data)
  data_r_test <- interval.ranges(new.sym.data)
  prediction_c <- predict(model$model_centers, data_c_test, n.trees = n.trees)
  prediction_r <- predict(model$model_range, data_r_test, n.trees = n.trees)
  prediction <- data.frame(prediction_l = prediction_c - prediction_r,
                           prediction_u = prediction_c + prediction_r)

  return(prediction)
}



#' Symbolic k-Nearest Neighbor Regression
#'
#' @param formula a formula object.
#' @param sym.data symbolc data.table
#' @param method cm or crm
#' @param scale logical, scale variable to have equal sd.
#' @param kmax maximum number of k, if ks is not specified.
#' @param kernel kernel to use. Possible choices are "rectangular" (which is standard unweighted knn), "triangular", "epanechnikov" (or beta(2,2)), "biweight" (or beta(3,3)), "triweight" (or beta(4,4)), "cos", "inv", "gaussian" and "optimal".
#' @import  kknn
#' @export
#' @references Lima-Neto, E.A., De Carvalho, F.A.T., (2008). Centre and range method to fitting a linear regression model on symbolic interval data. Computational Statistics and Data Analysis52, 1500-1515
#'
#'
#' Lima-Neto, E.A., De Carvalho, F.A.T., (2010). Constrained linear regression models for symbolic interval-valued variables.  Computational Statistics and Data Analysis 54, 333-347
#'
#'
#' Lima Neto, E.d.A., de Carvalho, F.d.A.T. Nonlinear regression applied to interval-valued data. Pattern Anal Applic 20, 809–824 (2017). https://doi.org/10.1007/s10044-016-0538-y
#'
#'
#' Rodriguez, O. (2018). Shrinkage linear regression for symbolic interval-valued variables.Journal MODULAD 2018, vol. Modulad 45, pp.19-38

sym.knn <- function(formula, sym.data, method = c("cm", "crm"), scale = TRUE,
                    kmax = 20, kernel = "triangular") {
  match.arg(method)

  data_c_train <- interval.centers(sym.data)

  model_knn_cm <- kknn::train.kknn(formula, data = data_c_train, scale = scale,
                                   kmax = kmax, kernel = kernel)
  class(model_knn_cm) <- c("symbolic_knn_cm", class(model_knn_cm))

  if(method == "cm") return(model_knn_cm)

  data_r_train <- interval.ranges(sym.data)

  model_knn_crm <- list()
  model_knn_crm$model_range <- kknn::train.kknn(formula, data = data_r_train, scale = scale,
                                                kmax = kmax, kernel = kernel)
  model_knn_crm$model_centers <- model_knn_cm
  class(model_knn_crm) <- c("symbolic_knn_crm", class(model_knn_crm))

  return(model_knn_crm)
}

#' Predict model_knn_cm model
#'
#' @param model model
#' @param new.sym.data new data
#' @param ... optional parameters
#' @references Lima-Neto, E.A., De Carvalho, F.A.T., (2008). Centre and range method to fitting a linear regression model on symbolic interval data. Computational Statistics and Data Analysis52, 1500-1515
#'
#'
#' Lima-Neto, E.A., De Carvalho, F.A.T., (2010). Constrained linear regression models for symbolic interval-valued variables.  Computational Statistics and Data Analysis 54, 333-347
#'
#'
#' Lima Neto, E.d.A., de Carvalho, F.d.A.T. Nonlinear regression applied to interval-valued data. Pattern Anal Applic 20, 809–824 (2017). https://doi.org/10.1007/s10044-016-0538-y
#'
#'
#' Rodriguez, O. (2018). Shrinkage linear regression for symbolic interval-valued variables.Journal MODULAD 2018, vol. Modulad 45, pp.19-38

#' @export
sym.predict.symbolic_knn_cm <- function(model, new.sym.data, ...) {

  data_l_test <- interval.min(new.sym.data)
  data_u_test <- interval.max(new.sym.data)
  prediction_l <- predict(model, data_l_test)
  prediction_u <- predict(model, data_u_test)
  prediction <- data.frame(prediction_l, prediction_u)
  prediction <- data.frame(prediction_l = apply(prediction, 1, min),
                           prediction_u = apply(prediction, 1, max))

  return(prediction)
}

#' Predict model_knn_crm model
#'
#' @param model model
#' @param new.sym.data new data
#' @param ... optional parameters
#' @references Lima-Neto, E.A., De Carvalho, F.A.T., (2008). Centre and range method to fitting a linear regression model on symbolic interval data. Computational Statistics and Data Analysis52, 1500-1515
#'
#'
#' Lima-Neto, E.A., De Carvalho, F.A.T., (2010). Constrained linear regression models for symbolic interval-valued variables.  Computational Statistics and Data Analysis 54, 333-347
#'
#'
#' Lima Neto, E.d.A., de Carvalho, F.d.A.T. Nonlinear regression applied to interval-valued data. Pattern Anal Applic 20, 809–824 (2017). https://doi.org/10.1007/s10044-016-0538-y
#'
#'
#' Rodriguez, O. (2018). Shrinkage linear regression for symbolic interval-valued variables.Journal MODULAD 2018, vol. Modulad 45, pp.19-38

#' @export
sym.predict.symbolic_knn_crm <- function(model, new.sym.data, ...) {

  data_c_test <- interval.centers(new.sym.data)
  data_r_test <- interval.ranges(new.sym.data)
  prediction_c <- predict(model$model_centers, data_c_test)
  prediction_r <- predict(model$model_range, data_r_test)
  prediction <- data.frame(prediction_l = prediction_c - prediction_r,
                           prediction_u = prediction_c + prediction_r)

  return(prediction)
}



#' Symbolic Support Vector Machines Regression
#'
#' @param formula a symbolic description of the model to be fit.
#' @param sym.data symbolic data.table
#' @param method method
#' @param scale A logical vector indicating the variables to be scaled. If scale is of length 1, the value is recycled as many times as needed. Per default, data are scaled internally (both x and y variables) to zero mean and unit variance. The center and scale values are returned and used for later predictions.
#' @param kernel the kernel used in training and predicting. You might consider changing some of the following parameters, depending on the kernel type.
#' @import e1071
#' @export
#' @references Lima-Neto, E.A., De Carvalho, F.A.T., (2008). Centre and range method to fitting a linear regression model on symbolic interval data. Computational Statistics and Data Analysis52, 1500-1515
#'
#'
#' Lima-Neto, E.A., De Carvalho, F.A.T., (2010). Constrained linear regression models for symbolic interval-valued variables.  Computational Statistics and Data Analysis 54, 333-347
#'
#'
#' Lima Neto, E.d.A., de Carvalho, F.d.A.T. Nonlinear regression applied to interval-valued data. Pattern Anal Applic 20, 809–824 (2017). https://doi.org/10.1007/s10044-016-0538-y
#'
#'
#' Rodriguez, O. (2018). Shrinkage linear regression for symbolic interval-valued variables.Journal MODULAD 2018, vol. Modulad 45, pp.19-38

sym.svm <- function(formula, sym.data, method = c("cm", "crm"), scale = TRUE, kernel = "radial") {
  match.arg(method)

  data_c_train <- interval.centers(sym.data)

  model_svm_cm <- e1071::svm(formula, data = data_c_train, scale = scale, kernel = kernel)
  class(model_svm_cm) <- c("symbolic_svm_cm", class(model_svm_cm))

  if(method == "cm") return(model_svm_cm)

  data_r_train <- interval.ranges(sym.data)

  model_svm_crm <- list()
  model_svm_crm$model_range <- e1071::svm(formula, data = data_r_train, scale = scale, kernel = kernel)
  model_svm_crm$model_centers <- model_svm_cm
  class(model_svm_crm) <- c("symbolic_svm_crm", class(model_svm_crm))

  return(model_svm_crm)
}

#' Predict model_svm_cm model
#'
#' @param model model
#' @param new.sym.data new data
#' @param ... optional parameters
#'
#' @export
#' @references Lima-Neto, E.A., De Carvalho, F.A.T., (2008). Centre and range method to fitting a linear regression model on symbolic interval data. Computational Statistics and Data Analysis52, 1500-1515
#'
#'
#' Lima-Neto, E.A., De Carvalho, F.A.T., (2010). Constrained linear regression models for symbolic interval-valued variables.  Computational Statistics and Data Analysis 54, 333-347
#'
#'
#' Lima Neto, E.d.A., de Carvalho, F.d.A.T. Nonlinear regression applied to interval-valued data. Pattern Anal Applic 20, 809–824 (2017). https://doi.org/10.1007/s10044-016-0538-y
#'
#'
#' Rodriguez, O. (2018). Shrinkage linear regression for symbolic interval-valued variables.Journal MODULAD 2018, vol. Modulad 45, pp.19-38

sym.predict.symbolic_svm_cm <- function(model, new.sym.data, ...) {

  data_l_test <- interval.min(new.sym.data)
  data_u_test <- interval.max(new.sym.data)
  prediction_l <- predict(model, data_l_test)
  prediction_u <- predict(model, data_u_test)
  prediction <- data.frame(prediction_l, prediction_u)
  prediction <- data.frame(prediction_l = apply(prediction, 1, min),
                           prediction_u = apply(prediction, 1, max))

  return(prediction)
}

#' Predict model_svm_crm model
#'
#' @param model model
#' @param new.sym.data new data
#' @param ... optional parameters
#' @references Lima-Neto, E.A., De Carvalho, F.A.T., (2008). Centre and range method to fitting a linear regression model on symbolic interval data. Computational Statistics and Data Analysis52, 1500-1515
#'
#'
#' Lima-Neto, E.A., De Carvalho, F.A.T., (2010). Constrained linear regression models for symbolic interval-valued variables.  Computational Statistics and Data Analysis 54, 333-347
#'
#'
#' Lima Neto, E.d.A., de Carvalho, F.d.A.T. Nonlinear regression applied to interval-valued data. Pattern Anal Applic 20, 809–824 (2017). https://doi.org/10.1007/s10044-016-0538-y
#'
#'
#' Rodriguez, O. (2018). Shrinkage linear regression for symbolic interval-valued variables.Journal MODULAD 2018, vol. Modulad 45, pp.19-38

#' @export
sym.predict.symbolic_svm_crm <- function(model, new.sym.data, ...) {

  data_c_test <- interval.centers(new.sym.data)
  data_r_test <- interval.ranges(new.sym.data)
  prediction_c <- predict(model$model_centers, data_c_test)
  prediction_r <- predict(model$model_range, data_r_test)
  prediction <- data.frame(prediction_l = prediction_c - prediction_r,
                           prediction_u = prediction_c + prediction_r)

  return(prediction)
}


#' Symbolic neural networks regression
#'
#' @param formula a symbolic description of the model to be fitted.
#' @param sym.data symbolic data.table
#' @param method cm crm
#' @param hidden a vector of integers specifying the number of hidden neurons (vertices) in each layer.
#' @param threshold a numeric value specifying the threshold for the partial derivatives of the error function as stopping criteria.
#' @param stepmax the maximum steps for the training of the neural network. Reaching this maximum leads to a stop of the neural network's training process.
#' @import neuralnet
#' @export
#' @references Lima-Neto, E.A., De Carvalho, F.A.T., (2008). Centre and range method to fitting a linear regression model on symbolic interval data. Computational Statistics and Data Analysis52, 1500-1515
#'
#'
#' Lima-Neto, E.A., De Carvalho, F.A.T., (2010). Constrained linear regression models for symbolic interval-valued variables.  Computational Statistics and Data Analysis 54, 333-347
#'
#'
#' Lima Neto, E.d.A., de Carvalho, F.d.A.T. Nonlinear regression applied to interval-valued data. Pattern Anal Applic 20, 809–824 (2017). https://doi.org/10.1007/s10044-016-0538-y
#'
#'
#' Rodriguez, O. (2018). Shrinkage linear regression for symbolic interval-valued variables.Journal MODULAD 2018, vol. Modulad 45, pp.19-38

sym.nnet <- function(formula, sym.data, method = c("cm", "crm"), hidden = c(10),
                     threshold = 0.05, stepmax = 1e+05) {
  match.arg(method)

  data_c_train <- interval.centers(sym.data)

  means_c <- sapply(data_c_train, mean)
  sds_c <- sapply(data_c_train, sd)
  data_c_train  <- tibble::as_tibble(scale(data_c_train, center = means_c, scale = sds_c))

  model_nnet_cm <- neuralnet::neuralnet(formula, data = data_c_train, hidden = hidden,
                                        threshold = threshold, stepmax = stepmax,
                                        linear.output = TRUE)

  model_nnet_cm$data_c_means <- means_c
  model_nnet_cm$data_c_sds <- sds_c

  class(model_nnet_cm) <- c("symbolic_nnet_cm", class(model_nnet_cm))

  if(method == "cm") return(model_nnet_cm)

  data_r_train <- interval.ranges(sym.data)

  means_r <- sapply(data_r_train, mean)
  sds_r <- sapply(data_r_train, sd)
  data_r_train  <- tibble::as_tibble(scale(data_r_train, center = means_r, scale = sds_r))

  model_nnet_crm <- list()
  model_nnet_crm$model_range <- neuralnet::neuralnet(formula, data = data_r_train, hidden = hidden,
                                                     threshold = threshold, stepmax = stepmax,
                                                     linear.output = TRUE)

  model_nnet_crm$model_range$data_r_means <- means_r
  model_nnet_crm$model_range$data_r_sds <- sds_r

  model_nnet_crm$model_centers <- model_nnet_cm
  class(model_nnet_crm) <- c("symbolic_nnet_crm", class(model_nnet_crm))

  return(model_nnet_crm)
}

#' Predict nnet_cm model
#'
#' @param model model
#' @param new.sym.data new data
#' @param ... optional parameters
#' @references Lima-Neto, E.A., De Carvalho, F.A.T., (2008). Centre and range method to fitting a linear regression model on symbolic interval data. Computational Statistics and Data Analysis52, 1500-1515
#'
#'
#' Lima-Neto, E.A., De Carvalho, F.A.T., (2010). Constrained linear regression models for symbolic interval-valued variables.  Computational Statistics and Data Analysis 54, 333-347
#'
#'
#' Lima Neto, E.d.A., de Carvalho, F.d.A.T. Nonlinear regression applied to interval-valued data. Pattern Anal Applic 20, 809–824 (2017). https://doi.org/10.1007/s10044-016-0538-y
#'
#'
#' Rodriguez, O. (2018). Shrinkage linear regression for symbolic interval-valued variables.Journal MODULAD 2018, vol. Modulad 45, pp.19-38

#' @export
sym.predict.symbolic_nnet_cm <- function(model, new.sym.data, ...) {

  data_l_test <- interval.min(new.sym.data)
  data_u_test <- interval.max(new.sym.data)

  means_c <- model$data_c_means
  sds_c <- model$data_c_sds

  data_l_test <- tibble::as_tibble(scale(data_l_test, center = means_c, scale = sds_c))
  data_u_test <- tibble::as_tibble(scale(data_u_test, center = means_c, scale = sds_c))

  response <- model$model.list$response
  mean_c_response <- means_c[response]
  sd_c_response <- sds_c[response]

  prediction_l <- neuralnet::compute(model, data_l_test)$net.result * sd_c_response + mean_c_response
  prediction_u <- neuralnet::compute(model, data_u_test)$net.result * sd_c_response + mean_c_response
  prediction <- data.frame(prediction_l, prediction_u)
  prediction <- data.frame(prediction_l = apply(prediction, 1, min),
                           prediction_u = apply(prediction, 1, max))

  return(prediction)
}

#' Predict nnet_crm model
#'
#' @param model model
#' @param new.sym.data new data
#' @param ... optional parameters
#' @references Lima-Neto, E.A., De Carvalho, F.A.T., (2008). Centre and range method to fitting a linear regression model on symbolic interval data. Computational Statistics and Data Analysis52, 1500-1515
#'
#'
#' Lima-Neto, E.A., De Carvalho, F.A.T., (2010). Constrained linear regression models for symbolic interval-valued variables.  Computational Statistics and Data Analysis 54, 333-347
#'
#'
#' Lima Neto, E.d.A., de Carvalho, F.d.A.T. Nonlinear regression applied to interval-valued data. Pattern Anal Applic 20, 809–824 (2017). https://doi.org/10.1007/s10044-016-0538-y
#'
#'
#' Rodriguez, O. (2018). Shrinkage linear regression for symbolic interval-valued variables.Journal MODULAD 2018, vol. Modulad 45, pp.19-38

#' @export
sym.predict.symbolic_nnet_crm <- function(model, new.sym.data, ...) {

  data_c_test <- interval.centers(new.sym.data)
  data_r_test <- interval.ranges(new.sym.data)

  means_c <- model$model_centers$data_c_means
  sds_c <- model$model_centers$data_c_sds

  means_r <- model$model_range$data_r_means
  sds_r <- model$model_range$data_r_sds

  data_c_test <- tibble::as_tibble(scale(data_c_test, center = means_c, scale = sds_c))
  data_r_test <- tibble::as_tibble(scale(data_r_test, center = means_r, scale = sds_r))

  response <- model$model_centers$model.list$response

  mean_c_response <- means_c[response]
  sd_c_response <- sds_c[response]

  mean_r_response <- means_r[response]
  sd_r_response <- sds_r[response]

  prediction_c <- neuralnet::compute(model$model_centers, data_c_test)$net.result * sd_c_response + mean_c_response
  prediction_r <- neuralnet::compute(model$model_range, data_r_test)$net.result * sd_r_response + mean_r_response
  prediction <- data.frame(prediction_l = prediction_c - prediction_r,
                           prediction_u = prediction_c + prediction_r)

  return(prediction)
}

