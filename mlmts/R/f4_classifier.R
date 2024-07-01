

#' Constructs the F4 classifier of López-Oriona and Vilar (2021)
#'
#' \code{f4_classifier} computes the F4 classifier for MTS proposed
#' by \insertCite{lopez2021f4;textual}{mlmts}.
#'
#' @param training_data A list of MTS constituting the training set to fit
#' classifier F4.
#' @param new_data A list of MTS for which the class labels have to be predicted.
#' @param classes A vector containing the class labels associated with the
#' elements in \code{training_data}.
#' @param levels The set of probability levels to compute the QCD-estimates.
#' @param cv_folds The number of folds concerning the cross-validation
#' procedure used to fit F4 with respect to \code{training_data}.
#' @param var_rate Rate of desired variability to select the principal
#' components associated with the QCD-based features.
#' @return If \code{new_data = NULL} (default), returns a fitted model of class
#' \code{train} (see \code{\link[caret]{train}}). Otherwise, the function
#' returns the predicted class labels for the elements in \code{new_data}.
#' @examples
#' predictions <- f4_classifier(training_data = Libras$data[1 : 20],
#' new_data = Libras$data[181 : 200], classes = Libras$classes[181 : 200])
#' # Computing the predictions for the test set of dataset Libras
#' @details
#' This function constructs the classifier F4 of . Given a set of MTS with
#' associated class labels, estimates of the quantile cross-spectral density (QCD)
#' and the maximum overlap discrete wavelet transform (MODWT) are first computed for each series. Then
#' Principal Components Analysis (PCA) is applied over the dataset of QCD-based
#' features and a given number of principal components are retained according to a criterion of
#' explained variability. Next, each series is decribed by means of the concatenation
#' of the QCD-based transformed features and the MODWT-based features. Finally, a traditional
#' random forest classifier is executed in the resulting dataset.
#' @encoding UTF-8
#' @author
#' Ángel López-Oriona, José A. Vilar
#' @references{
#'
#'   \insertRef{lopez2021f4}{mlmts}
#'
#' }
#' @export



f4_classifier <- function(training_data, new_data = NULL, classes, levels = c(0.1, 0.5, 0.9),
                          cv_folds = 5, var_rate = 0.90) {


  l_training_data <- length(training_data)
  l_new_data <- length(new_data)
  combined_data <- c(training_data, new_data)
  check_mts(combined_data)

  # Extracting the QCD-based features

  qcd_features <- dis_qcd(combined_data, levels = levels, features = TRUE)
  p_comp <- stats::prcomp(qcd_features)


  # Selecting the number of principal components based on variability

  percentage_variability <- cumsum(p_comp$sdev/sum(p_comp$sdev))
  n_p_comp <- sum(percentage_variability < var_rate) + 1
  p_comp_matrix <- data.frame(p_comp$x[, 1 : n_p_comp])
  p_comp_matrix_training <- p_comp_matrix[(1 : l_training_data),]


  # Extracting the wavelet-based features


  features_w <- dis_modwt(combined_data, wf = 'd4', J = 2, features = TRUE)
  features_w_training <- features_w[(1 : l_training_data),]



  complete_training_data <- data.frame(cbind(p_comp_matrix_training,
                                             features_w_training,
                                         factor(classes)))
  d <- dim(complete_training_data)[2]
  colnames(complete_training_data)[d] <- 'Class'
  train_control <- caret::trainControl(method = "cv", number = cv_folds)
  model <- caret::train(Class~., data = complete_training_data,
                 trControl = train_control, method = 'ranger')



  if (!is.null(new_data)) {

  p_comp_matrix_test <- p_comp_matrix[(l_training_data + 1) : (l_training_data + l_new_data),]
  features_w_test <- features_w[(l_training_data + 1) : (l_training_data + l_new_data),]
  complete_new_data <- data.frame(cbind(p_comp_matrix_test,
                                          features_w_test))
  predictions <- stats::predict(model, newdata = complete_new_data)

  return(predictions)

  } else {

    return(model)

  }

}
