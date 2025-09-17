#' @title Train Contamination Detection Model
#' @description Trains two SVM models (classification and regression) to detects whether a sample is contaminated another sample of its same species.
#'
#' @param feature Feature list objects from generate_feature()
#'
#' @import e1071
#' @return A list contains two trained svm models: regression & classification
#' @export
train_ct <- function(feature) {
  df <- as.data.frame(do.call(rbind, feature))
  df <- df[,-1] #The first column is file name, so it is not needed for model training.

  svm_regression_model <- e1071::svm(Mixture ~ .,
                                     data = df,
                                     type = 'eps-regression',
                                     kernel = 'radial',
                                     cost = 16,
                                     gamma = 0.25)

  svm_class_model <- e1071::svm(Mixture ~ .,
                                data = df,
                                type = 'C-classification',
                                kernel = 'radial',
                                cost = 16,
                                gamma = 0.25)

  return (list(regression=svm_regression_model, class=svm_class_model))
}
