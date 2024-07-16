#' Data balance.
#' Most classes adopt random undersampling, while a few classes adopt smote method to oversample to obtain relatively balanced data;
#'
#' @param data A data frame. Except class column, all are numeric types.
#' @param ratio Numeric between 0 and 1. The percent of test set split from data.
#' @param parallel Logical.
#'
#' @return A list contain train set and test set.
#' @export
#' @importFrom scutr SCUT_parallel SCUT oversample_smote resample_random
#' @importFrom data.table as.data.table
#' @importFrom mlr3 as_task_classif partition
#' @importFrom dplyr slice bind_rows
#' @importFrom magrittr set_names
#' @examples
#' library(dplyr)
#' data = rename(iris,class =Species)
#' mi_balance_data(data)
mi_balance_data <- function(data, ratio = 0.3, parallel = FALSE) {
  system.time({
    # The multicategorical data were balanced by random sampling of the undersampled samples and the oversampling part was sampled by the smote method to obtain relatively balanced data;
    if(parallel){
      data_smtd <- SCUT_parallel(data, "class", oversample = oversample_smote, undersample = resample_random)
    }else{
      data_smtd <- SCUT(data, "class", oversample = oversample_smote, undersample = resample_random)
    }
  })
  #Difference between the balanced data set and the pre-balanced data set, as the training set
  train_new <- setdiff(data_smtd, data)
  task <- data %>%
    as.data.table() %>%
    as_task_classif(target = "class", feature = -"class")
  # A portion of the original data set is also used as the training set
  train_raw <-partition(task, ratio = 1-ratio)$train %>% slice(data, .)
  #a split of the original data set plus a new sample obtained by sampling as the training set
  train <- bind_rows(train_raw, train_new)
  # Test set data from a split of the original data set
  test <-partition(task, ratio = 1-ratio)$test %>% slice(data, .)

  return(list(train, test))
}
