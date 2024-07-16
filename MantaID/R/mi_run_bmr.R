#' Compare classification models with small samples.
#'
#' @param data A tibble.All are numeric except the first column is a factor.
#'
#' @param row_num Number of samples used.
#' @param resamplings R6/Resampling.Resampling method.
#' @importFrom dplyr slice select across mutate
#' @importFrom tidyselect everything
#' @importFrom data.table as.data.table
#' @importFrom mlr3 as_task_classif lrns benchmark_grid set_threads benchmark msr rsmps
#' @export
#' @return  A list of R6 class of benchmark results and scores of test set.
#' examples
#' data(mi_data_procID)
#' mi_run_bmr(mi_data_procID)
mi_run_bmr <- function(data, row_num = 1000, resamplings = rsmps("cv", folds = 10)) {
  data <- data %>% mutate(across(.cols = -class,.fns = as.numeric))
  if(nrow(data)<row_num){
    row_num = nrow(data)
  }
  task <- data %>%
    slice(sample(nrow(data), row_num), preserve = TRUE) %>%
    as.data.table() %>%
    as_task_classif(target = "class", feature = -c("class"))
  learners <- lrns(c("classif.naive_bayes", "classif.rpart", "classif.ranger", "classif.xgboost", "classif.kknn", "classif.multinom"),
    predict_type = "prob",
    predict_sets = c("train", "test")
  )
  bmr_g <- benchmark_grid(
    tasks = task,
    learners = learners,
    resamplings = resamplings
  )
  set_threads(bmr_g)
  bmr <- benchmark(bmr_g)
  measures <- msr("classif.acc")
  scores <- bmr$score(measures) %>%
    as.data.table() %>%
    select(ncol(.), "learner_id", everything())
  list(bmr, scores)
}
