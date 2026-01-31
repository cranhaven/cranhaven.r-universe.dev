#' Select best model
#'
#' Returns performance metrics for a selected model
#' 
#' @param output A non-ranked viraltab output
#' @param modelo A character value 
#'
#' @return A table with a single model hyperparameters
#' @export
#'
#' @examples
#' \donttest{
#' library(dplyr)
#' library(magrittr)
#' library(baguette)
#' library(kernlab)
#' library(kknn)
#' library(ranger)
#' library(rules)
#' library(glmnet)
#' # Define the function to impute values in the undetectable range
#' set.seed(123)
#' impute_undetectable <- function(column) {
#' ifelse(column <= 40,
#'       rexp(sum(column <= 40), rate = 1/13) + 1,
#'             column)
#'             }
#' # Apply the function to all vl columns using purrr's map_dfc
#' library(viraldomain)
#' data("viral", package = "viraldomain")
#' viral_imputed <- viral %>%
#' mutate(across(starts_with("vl"), ~impute_undetectable(.x)))
#' traindata <- viral_imputed
#' semilla <- 1501
#' target <- "cd_2022"
#' viralvars <- c("vl_2019", "vl_2021", "vl_2022")
#' logbase <- 10
#' pliegues <- 2
#' repeticiones <- 1
#' rejilla <- 1
#' modelo <- "simple_rf"
#' set.seed(123)
#' viraltab(traindata, semilla, target, viralvars, logbase, pliegues, 
#' repeticiones, rejilla, rank_output = FALSE) %>% viralmodel(modelo) 
#' }
viralmodel <- function(output, modelo){
  magrittr::`%>%`(magrittr::`%>%`(
    magrittr::`%>%`(output,workflowsets::extract_workflow_set_result(modelo)),
    tune::select_best(metric = "rmse")),
    as.data.frame())
}