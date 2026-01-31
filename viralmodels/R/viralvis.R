#' Competing models plot
#'
#' Plots the rankings of a series of regression models for viral load or CD4 
#' counts
#' 
#' @param output A non-ranked viraltab output
#'
#' @returns A plot of ranking models
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
#' set.seed(123)
#' viraltab(traindata, semilla, target, viralvars, logbase, pliegues, 
#' repeticiones, rejilla, rank_output = FALSE) %>% viralvis() 
#' }
viralvis <- function(output) {
  magrittr::`%>%`(output,
                  tune::autoplot(
                    rank_metric = "rmse",  # <- how to order models
                    metric = "rmse",       # <- which metric to visualize
                    select_best = TRUE     # <- one point per workflow
                  ))
}