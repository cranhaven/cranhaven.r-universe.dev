#' Write Results of Multiverse Analysis to csv-Files
#' 
#' Exports the results to csv format. 
#' 
#' @param results An object of class multiverseMPT.
#' @param path a path where to save the files (e.g., \code{"C:/results/modelX_dataY_"})
#' @importFrom rlang .data
#' @export
write_results <- function(results, path = "MPTmultiverse_"
                          # what = c("est_group", "est_indiv", "est_rho", 
                          #          "test_between", "gof", "gof_group", "gof_indiv", 
                          #          "fungibility", "test_homogeneity")
                          ){
  
  # TODO: allow to specify which columns should be unnested and exported
  #       => requires to work with expressions/tidyverse-issues
  
  readr::write_csv(tidyr::unnest(results, .data$est_group), 
                   path = paste0(path,"est_group.csv"))
  readr::write_csv(tidyr::unnest(results, .data$est_indiv), 
                   path = paste0(path,"est_indiv.csv"))
  readr::write_csv(tidyr::unnest(results, .data$est_rho), 
                   paste0(path,"est_rho.csv"))
  
  readr::write_csv(tidyr::unnest(results, .data$test_between), 
                   paste0(path,"test_between.csv"))
  
  readr::write_csv(tidyr::unnest(results, .data$gof), 
                   paste0(path,"gof.csv"))
  readr::write_csv(tidyr::unnest(results, .data$gof_indiv), 
                   paste0(path,"gof_indiv.csv"))
  readr::write_csv(tidyr::unnest(results, .data$gof_group), 
                   paste0(path,"gof_group.csv"))
  
  readr::write_csv(tidyr::unnest(results, .data$fungibility), 
                   paste0(path,"fungibility.csv"))
}