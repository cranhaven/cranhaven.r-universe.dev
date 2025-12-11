#' @name run_abc_analysis
#' @author Sebastian Malkusch
#' @title Performs item categorization
#' @description Performs item categorization on permutation or shap analysis object
#' @details Interpretation results are passed to the function.
#' Based on the type of interpretation experiment the data is transformed into a uniformly structured data frame.
#' Item categorization is performed by computed ABC analysis.
#' The result is returned in form of a tibble.
#'
#' @param data_obj Resuls of model interpretation experiment
#' @param method Method used for model interpretation (permutatopn or shap)
#'
#' @return A tibble with item categories
#'
#' @importFrom tibble as_tibble
#' @importFrom dplyr group_by mutate select summarize
#' @importFrom tidyr pivot_longer
#' @importFrom ABCanalysis ABCanalysis
#' @importFrom stats sd
#'
#' @return a tibble
run_abc_analysis <- function(data_obj, method){

  # design data_df
  data_df <- switch(method,
                    "permutation"= data_obj,
                    "shap" = data_obj %>%
                      tibble::as_tibble() %>%
                      dplyr::mutate(id = seq(nrow(.))) %>%
                      tidyr::pivot_longer(cols = -c(id), names_to = "Feature", values_to = "Shap") %>%
                      dplyr::group_by(Feature) %>%
                      dplyr::select(-id) %>%
                      dplyr::summarise(Importance = mean(abs(Shap)),
                                       StDev = stats::sd(abs(Shap))),
                    "internal" = dplyr::rename(data_obj, Feature = Variable),
                    stop(sprintf("Interpretation method %s is unknown. Needs to be permutation shap or internal.", method)))

  # perform ABC_analysis
  x <- data_df$Importance
  names(x) <- data_df$Feature
  abc_obj <- ABCanalysis::ABCanalysis(x, PlotIt = FALSE)
  abc_categorization <- rep("C", length(x))
  abc_categorization[abc_obj$Aind] <- "A"
  abc_categorization[abc_obj$Bind] <- "B"

  # return results
  data_df %>%
    dplyr::mutate(abc_analysis = abc_categorization) %>%
    return()
}
