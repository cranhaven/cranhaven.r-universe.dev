#' Calculate the K-Nearest Neighbor model domain applicability score
#'
#' This function fits a K-Nearest Neighbor (KNN) model to the provided data
#' and computes a domain applicability score based on PCA distances.
#'
#' @importFrom dplyr %>%
#'
#' @param featured_col The name of the response variable to predict.
#' @param train_data The training dataset containing predictor variables and the response variable.
#' @param knn_hyperparameters A list of hyperparameters for the KNN model, including:
#'   - \code{neighbors}: The number of neighbors to consider.
#'   - \code{weight_func}: The weight function to use.
#'   - \code{dist_power}: The distance power parameter.
#' @param test_data The test dataset for making predictions.
#' @param threshold_value The threshold value used for computing domain scores.
#'
#' @return A data frame containing the computed domain scores for each observation in the test dataset.
#'
#' @export
#'
#' @examples
#' set.seed(123)
#' library(dplyr)
#' library(magrittr)
#' featured_col <- "cd_2022"
#' # Specifying features for training and testing procedures
#' train_data = viral %>%
#'   dplyr::select(cd_2022, vl_2022)
#' test_data = sero 
#' knn_hyperparameters <- list(neighbors = 5, weight_func = "optimal", dist_power = 0.3304783)
#' threshold_value <- 0.99
#' knn_domain_score(featured_col, train_data, knn_hyperparameters, test_data, threshold_value)
knn_domain_score <- function(featured_col, train_data, knn_hyperparameters, test_data, threshold_value){
    invisible(magrittr::`%>%`(c(-1) , abs()))
    invisible(kknn::contr.dummy(2))
  
  workflows::workflow() %>%
    workflows::add_recipe(
      recipes::recipe(
        stats::as.formula(
          paste(featured_col, "~ .")
        ),
        data = train_data
      )
    ) %>%
    workflows::add_model(
      parsnip::nearest_neighbor(
        neighbors = knn_hyperparameters$neighbors,
        weight_func = knn_hyperparameters$weight_func,
        dist_power = knn_hyperparameters$dist_power
      ) %>%
        parsnip::set_engine("kknn") %>%
        parsnip::set_mode("regression")
    ) %>%
    parsnip::fit(data = train_data) %>%
    stats::predict(test_data) %>%
    dplyr::bind_cols(
      applicable::apd_pca(
        ~ .,
        data = train_data,
        threshold = threshold_value) %>%
        applicable::score(test_data) %>%
        dplyr::select(
          tidyselect::starts_with("distance")
        )
    )
}