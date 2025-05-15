#' Global Explainers for K-Nearest Neighbor Models
#'
#' This function calculates global feature importance for a K-Nearest Neighbors
#' (KNN) model trained on HIV data with specified hyperparameters.
#'
#' @param vip_featured The name of the response variable to explain.
#' @param hiv_data The training dataset containing predictor variables and the response variable.
#' @param knn_hyperparameters A list of hyperparameters for the KNN model, including:
#'   - \code{neighbors}: The number of neighbors to consider.
#'   - \code{weight_func}: The weight function to use.
#'   - \code{dist_power}: The distance power parameter.
#' @param vip_train The dataset used for training the KNN model.
#' @param v_train The response variable used for training the KNN model.
#'
#' @return A list of global feature importance measures for each predictor variable.
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' set.seed(123)
#' hiv_data <- train2
#' knn_hyperparameters <- list(neighbors = 5, weight_func = "optimal", dist_power = 0.3304783)
#' vip_featured <- c("cd_2022")
#' vip_features <- c("cd_2019", "vl_2019", "cd_2021", "vl_2021", "vl_2022")
#' vip_train <- train2 |>
#' dplyr::select(rsample::all_of(vip_features))
#' v_train <- train2 |>
#' dplyr::select(rsample::all_of(vip_featured))
#' viralx_knn_glob(vip_featured, hiv_data, knn_hyperparameters, vip_train, v_train)
viralx_knn_glob <- function(vip_featured, hiv_data, knn_hyperparameters, vip_train, v_train) {
  DALEXtra::explain_tidymodels(workflows::workflow() |>
                                 workflows::add_recipe(recipes::recipe(stats::as.formula(paste(vip_featured,"~.")), data = hiv_data) |>
                                                         recipes::step_normalize(recipes::all_predictors())) |>
                                 workflows::add_model(parsnip::nearest_neighbor(
                                   neighbors = knn_hyperparameters$neighbors,
                                   weight_func = knn_hyperparameters$weight_func,
                                   dist_power = knn_hyperparameters$dist_power) |>
                                     parsnip::set_engine("kknn") |>
                                     parsnip::set_mode("regression")) |>
                                 parsnip::fit(data = hiv_data),
                               data = vip_train,
                               y = v_train,
                               label = "knn + normalized",
                               verbose = FALSE) |>
    DALEX::model_parts()
}
