#' Explain K-Nearest Neighbors Model
#'
#' Explains the predictions of a K-Nearest Neighbors (KNN) model for CD4 and
#' viral load data using the DALEX and DALEXtra packages. It provides insights
#' into the specified variable's impact on the KNN model's predictions.
#'
#' @import kknn
#'
#' @param vip_featured The name of the variable to be explained.
#' @param hiv_data The data frame containing the CD4 and viral load data.
#' @param knn_hyperparameters A list of hyperparameters to be tuned for the KNN model.
#' @param vip_train The training data used for creating the explainer object.
#' @param vip_new A new observation for which to generate explanations.
#'
#' @return A data frame containing explanations for the specified variable.
#'
#' @export
#'
#' @examples
#' hiv_data <- train2
#' knn_hyperparameters <- list(neighbors = 5, weight_func = "optimal", dist_power = 0.3304783)
#' vip_featured <- c("cd_2022")
#' vip_train <- hiv_data
#' vip_new <- vip_train[1,]
#' viralx_knn(vip_featured, hiv_data, knn_hyperparameters, vip_train, vip_new)
viralx_knn <- function(vip_featured, hiv_data, knn_hyperparameters, vip_train, vip_new) {
  DALEXtra::explain_tidymodels(
    workflows::workflow() |>
      workflows::add_recipe(
        recipes::recipe(
          stats::as.formula(paste(vip_featured, "~.")), data = hiv_data) |>
          recipes::step_normalize(recipes::all_predictors())) |>
      workflows::add_model(
        parsnip::nearest_neighbor(
          neighbors = knn_hyperparameters$neighbors,
          weight_func = knn_hyperparameters$weight_func,
          dist_power = knn_hyperparameters$dist_power) |>
          parsnip::set_engine("kknn") |>
          parsnip::set_mode("regression")) |>
      parsnip::fit(hiv_data),
    data = vip_train,
    y = vip_featured,
    label = "knn + normalized",
    verbose = FALSE) |>
    DALEX::predict_parts(vip_new) |>
    as.data.frame() |>
    dplyr::select(1,2) |>
    dplyr::mutate_if(is.numeric, round, digits = 2)
}
