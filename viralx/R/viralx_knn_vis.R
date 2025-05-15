#' Visualize SHAP Values for K-Nearest Neighbor Model
#'
#' Visualizes SHAP (Shapley Additive Explanations) values for a KNN
#' (K-Nearest Neighbor) model by employing the DALEXtra and DALEX packages to
#' provide visual insights into the impact of a specified variable on the
#' model's predictions.
#'
#' @param vip_featured The name of the response variable to explain.
#' @param hiv_data The training dataset containing predictor variables and the response variable.
#' @param knn_hyperparameters A list of hyperparameters for the KNN model, including:
#'   - \code{neighbors}: The number of neighbors to consider.
#'   - \code{weight_func}: The weight function to use.
#'   - \code{dist_power}: The distance power parameter.
#' @param vip_train The dataset used for training the KNN model.
#' @param vip_new The dataset for which SHAP values are calculated.
#' @param orderings The number of orderings for SHAP value calculations.
#' @return A list of SHAP values for each observation in \code{vip_new}.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' set.seed(123)
#' hiv_data <- train2
#' knn_hyperparameters <- list(neighbors = 5, weight_func = "optimal", dist_power = 0.3304783)
#' vip_featured <- c("cd_2022")
#' vip_train <- hiv_data
#' vip_new <- vip_train[1,]
#' orderings <- 20
#' viralx_knn_vis(vip_featured, hiv_data, knn_hyperparameters, vip_train, vip_new, orderings)
#' }
viralx_knn_vis <- function(vip_featured, hiv_data, knn_hyperparameters, vip_train, vip_new, orderings) {
  DALEXtra::explain_tidymodels(
    workflows::workflow() |>
      workflows::add_recipe(recipes::recipe(stats::as.formula(paste(vip_featured,"~.")), data = hiv_data) |>
                              recipes::step_normalize(recipes::all_predictors())) |>
      workflows::add_model(
        parsnip::nearest_neighbor(
          neighbors = knn_hyperparameters$neighbors,
          weight_func = knn_hyperparameters$weight_func,
          dist_power = knn_hyperparameters$dist_power) |>
          parsnip::set_engine("kknn") |>
          parsnip::set_mode("regression"))|>
      parsnip::fit(data = hiv_data),
    data = vip_train,
    y = vip_featured,
    label = "nn + normalized",
    verbose = FALSE) |>
    DALEX::predict_parts(vip_new, type ="shap", B = orderings) |>
    plot()
}
