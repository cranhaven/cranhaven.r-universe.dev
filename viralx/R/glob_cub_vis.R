#' Global Visualization of SHAP Values for Cubist Rules Model
#'
#' This function generates a visualization for the global feature importance of
#' a Cubist Rules (CR) model trained on HIV data with specified
#' hyperparameters.
#'
#' @param vip_featured The name of the response variable to explain.
#' @param hiv_data The training dataset containing predictor variables and the response variable.
#' @param cr_hyperparameters A list of hyperparameters for the CR model, including:
#'   - \code{committees}: The number of committees to consider.
#'   - \code{neighbors}: The number of neighbors to consider.
#' @param vip_train The dataset used for training the CR model.
#' @param v_train The response variable used for training the CR model.
#'
#' @return A visualization of global feature importance for the CR model.
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' library(rules)
#' library(Cubist)
#' set.seed(123)
#' hiv_data <- train2
#' cr_hyperparameters <- list(neighbors = 5, committees = 58)
#' vip_featured <- c("cd_2022")
#' vip_features <- c("cd_2019", "vl_2019", "cd_2021", "vl_2021", "vl_2022")
#' vip_train <- train2 |>
#' dplyr::select(rsample::all_of(vip_features))
#' v_train <- train2 |>
#' dplyr::select(rsample::all_of(vip_featured))
#' glob_cr_vis(vip_featured, hiv_data, cr_hyperparameters, vip_train, v_train)
glob_cr_vis <- function(vip_featured, hiv_data, cr_hyperparameters, vip_train, v_train) {
  DALEXtra::explain_tidymodels(workflows::workflow() |>
                                 workflows::add_recipe(recipes::recipe(stats::as.formula(paste(vip_featured,"~.")), data = hiv_data)) |>
                                 workflows::add_model(parsnip::cubist_rules(
                                   committees = cr_hyperparameters$committees,
                                   neighbors = cr_hyperparameters$neighbors) |>
                                     parsnip::set_engine("Cubist")) |>
                                   parsnip::fit(data = hiv_data),
                               data = vip_train,
                               y = v_train,
                               label = "cubist",
                               verbose = FALSE) |>
    DALEX::model_parts() |>
    plot()
}
