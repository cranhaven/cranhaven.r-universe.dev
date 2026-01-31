#' Calculate the Neural Network model domain applicability score
#'
#' This function fits a Neural Network model to the provided data and computes a
#' domain applicability score based on PCA distances.
#'
#' @param featured_col The name of the featured column in the training data.
#' @param train_data The training data used to fit the Neural Network model.
#' @param nn_hyperparameters A list of Neural Network hyperparameters, including hidden_units, penalty, and epochs.
#' @param test_data The testing domain data used to calculate the domain applicability score.
#' @param threshold_value The threshold value for domain applicability scoring.
#'
#' @return A tibble with the domain applicability scores.
#' @export
#'
#' @examples
#' set.seed(123)
#' library(dplyr)
#' featured_col <- "cd_2022"
#' # Specifying features for training and testing procedures
#' train_data = viral %>%
#'   dplyr::select(cd_2022, vl_2022)
#' test_data = sero 
#' nn_hyperparameters <- list(hidden_units = 1, penalty = 0.3746312,  epochs =  480)
#' threshold_value <- 0.99
#' nn_domain_score(featured_col, train_data, nn_hyperparameters, test_data, threshold_value)
nn_domain_score <- function(featured_col, train_data, nn_hyperparameters, test_data, threshold_value) {
  workflows::workflow() %>%
    workflows::add_recipe(
      recipes::recipe(
        stats::as.formula(
          paste(featured_col, "~ .")
        ), 
        data = train_data)
    ) %>%
    workflows::add_model(
      parsnip::mlp(
        hidden_units = nn_hyperparameters$hidden_units,
        penalty = nn_hyperparameters$penalty,
        epochs = nn_hyperparameters$epochs
      ) %>%
        parsnip::set_engine("nnet") %>%
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
          tidyselect::starts_with("distance"))
    )
}