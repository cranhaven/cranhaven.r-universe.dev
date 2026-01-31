#' Calculate the Random Forest Model Domain Applicability Score
#'
#' This function fits a Random Forest model to the provided data and computes a domain applicability score based on PCA distances.
#'
#' Random Forest creates a large number of decision trees, each independent of the others. The final prediction combines the predictions from all individual trees. This function uses the `ranger` engine for fitting regression models.
#'
#' @param featured_col A character string specifying the name of the response variable to predict.
#' @param train_data A data frame containing predictor variables and the response variable for training the model.
#' @param rf_hyperparameters A list of hyperparameters for the Random Forest model, including:
#'   - \code{mtry}: Number of predictors sampled at each split.
#'   - \code{min_n}: Minimum number of data points in a node for further splitting.
#'   - \code{trees}: Number of trees in the ensemble.
#' @param test_data A data frame for making predictions.
#' @param threshold_value A numeric threshold value used for computing domain applicability scores.
#'
#' @return A data frame containing the computed domain applicability scores for each observation in the test dataset.
#'
#' @export
#'
#' @examples
#' set.seed(123)
#' library(dplyr)
#' featured_col <- "cd_2022"
#' train_data <- viral %>%
#'   dplyr::select(cd_2022, vl_2022)
#' test_data <- sero
#' rf_hyperparameters <- list(mtry = 2, min_n = 5, trees = 500)
#' threshold_value <- 0.99
#' rf_domain_score(featured_col, train_data, rf_hyperparameters, test_data, threshold_value)
rf_domain_score <- function(featured_col, train_data, rf_hyperparameters, test_data, threshold_value) {
  invisible(ranger::ranger(Species ~ ., data = data.frame(Sepal.Length=c(5.1,4.9), Species=as.factor(c("setosa", "setosa")))))
  
  workflows::workflow() %>%
    workflows::add_recipe(
      recipes::recipe(
        stats::as.formula(
          paste(featured_col, "~ .")
        ), 
        data = train_data)
    ) %>%
    workflows::add_model(
      parsnip::rand_forest(
        mtry = rf_hyperparameters$mtry, 
        min_n = rf_hyperparameters$min_n, 
        trees = rf_hyperparameters$trees
      ) %>%
        parsnip::set_engine("ranger") %>%
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