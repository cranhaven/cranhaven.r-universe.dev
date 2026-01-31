#' Calculate the MARS model domain applicability score
#'
#' This function fits a MARS (Multivariate Adaptive Regression Splines) model to
#' the provided data and computes a domain applicability score based on PCA distances.
#'
#' @importFrom dplyr %>%
#'
#' @param featured_col The name of the featured column.
#' @param train_data A data frame containing the training data.
#' @param mars_hyperparameters A list of hyperparameters for the MARS model, including:
#'   - \code{num_terms}: The number of terms to include in the MARS model.
#'   - \code{prod_degree}: The degree of interaction terms to include.
#'   - \code{prune_method}: The method used for pruning the MARS model.
#' @param test_data A data frame containing the test data.
#' @param threshold_value The threshold value for the domain score.
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
#' mars_hyperparameters <- list(num_terms = 3, prod_degree = 1, prune_method = "none")
#' threshold_value <- 0.99
#' # Call the function
mars_domain_score <- function(featured_col, train_data, mars_hyperparameters, test_data, threshold_value) {
  invisible(earth::etitanic[1,1])
  
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
      parsnip::mars(
        num_terms = mars_hyperparameters$num_terms,
        prod_degree = mars_hyperparameters$prod_degree,
        prune_method = mars_hyperparameters$prune_method
      ) %>%
        parsnip::set_engine("earth") %>%
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