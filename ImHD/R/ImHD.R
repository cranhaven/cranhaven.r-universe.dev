#'@title Artificial Intelligence Based Machine Learning Algorithms for Height Diameter Relationships of Conifer Trees
#' @param data Datasets
#' @param splitratio Train-Test split ratio
#' @import stats randomForest e1071 xgboost ggplot2 reshape2 rpart
#' @return
#' \itemize{
#'   \item Prediction: Prediction of all ML models
#'   \item Accuracy:  Accuracy metrics
#' }
#' @export
#'
#' @examples
#' \donttest{
#' library("ImHD")
#' data <- system.file("extdata", "data_test.csv", package = "ImHD")
#' data_test <- read.csv(data)
#' Model<-ImHD(data =data_test)
#' }
#' @references
#' \itemize{
#'\item Jeelani, M.I., Tabassum, A., Rather, K and Gul,M.2023. Neural Network Modeling of Height Diameter Relationships for Himalayan Pine through Back Propagation Approach. Journal of The Indian Society of Agricultural Statistics. 76(3): 169–178.  <doi:10.1002/9781118032985>
#' }

ImHD <- function(data, splitratio=0.7) {
  Model<-NULL
  value<-NULL
  Metric<-NULL
  Diameter<-NULL
  SplitRatio<-splitratio
  train_index <- sample(1:nrow(data), 0.7 * nrow(data))
  train_data <- data[train_index, ]
  test_data <- data[-train_index, ]

  # Define predictors and target variable
  x_train <- train_data$Diameter
  y_train <- train_data$Height
  x_test <- test_data$Diameter

  # Decision Tree
  dt_model <- rpart(Height ~ Diameter, data = train_data)
  dt_predictions <- predict(dt_model, newdata = data.frame(Diameter = x_test))

  # Random Forest
  rf_model <- randomForest(Height ~ Diameter, data = train_data)
  rf_predictions <- predict(rf_model, newdata = data.frame(Diameter = x_test))

  # Support Vector Machine
  svm_model <- svm(Height ~ Diameter, data = train_data)
  svm_predictions <- predict(svm_model, newdata = data.frame(Diameter = x_test))

  # Gradient Boosting
  gb_model <- xgboost(data = matrix(x_train), label = y_train, nrounds = 100)
  gb_predictions <- predict(gb_model, newdata = matrix(x_test))

    # Calculate RMSE ,  MAE and PER

  # Calculate RMSE and MAE
  rmse <- function(predictions, actual) {
    sqrt(mean((predictions - actual)^2))
  }

  mae <- function(predictions, actual) {
    mean(abs(predictions - actual))
  }

  # Calculate PER (Prediction Error Rate)
  per <- function(predictions, actual, threshold) {
    sum(abs(predictions - actual) > threshold) / length(actual)
  }

  # Actual height values for the test data
  y_test <- test_data$Height

  # Calculate RMSE and MAE for each model
  dt_rmse <- rmse(dt_predictions, y_test)
  dt_mae <- mae(dt_predictions, y_test)

  rf_rmse <- rmse(rf_predictions, y_test)
  rf_mae <- mae(rf_predictions, y_test)

  svm_rmse <- rmse(svm_predictions, y_test)
  svm_mae <- mae(svm_predictions, y_test)

  gb_rmse <- rmse(gb_predictions, y_test)
  gb_mae <- mae(gb_predictions, y_test)

  # Set threshold for PER calculation
  threshold <- 0.1

  # Calculate PER for each model
  dt_per <- per(dt_predictions, y_test, threshold)
  rf_per <- per(rf_predictions, y_test, threshold)
  svm_per <- per(svm_predictions, y_test, threshold)
  gb_per <- per(gb_predictions, y_test, threshold)

  # Print the results
  cat("Decision Tree:\n", "RMSE:", dt_rmse, "MAE:", dt_mae, "PER:", dt_per, "\n")
  cat("Random Forest:\n", "RMSE:", rf_rmse, "MAE:", rf_mae, "PER:", rf_per, "\n")
  cat("Support Vector Machine:\n", "RMSE:", svm_rmse, "MAE:", svm_mae, "PER:", svm_per, "\n")
  cat("Gradient Boosting:\n", "RMSE:", gb_rmse, "MAE:", gb_mae, "PER:", gb_per, "\n")


  # Comparison of Algothirms in tems of metrices
  # Create a data frame for the metrics
  metrics_df <- data.frame(
    Model = c("Decision Tree", "Random Forest", "Support Vector Machine", "Gradient Boosting"),
    RMSE = c(dt_rmse, rf_rmse, svm_rmse, gb_rmse),
    MAE = c(dt_mae, rf_mae, svm_mae, gb_mae),
    PER = c(dt_per, rf_per, svm_per, gb_per)
  )

  # Melt the data for plotting
  metrics_melted <- melt(metrics_df, id.vars = "Model", variable.name = "Metric")

  # Create the plot using ggplot2
  ggplot(metrics_melted, aes(x = Model, y = value, fill = Metric)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = "Model Comparison: RMSE, MAE, and PER",
         x = "Model",
         y = "Value") +
    scale_fill_manual(values = c("RMSE" = "blue", "MAE" = "green", "PER" = "red")) +
    theme_minimal()


  # Prediction plot
  # Create a data frame with actual and predicted values
  prediction_df <- data.frame(
    Diameter = x_test,
    Actual = y_test,
    Decision_Tree = dt_predictions,
    Random_Forest = rf_predictions,
    SVM = svm_predictions,
    Gradient_Boosting = gb_predictions
  )

  # Melt the data for plotting
  prediction_melted <- melt(prediction_df, id.vars = "Diameter", variable.name = "Model")

  # Create the prediction plot using ggplot2
  ggplot(prediction_melted, aes(x = Diameter, y = value, color = Model)) +
    geom_point(size = 2) +
    geom_smooth(method = "loess", se = FALSE, size = 1.5, aes(group = Model)) +
    labs(title = "Height Prediction Comparison with Smooth Lines",
         x = "Diameter",
         y = "Height") +
    scale_color_manual(values = c("Actual" = "black",
                                  "Decision_Tree" = "blue",
                                  "Random_Forest" = "green",
                                  "SVM" = "red",
                                  "Gradient_Boosting" = "purple")) +
    theme_minimal() +
    theme(legend.position = "top")
  results<- list(Predictions=prediction_df, Accuracy=metrics_df)
  return(results)
}
