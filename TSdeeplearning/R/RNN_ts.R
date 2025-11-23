#' @importFrom keras compile layer_simple_rnn layer_dense timeseries_generator keras_model_sequential evaluate fit optimizer_rmsprop
#' @importFrom tsutils lagmatrix
#' @importFrom BiocGenerics normalize
#' @importFrom utils head tail
#' @importFrom graphics legend lines
#' @importFrom stats as.ts ts predict
#' @importFrom magrittr %>%
#' @export
#'

RNN_ts=function(xt, xtlag=4, uRNN=2, Drate = 0, nEpochs = 10, Loss = "mse", AccMetrics = "mae", ActFn = "tanh", Split = 0.8, Valid = 0.1)
{
  lag_xt <- lagmatrix(as.ts(xt), lag = c(0:(xtlag)))
  series <- lag_xt
  series_all <- series[-c(1:xtlag), ]
  series <- series_all[, -1]

  feature<-ncol(series)
  aa <- 1/(max(series[, 1]) - min(series[, 1]))
  bb <- min(series[, 1])/(max(series[, 1]) - min(series[, 1]))
  norm <- function(x) {
    return((x - min(x))/(max(x) - min(x)))
  }
  denorm <- function(x) {
    return((x + bb)/aa)
  }
  transformed_data <- apply(series, 2, norm)
  transformed_train <- transformed_data[c(1:(nrow(transformed_data) *
                                               Split)), ]
  transformed_test <- transformed_data[-c(1:(nrow(transformed_data) *
                                               Split)), ]
  training_datapoints <- nrow(transformed_train)
  test_datapoints <- nrow(transformed_test)
  trainingset <- timeseries_generator(data = transformed_train,
                                      targets = transformed_train[, 1], length = 1, sampling_rate = 1,
                                      batch_size = 1)
  testset <- timeseries_generator(data = transformed_test,
                                  targets = transformed_test[, 1], length = 1, sampling_rate = 1,
                                  batch_size = 1)
  RNN_model <- keras_model_sequential() %>%
    layer_simple_rnn(units = uRNN,input_shape = c(1, feature), activation = ActFn,
                     dropout = Drate, return_sequences = TRUE) %>%
    layer_dense(units = 1)
  RNN_model %>% compile(optimizer = optimizer_rmsprop(), loss = Loss,
                        metrics = AccMetrics)
  summary(RNN_model)
  RNN_model_history <- RNN_model %>% fit(trainingset, batch_size = 1,
                                         epochs = nEpochs, validation.split = Valid)
  RNN_model %>% evaluate(trainingset)
  RNN_model_fitted_normal <- RNN_model %>% predict(trainingset)
  training_fitted_value <- denorm(RNN_model_fitted_normal)
  RNN_model %>% evaluate(testset)
  RNN_predicted_norm <- RNN_model %>% predict(testset)
  forecast <- denorm(RNN_predicted_norm)
  actual_data <- series_all[, 2]
  training_actual <- actual_data[c((1 + 1):training_datapoints)]
  testing_actual <- actual_data[c((1 + training_datapoints + 1):(training_datapoints +
                                                                   test_datapoints))]
  fcast_criteria <- matrix(nrow = 2, ncol = 2)
  fcast_criteria[1, 1] <- round(sqrt(mean((training_actual - training_fitted_value)^2)),
                                digits = 4)
  fcast_criteria[1, 2] <- round(mean(abs((training_actual - training_fitted_value)/training_actual)),
                                digits = 4)
  fcast_criteria[2, 1] <- round(sqrt(mean((testing_actual - forecast)^2)),
                                digits = 4)
  fcast_criteria[2, 2] <- round(mean(abs((testing_actual - forecast)/testing_actual)),
                                digits = 4)
  row.names(fcast_criteria) <- c("Training_set", "Testing_set")
  colnames(fcast_criteria) <- c("RMSE", "MAPE")
  Actualvsforecast_trainset=plot(ts(training_fitted_value),col="red",ylim=c(min(training_actual)-20,max(training_actual)+20), ylab="Value", main="Train Set",type="l", lwd=3)
  lines(ts(training_actual),col="black",type="l", lwd=3)
  legend("bottomright", c("Fitted", "Actual"), lty=c(1,1),
         inset=c(0,1), xpd=TRUE, horiz=TRUE, bty="n", col=c("red","black"))
  Actualvsforecast_testset=plot(ts(forecast),col="red",ylim=c(min(forecast)-20,max(testing_actual)+20), ylab="Value", main="Test Set", type="l", lwd=3)
  lines(ts(testing_actual),col="black",type="l", lwd=3)
  legend("bottomright", c("Forecast", "Actual"), lty=c(1,1),
         inset=c(0,1), xpd=TRUE, horiz=TRUE, bty="n", col=c("red","black"))

  return(list(TrainFittedValue = training_fitted_value, TestPredictedValue = forecast,
              fcast_criteria = fcast_criteria,plot_trainset=Actualvsforecast_trainset, plot_testset=Actualvsforecast_testset))
}

