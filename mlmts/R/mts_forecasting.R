
#' A forecasting procedure for MTS based on lag-embedding matrices
#'
#' \code{mts_forecasting} computes a general forecasting method for MTS based
#' on fitting standard regression models to lag-embedding matrices.
#'
#' @param X A list of MTS (numerical matrices).
#' @param max_lag The maximum lag considered to construct the lag-embedding matrices.
#' @param model_caret The corresponding regression model.
#' @param h The prediction horizon.
#' @return A list containing the \eqn{h}-step ahead forecast (matrix) for each
#' one of the MTS.
#' @examples
#' predictions <- mts_forecasting(RacketSports$data[1], model_caret = 'lm', h = 1)
#' # Obtaining the predictions for the first series in dataset RacketSports
#' # by using standard linear regression and a forecasting horizon of 1
#' predictions <- mts_forecasting(RacketSports$data[1], model_caret = 'rf', h = 3)
#' # Obtaining the predictions for the first series in dataset RacketSports
#' # by using the random forest and a forecasting horizon of 3
#' @details
#' This function performs a forecasting procedure based on lag-embedding
#' matrices. Given a list of MTS, it returns the corresponding list of \eqn{h}-step ahead
#' forecasts. We assume we want to forecast a given MTS \eqn{\boldsymbol X_T}
#' with certain univariate components
#' for a given forecasting horizon \eqn{h} and a maximum number of lags \eqn{L}.
#' For each component, the corresponding lag-embedded matrix is constructed
#' by considering the past information about that component and all the remaining
#' ones. The selected regression model is fitted to all the constructed matrices
#' (considering the last column as the response variables), and the fitted models
#' are used to construct the \eqn{h}-step ahead forecasts in a recursive manner.
#' @encoding UTF-8
#' @author
#' Ángel López-Oriona, José A. Vilar
#' @export


mts_forecasting <- function(X, max_lag = 1, model_caret = 'lm', h = 1) {


  # Computing the prediction matrix for each MTS

  list_predictions <- lapply(X, auxiliary_forecasting_function_4, max_lag = max_lag,
                             model_caret = model_caret, h = h)
  return(list_predictions)


}
