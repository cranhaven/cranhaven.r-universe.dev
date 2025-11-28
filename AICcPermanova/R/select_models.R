#' Select models based on AICc and VIF.
#'
#' This function selects models from a data frame based on the AICc and VIF values. Models with AICc greater than negative infinity and VIF less than or equal to 6 are considered. The difference in AICc values for each model is calculated with respect to the model with the minimum AICc. Models with a difference in AICc less than or equal to the specified delta_aicc value are selected.
#'
#' @param df a data frame containing the models to select from.
#' @param delta_aicc a numeric value specifying the maximum difference in AICc values allowed.
#' @return a data frame containing the selected models and the AIC weights.
#' @examples
#' df <- data.frame(AICc = c(10, 12, 15, 20), max_vif = c(2, 4, 5, 6))
#' select_models(df)
#' select_models(df, delta_aicc = 5)
#' @importFrom data.table setDT .SD
#' @export

select_models <- function(df, delta_aicc = 2){
  AICc <- DeltaAICc <- max_vif <- AICWeight <- NULL
  Result <- data.table::setDT(df)[AICc > -Inf & max_vif <= 5,
                      DeltaAICc := AICc - min(AICc)][DeltaAICc <= delta_aicc][, AICWeight := exp( -0.5*DeltaAICc)/sum(exp( -0.5*DeltaAICc))] |>
    as.data.frame()
  # remove columns with only NAs
  Result <- Result[, colSums(is.na(Result)) != nrow(Result), drop = FALSE]
  return(Result)
}
