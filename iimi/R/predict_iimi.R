#' @title predict_iimi()
#'
#' @export
#' @importFrom stats predict
#' @importFrom stats model.matrix
#' @importFrom mltools sparsify
#' @importFrom data.table data.table
#' @importFrom dplyr %>% summarise group_by
#' @importFrom xgboost xgb.load.raw
#'
#' @examples
#'
#' \dontrun{df <- convert_rle_to_df(example_cov)
#' predictions <- predict_iimi(df)
#' }
#'
#'
#'
#' @description Uses a machine learning model to predict the infection status
#'    for the plant sample(s). User can use their own model if needed.
#'
#' @param newdata A matrix or data frame that contains the features extracted
#'    from the coverage profile using `convert_bam_to_cov()`.
#' @param method The machine learning method of choice, `rf`,
#'    `xgb`, or `en`. `rf` stands for random forest model; `xgb` stands for
#'    XGBoost model; and `en` stands for elastic net model.
#' @param trained_model The trained model. If not provided, default model is
#'    used.
#' @param report_virus_level If `TRUE`, the function returns the aggregated
#'    results based on the virus. If `FALSE`, the function returns the
#'    unaggregated results based on segment level with each decision's
#'    probability decided by the model. We do not recommended to set this to
#'    `FALSE`.
#'
#' @return A data frame of diagnostics result for each sample


predict_iimi <-
  function(newdata,
           method = "xgb",
           trained_model,
           report_virus_level = TRUE) {
    if (method == "rf") {
      if (missing(trained_model)) {
        model = trained_rf
      } else {
        model = trained_model
      }
      prediction = predict(newdata = newdata[, -c(1:4)],
                           model,
                           importance = T,
                           type = "prob")

      result_df <- data.frame(
        Sample_ID = newdata$sample_id,
        Virus_name = newdata$virus_name,
        Isolate_ID = newdata$iso_id,
        Segment_ID = newdata$seg_id,
        Prediction = prediction[, "TRUE"] > 0.5,
        Probability = prediction[, "TRUE"]
      )
      if (report_virus_level) {
        result_df <- result_df %>% group_by(Sample_ID, Virus_name) %>%
          summarise(
            Virus_name = dplyr::first(Virus_name),
            Segment_ID = dplyr::first(Segment_ID),
            Isolate_ID = dplyr::first(Isolate_ID),
            Prediction = any(as.logical(Prediction))
          )
      }


    }

    if (method == "xgb") {
      test = sparsify(data.table(newdata[, -c(1:4)]))

      if (missing(trained_model)) {
        model = xgb.load.raw(trained_xgb)
      } else {
        model = trained_model
      }

      prediction = predict(newdata = test, model)

      result_df <- data.frame(
        Sample_ID = newdata$sample_id,
        Virus_name = newdata$virus_name,
        Isolate_ID = newdata$iso_id,
        Segment_ID = newdata$seg_id,
        Prediction = prediction > 0.5,
        Probability = prediction
      )
      if (report_virus_level) {
        result_df <- result_df %>% group_by(Sample_ID, Virus_name) %>%
          summarise(
            Virus_name = dplyr::first(Virus_name),
            Segment_ID = dplyr::first(Segment_ID),
            Isolate_ID = dplyr::first(Isolate_ID),
            Prediction = any(as.logical(Prediction))
          )
      }
    }

    if (method == "en") {
      xx.test = cbind(rep(1, nrow(newdata)), newdata[, -c(1:4)])
      colnames(xx.test)[1] = "labels"
      test = model.matrix(labels ~ ., xx.test)[,-1]

      if (missing(trained_model)) {
        model = trained_en
      } else {
        model = trained_model
      }

      prediction = predict(model,
                           newx = test,
                           s = "lambda.1se",
                           type = "response")

      result_df <- data.frame(
        Sample_ID = newdata$sample_id,
        Virus_name = newdata$virus_name,
        Isolate_ID = newdata$iso_id,
        Segment_ID = newdata$seg_id,
        Prediction = prediction > 0.5,
        Probability = prediction
      )
      colnames(result_df)[5:6] = c("Prediction", "Probability")
      if (report_virus_level) {
        result_df <- result_df %>% group_by(Sample_ID, Virus_name) %>%
          summarise(
            Virus_name = dplyr::first(Virus_name),
            Segment_ID = dplyr::first(Segment_ID),
            Isolate_ID = dplyr::first(Isolate_ID),
            Prediction = any(as.logical(Prediction))
          )
      }
    }

    if (method %in% c("rf", "xgb", "en") == F) {
      stop("`method` must be `rf`, `xgb`, or `en`.")
    }

    result_df
  }
