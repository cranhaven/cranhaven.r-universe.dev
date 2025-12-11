#' @name fml_interpret
#' @author Sebastian Malkusch
#' @title fml_interpret
#' @description Pipeline function that sets up and runs a post-hoc interpretation of an ml experiment.
#' All results are written to rds files.
#'
#' @importFrom rjson fromJSON
#' @importFrom data.table fread
#' @importFrom tibble column_to_rownames
#' @importFrom dplyr all_of mutate rename select
#' @importFrom vip vi_permute
#' @importFrom fastshap explain
#' @importFrom caret train trainControl
#' @importFrom utils read.csv
#' @importFrom stringr str_equal
#'
#' @include fml_parser.R
#' @include fml_format_response.R
#' @include fml_categorize.R
#'
#' @param parser_inst instance of fml_parser class that comprises command line arguments.
#' @return none
#'
#' @examples
#' \dontrun{
#' parser_inst <-  flowml::create_parser()
#'
#' parser_inst$pipeline_segment <- "interpret"
#' parser_inst$config <- flowml::fml_example(file = "reg_config.json")
#' parser_inst$data <- flowml::fml_example(file = "reg_data.csv")
#' parser_inst$samples_train <- flowml::fml_example(file = "reg_samples_train.txt")
#' parser_inst$samples_test <- flowml::fml_example(file = "reg_samples_test.txt")
#' parser_inst$features <- flowml::fml_example(file = "reg_features.txt")
#' parser_inst$extended_features <- flowml::fml_example(file = "reg_features_extended.txt")
#' parser_inst$trained <- flowml::fml_example(file = "reg_fit.rds")
#' parser_inst$interpretation <- "shap"
#' parser_inst$result_dir <- tempdir()
#'
#' flowml::fml_interpret(parser_inst = parser_inst)
#' }
#'
#' @export
#'
fml_interpret = function(parser_inst){
  # read config
  config_inst <- rjson::fromJSON(file = parser_inst$config)

  # omit default path in writing functions
  if(!dir.exists(parser_inst$result_dir))
  {
    stop(sprintf("result_dir does not exist: %s\n", parser_inst$result_dir))
  }
  if(stringr::str_equal(config_inst$fit.id, ""))
  {
    stop(sprintf("fit.id is empty\n"))
  }

  # read tuned model
  model_inst <- readRDS(file = parser_inst$trained)

  # read data
  data_df <- data.table::fread(parser_inst$data) %>%
    tibble::column_to_rownames(config_inst$ml.sampleID) %>%
    as.data.frame()

  # read samples
  samples_lst <- utils::read.csv(parser_inst$samples_train, header = FALSE)$V1

  # read features
  train_features_lst <- utils::read.csv(parser_inst$features, header = FALSE)$V1
  n_features <- length(train_features_lst)

  # filter data
  filtered_df <- data_df[samples_lst,] %>%
    dplyr::mutate(!!as.symbol(config_inst$ml.response) := format_y(!!as.symbol(config_inst$ml.response), config_inst$ml.type)) %>%
    dplyr::select(dplyr::all_of(append(train_features_lst, config_inst$ml.response)))


  # analysis parameters
  fml_resamples <- as.integer(config_inst$ml.interpret$n.repeats)
  fml_response <- config_inst$ml.response
  fml_mode <- config_inst$ml.type
  fml_metric <- switch(fml_mode,
                      "regression" = "rmse",
                      "classification" = "accuracy")
  fml_reference_class <- config_inst$ml.interpret$shap.reference.class
  fml_seed <- as.integer(config_inst$ml.seed)

  # run interpretation experiment.
  set.seed(fml_seed)

  pred_shap <- function(model, newdata) {
    switch(fml_mode,
           "classification" = {predict(model, newdata = newdata, type = "prob")[[fml_reference_class]]},
           "regression" = {predict(model, newdata = newdata)}
    )
  }

  interpret_inst <- switch (parser_inst$interpretation,
                            "permutation" = {
                              vip::vi_permute(object = model_inst,
                                              target = fml_response,
                                              metric = fml_metric,
                                              pred_wrapper = predict,
                                              train = filtered_df,
                                              smaller_is_better = TRUE,
                                              type = "difference",
                                              nsim = fml_resamples) %>%
                                dplyr::rename(Feature = Variable) %>%
                                return()
                            },
                            "shap" = {
                              fastshap::explain(object = model_inst,
                                                feature_names = train_features_lst,
                                                pred_wrapper = pred_shap,
                                                nsim = fml_resamples,
                                                X = as.data.frame(dplyr::select(filtered_df, dplyr::all_of(train_features_lst))),
                                                newdata = NULL,
                                                adjust = TRUE) %>%
                                return()
                            },
                            "internal" = {
                              vip::vi_model(object = model_inst) %>%
                                return()
                            },
                            stop(sprintf("Interpretation method %s is unknown. Needs to be permutation shap or internal.", parser_inst$interpretation))
  )

  # perform item categorization
  categorize_df <- run_abc_analysis(interpret_inst, parser_inst$interpretation)

  # save results to the multiQC section
  result_obj <- list("interpretation" = interpret_inst, "catrgorization" = categorize_df)

  path_to_result <- sprintf("%s/%s_interpretation_%s.rds",
                            parser_inst$result_dir,
                            parser_inst$interpretation,
                            config_inst$fit.id)

  saveRDS(result_obj, file = path_to_result)
}
