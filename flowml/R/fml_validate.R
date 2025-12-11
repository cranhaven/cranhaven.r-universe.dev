#' @name fml_validate
#' @author Kolja Becker
#' @title fml_validate
#' @description Pipeline function that performs a validation experiment on a
#' a caret train object based on test samples.
#'
#' @importFrom dplyr bind_rows
#' @importFrom rjson fromJSON
#' @importFrom data.table data.table fread fwrite
#' @importFrom tibble column_to_rownames
#' @importFrom caret postResample train trainControl
#' @importFrom utils read.csv
#' @importFrom stats predict
#' @importFrom stringr str_equal
#'
#' @include fml_format_response.R
#' @include fml_parser.R
#'
#' @param parser_inst instance of fml_parser class that comprises command line arguments.
#' @return none
#'
#' @examples
#' \dontrun{
#' parser_inst <-  flowml::create_parser()
#'
#' parser_inst$pipeline_segment <- "validate"
#' parser_inst$config <- flowml::fml_example(file = "reg_config.json")
#' parser_inst$data <- flowml::fml_example(file = "reg_data.csv")
#' parser_inst$samples_train <- flowml::fml_example(file = "reg_samples_train.txt")
#' parser_inst$samples_test <- flowml::fml_example(file = "reg_samples_test.txt")
#' parser_inst$features <- flowml::fml_example(file = "reg_features.txt")
#' parser_inst$extended_features <- flowml::fml_example(file = "reg_features_extended.txt")
#' parser_inst$trained <- flowml::fml_example(file = "reg_fit.rds")
#' parser_inst$permutation <- "none"
#' parser_inst$result_dir <- tempdir()
#'
#' flowml::fml_validate(parser_inst = parser_inst)
#' }
#'
#' @export
#'
fml_validate = function(parser_inst){
  # read config file
  file.data = parser_inst$data
  file.config = parser_inst$config
  file.trained_model = parser_inst$trained
  list.samples.test = parser_inst$samples_test

  # load config
  config_inst = rjson::fromJSON(file = file.config)

  # omit default path in writing functions
  if(!dir.exists(parser_inst$result_dir))
  {
    stop(sprintf("result_dir does not exist: %s\n", parser_inst$result_dir))
  }
  if(stringr::str_equal(config_inst$fit.id, ""))
  {
    stop(sprintf("fit.id is empty\n"))
  }

  # load trained model
  cv_model = readRDS(file.trained_model)

  # data
  # NOTE: using fread because it's faster
  df.data = data.table::fread(file.data) %>%
    tibble::column_to_rownames(config_inst$ml.sampleID)

  # features
  list.features = colnames(cv_model$trainingData)[-ncol(cv_model$trainingData)]

  # prepare list with test samples
  list.test = lapply(list.samples.test, function(x)
    # read in samples test
    utils::read.csv(x, header = FALSE)$V1) %>%
    # assign names
    magrittr::set_names(lapply(list.samples.test, function(x) x))

  # evaluate on test data
  df.eval = lapply(list.test, function(x){
    # predict
    y.model = stats::predict(cv_model, df.data[x, list.features])
    # data vector
    y.data = format_y(df.data[x, config_inst$ml.response], config_inst$ml.type)
    # performance evaluation
    caret::postResample(y.model, y.data)}) %>%
    dplyr::bind_rows(.id='test_data') %>%
    data.table::data.table()

  # write output
  path_to_eval_result = sprintf("%s/%s_eval.csv", parser_inst$result_dir, config_inst$fit.id)
  data.table::fwrite(df.eval, path_to_eval_result)

}
