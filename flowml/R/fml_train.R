#' @name fml_train
#' @author Kolja Becker
#' @title fml_train
#' @description Pipeline function that performs a hyper-parameter screeing experiment.
#'
#' @importFrom rjson fromJSON
#' @importFrom data.table fread
#' @importFrom tibble column_to_rownames
#' @importFrom caret train trainControl
#' @importFrom utils read.csv write.table
#' @importFrom stringr str_equal
#'
#' @include fml_grids.R
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
#' parser_inst$pipeline_segment <- "train"
#' parser_inst$config <- flowml::fml_example(file = "reg_config.json")
#' parser_inst$data <- flowml::fml_example(file = "reg_data.csv")
#' parser_inst$samples_train <- flowml::fml_example(file = "reg_samples_train.txt")
#' parser_inst$samples_test <- flowml::fml_example(file = "reg_samples_test.txt")
#' parser_inst$features <- flowml::fml_example(file = "reg_features.txt")
#' parser_inst$extended_features <- flowml::fml_example(file = "reg_features_extended.txt")
#' parser_inst$result_dir <- tempdir()
#'
#' flowml::fml_train(parser_inst = parser_inst)
#' }
#'
#' @export
#'
fml_train = function(parser_inst){
  # pass arguments
  file.config = parser_inst$config
  file.data = parser_inst$data
  file.samples.train = parser_inst$samples_train
  file.features.train = parser_inst$features

  # read config file
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

  # data
  # NOTE: using fread because it's faster
  df.data = data.table::fread(file.data) %>%
    tibble::column_to_rownames(config_inst$ml.sampleID)

  # samples
  list.samples = utils::read.csv(file.samples.train, header = FALSE)$V1

  # features
  list.features = utils::read.csv(file.features.train, header = FALSE)$V1

  # set up trainControl
  # TODO: implement other methods such as jackknife, bootstrap, ...
  # TODO: does trControl support balanced splitting?
  trControl = caret::trainControl(
    method = config_inst$ml.cv$method,
    number = as.numeric(config_inst$ml.cv$fold),
    repeats = as.numeric(config_inst$ml.cv$repeats))

  # train model
  set.seed(as.numeric(config_inst$ml.seed))

  if (config_inst$ml.interpret$caret.native.importance != FALSE){
    cv_model = caret::train(
      y = format_y(df.data[list.samples, config_inst$ml.response], config_inst$ml.type),
      x = df.data[list.samples, list.features, drop=FALSE],
      method = config_inst$ml.method,
      preProcess = config_inst$ml.preprocess,
      trControl = trControl,
      tuneGrid = list.grids[[config_inst$ml.cv$tune.grid]], # NOTE: if NULL tuneLength is used
      tuneLength = as.numeric(config_inst$ml.cv$tune.length),
      importance = config_inst$ml.interpret$caret.native.importance, # NOTE: values depend on the respective model
      verbosity = 0
      )
  } else {
    cv_model = caret::train(
      y = format_y(df.data[list.samples, config_inst$ml.response], config_inst$ml.type),
      x = df.data[list.samples, list.features, drop=FALSE],
      method = config_inst$ml.method,
      preProcess = config_inst$ml.preprocess,
      trControl = trControl,
      tuneGrid = list.grids[[config_inst$ml.cv$tune.grid]], # NOTE: if NULL tuneLength is used
      tuneLength = as.numeric(config_inst$ml.cv$tune.length),
      verbosity = 0
    )
  }

  # ml run time
  ml.run_time =
    cv_model$times$everything['elapsed'] + cv_model$times$final['elapsed']

  ### save

  # output files
  file.rds = sprintf("%s/%s.rds", parser_inst$result_dir, config_inst$fit.id)
  file.log = sprintf("%s/%s.log", parser_inst$result_dir, config_inst$fit.id)
  file.res = sprintf("%s/%s_resample.txt", parser_inst$result_dir, config_inst$fit.id)
  file.imp = sprintf("%s/%s_varimp.txt", parser_inst$result_dir, config_inst$fit.id)

  # model object
  saveRDS(cv_model, file.rds)

  # CV resampling table for best fit model
  data.table::fwrite(cv_model$resample, file.res)

  # log file
  list(
    name.out = config_inst$fit.id,
    file.data = file.data,
    file.samples.train = file.samples.train,
    file.features.train = file.features.train,
    ml.sampleID = config_inst$ml.sampleID,
    ml.seed = config_inst$ml.seed,
    ml.type = config_inst$ml.type,
    ml.method = config_inst$ml.method,
    ml.response = config_inst$ml.response,
    ml.preProcess = config_inst$ml.preprocess,
    ml.fold = config_inst$ml.cv$fold,
    ml.repeats = config_inst$ml.cv$repeats,
    ml.grid = config_inst$ml.cv$tune.grid,
    ml.caret.native.importance = config_inst$ml.interpret$caret.native.importance,
    ml.run_time = ml.run_time,
    note.log = config_inst$note
  ) %>%
    rjson::toJSON() %>%
    write(file = file.log)

  # this code gives a warning about row names.
  # utils::write.table(t(
  #   data.frame(
  #     name.out = config$fit.id,
  #     file.data = file.data,
  #     file.samples.train = file.samples.train,
  #     file.features.train = file.features.train,
  #     ml.sampleID = config$ml.sampleID,
  #     ml.seed = config$ml.seed,
  #     ml.type = config$ml.type,
  #     ml.method = config$ml.method,
  #     ml.response = config$ml.response,
  #     ml.preProcess = config$ml.preprocess,
  #     ml.fold = config$ml.cv$fold,
  #     ml.repeats = config$ml.cv$repeats,
  #     ml.grid = config$ml.cv$tune.grid,
  #     ml.run_time = ml.run_time,
  #     note.log = config$note)),
  #   file.log,
  #   row.names = FALSE,
  #   quote = FALSE,
  #   col.names = FALSE,
  #   sep='\t')

}
