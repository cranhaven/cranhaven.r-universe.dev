#' @name fml_bootstrap
#' @author Sebastian Malkusch
#' @title fml_bootstrap
#' @description Pipeline function that sets up and runs a resampling experiment.
#' @details The experiment is run in parallel.
#' All results are written to files.
#'
#' @importFrom parallel detectCores
#' @importFrom future plan
#' @importFrom rjson fromJSON
#' @importFrom data.table fread
#' @importFrom tibble column_to_rownames tibble
#' @importFrom dplyr mutate select all_of
#' @importFrom furrr future_map furrr_options
#' @importFrom purrr map possibly
#' @importFrom tidyr unnest
#' @importFrom readr write_csv
#' @importFrom caret train trainControl
#' @importFrom stringr str_flatten str_equal
#' @importFrom rlang :=
#' @importFrom utils read.csv write.table
#'
#' @include fml_parser.R
#' @include fml_resampler.R
#' @include fml_format_response.R
#' @include fml_resample.R
#'
#' @param parser_inst Instance of fml_parser class that comprises command line arguments.
#' @return none
#'
#' @examples
#' \dontrun{
#' parser_inst <-  flowml::create_parser()
#'
#' parser_inst$pipeline_segment <- "bootstrap"
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
#' flowml::fml_bootstrap(parser_inst = parser_inst)
#' }
#'
#' @export
#'
fml_bootstrap = function(parser_inst){
  # set up environment for parallel computing
  n_cores <- parallel::detectCores()
  if(parser_inst$cores == 1){
    future::plan(strategy = "sequential")
  }
  else if(n_cores < parser_inst$cores){
    future::plan(strategy = "multisession", workers = n_cores)
  }else{
    future::plan(strategy = "multisession", workers = parser_inst$cores)
  }

  # define start time
  start_time <- Sys.time()

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

  # read model
  model_inst <- readRDS(file = parser_inst$trained)

  # read data
  data_df <- data.table::fread(parser_inst$data) %>%
    tibble::column_to_rownames(config_inst$ml.sampleID) %>%
    as.data.frame()

  # read samples
  samples_lst <- utils::read.csv(parser_inst$samples_train, header = FALSE)$V1

  # read features
  train_features_lst <- utils::read.csv(parser_inst$features, header = FALSE)$V1
  resample_features_lst <- c()
  if(parser_inst$permutation == "features"){
    resample_features_lst <- utils::read.csv(parser_inst$extended_features, header = FALSE)$V1
  }
  complete_features_lst <- append(train_features_lst, resample_features_lst)
  n_features <- length(train_features_lst)
  # filter data
  filtered_df <- switch (parser_inst$permutation,
                         'none' = {
                           data_df[samples_lst,] %>%
                             dplyr::mutate(!!as.symbol(config_inst$ml.response) := format_y(!!as.symbol(config_inst$ml.response), config_inst$ml.type)) %>%
                             dplyr::select(dplyr::all_of(append(train_features_lst, config_inst$ml.response))) %>%
                             return()
                         },
                         'response' = {
                           data_df[samples_lst,] %>%
                             dplyr::mutate(!!as.symbol(config_inst$ml.response) := format_y(!!as.symbol(config_inst$ml.response), config_inst$ml.type)) %>%
                             dplyr::select(dplyr::all_of(append(train_features_lst, config_inst$ml.response))) %>%
                             return()
                         },
                         'features' = {
                           data_df[samples_lst,] %>%
                             dplyr::mutate(!!as.symbol(config_inst$ml.response) := format_y(!!as.symbol(config_inst$ml.response), config_inst$ml.type)) %>%
                             dplyr::select(dplyr::all_of(append(complete_features_lst, config_inst$ml.response))) %>%
                             return()
                         },
                         stop(sprintf("Permutation method %s is unknown. Needs to be none, features or response.", parser_inst$permutation))
  )


  # run permutation experiment
  bootstrap_df <- tibble::tibble(permutations = seq(as.integer(config_inst$ml.bootstrap$n.permutations)), seed = as.integer(config_inst$ml.seed) + seq(as.integer(config_inst$ml.bootstrap$n.permutations))) %>%
    dplyr::mutate(permutation_type = parser_inst$permutation) %>%
    dplyr::mutate(resample_obj = furrr::future_map(.options = furrr::furrr_options(seed = TRUE), .x = seed, .f = purrr::possibly(.f = create_resample_experiment, otherwise = NULL), filtered_df, parser_inst,  model_inst, config_inst, n_features)) %>%
    dplyr::mutate(metrics = purrr::map(.x = resample_obj, .f = purrr::possibly(.f = function(x){x$metrics_df}, otherwise = NULL))) %>%
    dplyr::mutate(confusion = purrr::map(.x = resample_obj, .f = purrr::possibly(.f = function(x){x$confusion_df}, otherwise = NULL)))

  # extract metrics
  metrics_df <- bootstrap_df %>%
    dplyr::select(permutation_type, permutations, seed, metrics) %>%
    tidyr::unnest(metrics)

  # This result will go to the multiQC section
  path_to_metrics_file <- sprintf("%s/%s_permute_%s_bootstrap_metrics.csv",
                                  parser_inst$result_dir,
                                  config_inst$fit.id,
                                  parser_inst$permutation)

  readr::write_csv(metrics_df, path_to_metrics_file)

  # write confusion
  if(config_inst$ml.type == "classification"){
    confusion_df <- bootstrap_df %>%
      dplyr::select(permutation_type, permutations, seed, confusion) %>%
      tidyr::unnest(confusion)
    # This result will go to the multiQC section
    path_to_confusion_file <- sprintf("%s/%s_permute_%s_bootstrap_confusion.csv",
                                      parser_inst$result_dir,
                                      config_inst$fit.id,
                                      parser_inst$permutation)

    readr::write_csv(confusion_df, path_to_confusion_file)
  }

  # log computation time
  end_time = Sys.time()
  run_time = end_time - start_time

  # save experimental conditions
  # may be redundant, same as log in fml_train

  extended_features_file <- "null"
  if(!is.null(parser_inst$extended_features)){
    extended_features_file <- parser_inst$extended_features
  }

  file.log = sprintf("%s/%s_permute_%s_bootstrap.log",
                     parser_inst$result_dir,
                     config_inst$fit.id,
                     parser_inst$permutation)

  list(name.out = config_inst$fit.id,
       file.data = parser_inst$data,
       file.samples.train = parser_inst$samples,
       file.features.train = parser_inst$features,
       file.features.resample = extended_features_file,
       ml.model = parser_inst$trained,
       ml.seed = config_inst$ml.seed,
       ml.type = config_inst$ml.type,
       ml.method = config_inst$ml.method,
       ml.response = config_inst$ml.response,
       ml.preProcess = stringr::str_flatten(config_inst$ml.preprocess, collapse = "; "),
       boot.permutation.method = parser_inst$permutation,
       boot.n.resamples = config_inst$ml.bootstrap$n.resamples,
       boot.n.permutations = config_inst$ml.bootstrap$n.permutations,
       boot.n.cores = parser_inst$cores,
       boot.run_time = sprintf("%.3f", run_time),
       note.log = config_inst$note) %>%
    rjson::toJSON() %>%
    write(file = file.log)

}

