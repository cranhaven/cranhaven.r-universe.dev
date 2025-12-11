#' @name create_resample_experiment
#' @author Sebastian Malkusch
#' @title create_resample_experiment
#' @description Creates an object of a resampling experiment.
#' @details Creates a resampling experiment.
#' It uses user defined parameters to set up the experiment.
#' It creates an instance of the Resampler object
#' and runs the experiment according to the user-defined parameters.
#'
#' @include fml_resampler.R
#'
#' @param seed sets the seed for the random number generator to guarantee reproducibility.
#' (int)
#' @param data_df data frame to be learned from.
#' (tibble::tibble)
#' @param parser_inst instance of parser object. (optparse::parse_args).
#' @param model_inst instance of caret_train object (caret::train).
#' @param config_inst list of config options (list).
#' @param n_features number of features (int).
#' @return An instance of type 'Resampler'.
create_resample_experiment = function(seed, data_df, parser_inst,  model_inst, config_inst, n_features){
  #set seed
  set.seed(seed)

  # create Resampler instance
  resampler_inst = Resampler$new(permute = parser_inst$permutation,
                                 n_resample = as.integer(config_inst$ml.bootstrap$n.resamples),
                                 fml_method = model_inst$method,
                                 fml_type = config_inst$ml.type,
                                 hyper_parameters = model_inst$bestTune,
                                 pre_process_lst = config_inst$ml.preprocess,
                                 response_var = config_inst$ml.response,
                                 n_features = as.integer(n_features),
                                 strata_var = NULL)
  # strata_var is not yet implemented!! How to handle empty variable?

  # Train model
  resampler_inst$fit(data_df = data_df)

  # return trained object
  return(resampler_inst)
}
