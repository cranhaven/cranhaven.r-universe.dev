#' Bayesian hierarchical generalized linear regression using Drift-Diffusion Model
#'
#' @description
#' `regddm` makes it easy to fit a single Bayesian hierarchical drift-diffusion
#' model (DDM) that estimates the DDM parameters of each subject and uses the
#' estimated parameters as variables in a generalized linear regression.
#'
#' @param data1 Subject-level dataframe with column such as age and gender. It
#'   must contain an `id` column unique for each subject.
#' @param data2 Trial-level dataframe. It must contain three columns: `id`,
#'   `response` and `rt`. It can also contain additional trial-level variables
#'   such as experiment condition.
#' @param model A list containing 0-5 formulas, specifying the dependence
#'   structure between variables.
#' @param family Family of distribution of `y`. Can be `gaussian`, `bernoulli`
#'   or `poisson`.
#' @param init Either `default` or other values supported by
#'   \code{\link[rstan]{stan}} function of \pkg{RStan}
#' @param prior A logistic value, specifying whether or not to use default prior
#'   for DDM parameters. By default, `prior` = TRUE.
#' @param stan_filename A string specifying the automatically generated \code{stan}
#'   file name. By default, an empty string `''` is provided. A temporary file
#'   will be created and deleted after the model is fit.
#' @param gen_model A logistic value indicating weather or not to generate the
#'   model. If not, RegDDM will not generate the code but use the existing \code{stan}
#'   code from \code{stan_filename} instead.
#' @param fit_model A logistic value indicating weather or not to fit the model.
#'   If not, RegDDM will only generate the code and return an unfitted
#'   \code{\link{regddmfit}} object.
#' @param warmup Number of warm-up iterations. Default is 500.
#' @param iter Number of iterations, which must be greater than \code{warmup}.
#'   Default value is 1000.
#' @param chains Number of chains to run for diagnosis. Default value is 4.
#' @param cores Number of cores to run the chains. It is best to make
#'   `cores = chains`.  Default value is 4.
#' @param ... Other parameters sent to \code{\link[rstan]{stan}} function of
#'   \pkg{RStan}.
#'
#' @return A \code{\link{regddmfit}} object.
#'
#' @import stats
#' @importFrom rlang .data
#' @export
#'
#' @references To be added
#'
#' @examples
#' # Note: each example takes about 20 minutes to run. During this period, you
#' # may not be able to open/save files or see the progress. To prevent this,
#' # it is recommended to copy, paste and run the example code in the console.
#'
#' \dontrun{
#' # Example analysis over the synthetic dataset.
#' data(regddm_data)
#' model = list(v ~ memload, iq ~ v_memload + v_0 + age + education)
#' regddm(
#'   regddm_data$data1,
#'   regddm_data$data2,
#'   model
#' )
#'
#' # Alternatively, subjects' DDM parameters can be used as the outcome.
#' model = list(v ~ memload, v_memload ~ gender)
#' regddm(
#'   regddm_data$data1,
#'   regddm_data$data2,
#'   model
#' )
#' }
regddm = function(
    data1,
    data2,
    model = list(),
    family = "gaussian",
    init = "default",
    prior = TRUE,
    stan_filename = "",
    gen_model = TRUE,
    fit_model = TRUE,
    warmup = 500,
    iter = 1000,
    chains = 4,
    cores = 4,
    ...
){
  # If we don't generate the model, a valid stan_filename must be provided
  if(gen_model == FALSE & stan_filename == ""){
    stop("if gen_model == FALSE, must provide stan_filename for the stan model")
  }
  if(gen_model == FALSE & !file.exists(stan_filename)){
    stop(paste0(stan_filename, " does not exist!"))
  }

  # convert all non-numeric columns into factors
  data1 = dplyr::mutate(data1, dplyr::across(dplyr::where(~ !is.numeric(.)), as.factor))
  data2 = dplyr::mutate(data2, dplyr::across(dplyr::where(~ !is.numeric(.)), as.factor))

  # remove variables that is not included in the model for simplicity
  used_vars = c()
  for(f in model){
    used_vars = c(used_vars, as.character(attr(terms(f), "variables"))[-1])
  }
  for(tmp_col in colnames(data2)){
    if(tmp_col %in% c("id","rt","response")){
      next
    }
    if(!tmp_col %in% used_vars){
      data2 = dplyr::select(data2, -dplyr::all_of(tmp_col))
    }
  }
  for(tmp_col in colnames(data1)){
    if(tmp_col == "id"){
      next
    }
    if(!tmp_col %in% used_vars){
      data1 = dplyr::select(data1, -dplyr::all_of(tmp_col))
    }
  }

  # check for errors in the data
  check_data(data1, data2)

  # parse the model into better formats
  model_parsed = parse_model(model, data1, data2)

  # format the data into a list required by rstan
  stan_data = format_data(data1, data2)

  # check for errors in the model
  check_model(stan_data$x_names, stan_data$c_names, model_parsed)

  # see if the automatically generated file should be deleted
  delete_flag = FALSE
  if(stan_filename == ""){
    stan_filename = stringr::str_c(tempdir(), "\\", sample.int(999999, size = 1),"_stan_model_tmp.stan")
    delete_flag = TRUE
  }

  # automatically generate the stan model
  if(gen_model){
    generate_model(
      stan_data,
      data1,
      model_parsed,
      prior,
      family,
      stan_filename
    )
  }
  rstan::stanc(stan_filename)

  # fit the stan model, if not, return the unfitted regddmfit
  if(!fit_model){
    if(delete_flag){
      file.remove(stan_filename)
    }
    return(regddmfit(data1, data2, model, family, NULL))
  }

  # find the initialization for MCMC sampling
  if(is.character(init) && init == "default"){
    init_list = generate_default_initialization(data1, data2, model_parsed)
    init = replicate(chains, init_list, simplify = FALSE)
  }

  stan_model <- stan_filename

  stan_fit <- rstan::stan(
    file = stan_model,
    data = stan_data$data,
    init = init,
    warmup = warmup,
    iter = iter,
    chains = chains,
    cores = cores,
    ...
  )

  # remove the temporary file if necessary
  if(delete_flag){
    file.remove(stan_filename)
  }

  # format the result into a regddmfit object.
  fit = regddmfit(data1, data2, model, family, stan_fit)

  return(fit)
}
