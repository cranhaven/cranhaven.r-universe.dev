


#' Estimate growth parameters from the model fit 
#'
#' @description The \strong{growthparameters()} computes population
#'   average and and individual-specific growth parameters (such as age at peak
#'   growth velocity) and the uncertainty (standard error, SE and the credible
#'   interval, CI).
#'
#' @details The \strong{growthparameters()} internally calls the
#'   [fitted_draws()] or the [predict_draws()] function to estimate the first
#'   derivative based growth parameters for each posterior draw. The growth
#'   parameters estimated are age at peak growth velocity (APGV), peak growth
#'   velocity (PGV), age at takeoff growth velocity (ATGV), takeoff growth
#'   velocity (TGV), age at cessation of growth velocity (ACGV), and the
#'   cessation growth velocity (CGV). The APGV and PGV are estimated by calling
#'   the [sitar::getPeak()] function whereas the ATGV and TGV are estimated by
#'   using the [sitar::getTakeoff()] function. The [sitar::getTrough()] function
#'   is used to estimates ACGV and CGV parameters. The parameters obtained from
#'   each posterior draw are then summarized appropriately to get the estimates
#'   and the uncertainty (SEs and CIs) around these estimates. Please note that
#'   it is not always possible to estimate cessation and takeoff growth
#'   parameters when there are no distinct pre-peak or post-peak troughs.
#'
#'
#' @param model An object of class \code{bgmfit}.
#' 
#' @param newdata An optional data frame to be used in estimation. If
#'   \code{NULL} (default), the \code{newdata} is retrieved from the
#'   \code{model}.
#' 
#' @param resp A character string (default \code{NULL}) to specify response
#'   variable when processing posterior draws for the \code{univariate_by} and
#'   \code{multivariate} models. See [bsitar::bsitar()] for details on
#'   \code{univariate_by} and \code{multivariate} models
#'   
#' @param ndraws A positive integer indicating the number of posterior draws to
#'   be used in estimation. If \code{NULL} (default), all draws are used.
#'   
#' @param draw_ids An integer indicating the specific posterior draw(s) 
#' to be used in estimation (default \code{NULL}).
#'   
#' @param summary A logical indicating whether only the estimate should be
#'   computed (\code{TRUE}, default), or estimate along with SE and CI should be
#'   returned (\code{FALSE}). Setting \code{summary} as \code{FALSE} will
#'   increase the computation time.
#'
#' @param robust A logical to specify the summarize options. If \code{FALSE}
#'   (the default) the mean is used as the measure of central tendency and the
#'   standard deviation as the measure of variability. If \code{TRUE}, the
#'   median and the median absolute deviation (MAD) are applied instead. Ignored
#'   if \code{summary} is \code{FALSE}.
#'   
#' @param re_formula Option to indicate whether or not to include the
#'   individual/group-level effects in the estimation. When \code{NA} (default),
#'   the individual-level effects are excluded and therefore population average
#'   growth parameters are computed. When \code{NULL}, individual-level effects
#'   are included in the computation and hence the growth parameters estimates
#'   returned are individual-specific. In both situations, (i.e,, \code{NA} or
#'   \code{NULL}), continuous and factor covariate(s) are appropriately included
#'   in the estimation. The continuous covariates by default are set to their
#'   means (see \code{numeric_cov_at} for details) whereas factor covariates are
#'   left unaltered thereby allowing estimation of covariate specific population
#'   average and individual-specific growth parameter.
#'   
#' @param peak A logical (default \code{TRUE}) to indicate whether or
#'   not to calculate the age at peak velocity (APGV) and the peak velocity
#'   (PGV) parameters.
#'   
#' @param takeoff  A logical (default \code{FALSE}) to indicate whether
#'   or not to calculate the age at takeoff velocity (ATGV) and the takeoff
#'   growth velocity (TGV) parameters.
#'
#' @param trough A logical (default \code{FALSE}) to indicate whether or
#'   not to calculate the age at cessation of growth velocity (ACGV) and the
#'   cessation of growth velocity (CGV) parameters.
#' 
#' @param acgv A logical (default \code{FALSE}) to indicate whether or not to
#'   calculate the age at cessation of growth velocity from the velocity curve.
#'   If \code{TRUE}, age at cessation of growth velocity (ACGV) and the
#'   cessation growth velocity (CGV) are  calculated based on the percentage of
#'   the peak growth velocity as defined by the \code{acgv_velocity} argument
#'   (see below). The \code{acgv_velocity} is typically set at 10 percent of the
#'   peak growth velocity. The ACGV and CGV are calculated along with the the
#'   uncertainty (SE and CI) around the ACGV and CGV parameters.
#'   
#' @param acgv_velocity Specify the percentage of the peak growth velocity to be 
#'  used when estimating \code{acgv}. The default value is \code{0.10} i.e., 
#'  10 percent of the peak growth velocity.
#'   
#' @param estimation_method A character string to specify the estimation method
#'   when calculating the velocity from the posterior draws. The \code{'fitted'}
#'   method internally calls the [bsitar::fitted_draws()] whereas the option
#'   \code{predict} calls the [bsitar::predict_draws()]. See
#'   [brms::fitted.brmsfit()] and [brms::predict.brmsfit()] for derails.
#'   
#' @param numeric_cov_at An optional (named list) argument to specify the value
#'   of continuous covariate(s). The default \code{NULL} option set the
#'   continuous covariate(s) at their mean. Alternatively, a named list can be
#'   supplied to manually set these values. For example, \code{numeric_cov_at =
#'   list(xx = 2)} will set the continuous covariate varibale 'xx' at 2. The
#'   argument \code{numeric_cov_at} is ignored when no continuous covariate is
#'   included in the model.
#'   
#' @param levels_id An optional argument to specify the \code{ids} for
#'   hierarchical model (default \code{NULL}). It is used only when model is
#'   applied to the data with 3 or more levels of hierarchy. For a two level
#'   model, the \code{levels_id} is automatically inferred from the model fit.
#'   Even for 3 or higher level model, the \code{levels_id} is inferred from the
#'   model fit but under the assumption that hierarchy is specified from lowest
#'   to upper most level i.e, \code{id} followed by \code{study} where \code{id}
#'   is nested within the \code{study} Note that it is not guaranteed that the
#'   \code{levels_id} is sorted correctly, and therefore it is better to set it
#'   manually when fitting a model with three or more levels of hierarchy.
#'   
#' @param avg_reffects An optional argument (default \code{NULL}) to calculate
#'   (marginal/average) curves and growth parameters (such as APGV and PGV). If
#'   specified, it must be a named list indicating the \code{over} (typically
#'   level 1 predictor, such as age), \code{feby} (fixed effects, typically a
#'   factor variable), and  \code{reby} (typically \code{NULL} indicating that
#'   parameters are integrated over the random effects) such as
#'   \code{avg_reffects = list(feby = 'study', reby = NULL, over = 'age'}.
#'   
#'@param aux_variables An optional argument to specify the variable(s) that can
#'  be passed to the \code{ipts} argument (see below). This is useful when
#'  fitting location scale models and measurement error models. An
#'  indication to use \code{aux_variables} is when post processing functions
#'  throw an error such as \code{variable 'x' not found either 'data' or
#'  'data2'}
#'   
#' @param ipts An integer to set the length of the predictor variable to get a
#'   smooth velocity curve. The \code{NULL} will return original values whereas
#'   an integer such as \code{ipts = 10} (default) will interpolate the
#'   predictor. It is important to note that these interpolations do not alter
#'   the range of predictor when calculating population average and/or the
#'   individual specific growth curves.
#'   
#' @param deriv_model A logical to specify whether to estimate velocity curve
#'   from the derivative function, or the differentiation of the distance curve.
#'   The argument \code{deriv_model} is set to \code{TRUE} for those functions
#'   which need velocity curve such as \code{growthparameters()} and
#'   \code{plot_curves()}, and \code{NULL} for functions which explicitly use
#'   the distance curve (i.e., fitted values) such as \code{loo_validation()}
#'   and \code{plot_ppc()}.
#' 
#' @param conf A numeric value (default \code{0.95}) to compute CI. Internally,
#'   the \code{conf} is translated into a paired probability values as
#'   \code{c((1 - conf)/2, 1 - (1 - conf) / 2)}. For \code{conf = 0.95}, this
#'   will compute 95% CI and the variables with lower and upper limits will be
#'   named as \code{Q.2.5} and \code{Q.97.5}.
#'   
#' @param xrange An integer to set the predictor range (i.e., age) when
#'   executing the interpolation via \code{ipts}. The default \code{NULL} sets
#'   the individual specific predictor range whereas code \code{xrange = 1} sets
#'   identical range for individuals within the same higher grouping variable
#'   (e.g., study). Code \code{xrange  = 2} sets the identical range across the
#'   entire sample. Lastly, a paired numeric values can be supplied e.g.,
#'   \code{xrange = c(6, 20)} to set the range within those values.
#'   
#' @param  xrange_search A vector of length two, or a character string
#'   \code{'range'} to set the range of predictor variable (\code{x} ) within
#'   which growth parameters are searched. This is useful when there is more
#'   than one peak and user wants to summarize peak within a given range of the
#'   \code{x} variable. Default \code{xrange_search = NULL}.
#'   
#' @param digits An integer (default \code{2}) to set the decimal argument for
#'   the [base::round()] function.
#' 
#' @param seed An integer (default \code{123}) that is passed to the estimation
#'   method.
#'   
#' @param future A logical (default \code{FALSE}) to specify whether or not to
#'   perform parallel computations. If set to \code{TRUE}, the
#'   [future.apply::future_sapply()] function is used to summarize draws.
#'   
#' @param future_session A character string to set the session type when
#'   \code{future = TRUE}. The \code{'multisession'} (default) options sets the
#'   multisession whereas the \code{'multicore'} sets the multicore session.
#'   Note that option \code{'multicore'} is not supported on Windows systems.
#'   For more details, see [future.apply::future_sapply()].
#'   
#' @param cores Number of cores to be used when running the parallel
#'   computations (if \code{future = TRUE}). On non-Windows systems this
#'   argument can be set globally via the mc.cores option. For the default
#'   \code{NULL} option, the number of cores are set automatically by calling
#'   the [future::availableCores()]. The number of cores used are the maximum
#'   number of cores avaialble minus one, i.e., \code{future::availableCores() -
#'   1}.
#'  
#' @param parms_eval A logical to specify whether or not to get growth
#'   parameters on the fly. This is for internal use only and mainly needed for
#'   compatibility across internal functions.
#' 
#' @param idata_method A character string to indicate the interpolation method.
#'   The number of of interpolation points is set up the \code{ipts} argument.
#'   Options available for \code{idata_method} are \emph{method 1} (specified as
#'   \code{'m1'}) and \emph{method 2} (specified as \code{'m2'}). The
#'   \emph{method 1} (\code{'m1'}) is adapted from the the \pkg{iapvbs} package
#'   and is documented here
#'    <https://rdrr.io/github/Zhiqiangcao/iapvbs/src/R/exdata.R>
#'   whereas \emph{method 2} (\code{'m2'}) is based on the \pkg{JMbayes}
#'   package as documented here
#'   <https://github.com/drizopoulos/JMbayes/blob/master/R/dynPred_lme.R>.
#'  The \code{'m1'} method works by internally constructing the data frame based
#'  on the model configuration whereas the method \code{'m2'} uses the exact
#'  data frame used in model fit and can be accessed via \code{fit$data}. If
#'  \code{idata_method = NULL, default}, then method \code{'m2'} is
#'  automatically set. Note that method \code{'m1'} might fail in some cases
#'  when model involves covariates particularly when model is fit as
#'  \code{univariate_by}. Therefore, it is advised to switch to method
#'  \code{'m2'} in case \code{'m1'} results in error.
#'  
#' @param parms_method A character to specify the method used to when evaluating
#'   \code{parms_eval}. The default is \code{getPeak} which uses the
#'   [sitar::getPeak()] function from the \code{sitar} package. The alternative
#'   option is \code{findpeaks} that uses the [pracma::findpeaks()] function
#'   function from the \code{pracma} package. This is for internal use only and
#'   mainly needed for compatibility across internal functions.
#'   
#' @param verbose An optional argument (logical, default \code{FALSE}) to
#'  indicate whether to print information collected during setting up the 
#'  object(s). 
#'  
#' @param fullframe A logical to indicate whether to return \code{fullframe}
#'   object in which \code{newdata} is bind to the summary estimates. Note that
#'   \code{fullframe} can not be combined with \code{summary = FALSE}.
#'   Furthermore, \code{fullframe} can only be used when \code{idata_method =
#'   'm2'}. A particular use case is when fitting \code{univariate_by} model.
#'   The \code{fullframe} is mainly for internal use only. 
#'   
#' @param dummy_to_factor A named list (default \code{NULL}) that is used to
#'   convert dummy variables into a factor variable. The named elements are
#'   \code{factor.dummy}, \code{factor.name}, and \code{factor.level}. The
#'   \code{factor.dummy} is a vector of character strings that need to be
#'   converted to a factor variable whereas the \code{factor.name} is a single
#'   character string that is used to name the newly created factor variable.
#'   The \code{factor.level} is used to name the levels of newly created factor.
#'   When \code{factor.name} is \code{NULL}, then the factor name is internally
#'   set as \code{'factor.var'}. If \code{factor.level} is \code{NULL}, then
#'   names of factor levels are take from the \code{factor.dummy} i.e., the
#'   factor levels are assigned same name as \code{factor.dummy}. Note that when
#'   \code{factor.level} is not \code{NULL}, its length must be same as the
#'   length of the \code{factor.dummy}.
#' 
#' @param usesavedfuns A logical (default \code{FALSE}) to indicate whether to
#'   use the already exposed and saved \code{Stan} functions. This is for
#'   internal purposes only and mainly used during the testing of the functions
#'   and therefore should not be used by users as it might lead to unreliable
#'   estimates.
#' 
#' @param clearenvfuns A logical to indicate whether to clear the exposed
#'   function from the environment (\code{TRUE}) or not (\code{FALSE}). If
#'   \code{NULL} (default), then \code{clearenvfuns} is set as \code{TRUE} when
#'   \code{usesavedfuns} is \code{TRUE}, and \code{FALSE} if \code{usesavedfuns}
#'   is \code{FALSE}.
#'  
#' @param envir Environment used for function evaluation. The default is
#'   \code{NULL} which will set \code{parent.frame()} as default environment.
#'   Note that since most of post processing functions are based on \pkg{brms},
#'   the functions needed for evaluation should be in the \code{.GlobalEnv}.
#'   Therefore, it is strongly recommended to set \code{ envir = globalenv()}
#'   (or \code{envir = .GlobalEnv}). This is particularly true for the
#'   derivatives such as velocity curve.
#'   
#' @param ... Further arguments passed to \code{brms::fitted.brmsfit()} and
#'   \code{brms::predict()} functions.
#'
#' @return A data frame with either five columns (when \code{summary = TRUE}),
#'   or two columns when \code{summary = False} (assuming \code{re_formual =
#'   NULL}). The first two columns common to each scenario (\code{summary =
#'   TRUE/False}) are \code{'Parameter'} and \code{'Estimate'} which define the
#'   name of the growth parameter (e.g., APGV, PGV etc), and estimate. When 
#'   \code{summary = TRUE}, the three additional columns are \code{'Est.Error'}, 
#'   and a paired vector of names defining the lower and upper limits of the 
#'   CIs. The CI columns are named as Q with appropriate suffix taken from the 
#'   percentiles used to construct these intervals (such as  \code{Q.2.5} and 
#'   \code{Q.97.5} where\code{2.5} and  \code{97.5} are the  \code{0.025} and 
#'   \code{0.975} percentiles used to compute by the 95% CI by calling the 
#'   quantile function. When \code{re_formual = NULL}, an additional column is 
#'   added that denotes the individual identifier (typically \code{id}).
#'   
#' @export growthparameters.bgmfit
#' @export
#'
#' @importFrom rlang .data
#' 
#' @inherit brms::prepare_predictions.brmsfit params 
#'
#' @inherit berkeley author
#'
#' @examples
#' 
#' # Fit Bayesian SITAR model 
#' 
#' # To avoid mode estimation which takes time, a model fitted to the 
#' # 'berkeley_mdata' has already been saved as 'berkeley_mfit'. 
#' # Details on 'berkeley_mdata' and 'berkeley_mfit' are provided in the 
#' # 'bsitar' function.
#' 
#' model <- berkeley_mfit
#' 
#' # Population average age and velocity during the peak growth spurt
#' growthparameters(model, re_formula = NA)
#' 
#' \donttest{
#' # Population average age and velocity during the take-off and the peak 
#' # growth spurt (APGV, PGV. ATGV, TGV)
#' 
#' growthparameters(model, re_formula = NA, peak = TRUE, takeoff = TRUE)
#' 
#' # Individual-specific age and velocity during the take-off and the peak
#' # growth spurt (APGV, PGV. ATGV, TGV)
#' 
#' growthparameters(model, re_formula = NULL, peak = TRUE, takeoff = TRUE)
#' }
#' 
growthparameters.bgmfit <- function(model,
                               newdata = NULL,
                               resp = NULL,
                               ndraws = NULL,
                               draw_ids = NULL,
                               summary = TRUE,
                               robust = FALSE,
                               re_formula = NA,
                               peak = TRUE,
                               takeoff = FALSE,
                               trough = FALSE,
                               acgv = FALSE,
                               acgv_velocity = 0.10,
                               estimation_method = 'fitted',
                               allow_new_levels = FALSE,
                               sample_new_levels = "uncertainty",
                               incl_autocor = TRUE,
                               numeric_cov_at = NULL,
                               levels_id = NULL,
                               avg_reffects = NULL,
                               aux_variables = NULL,
                               ipts = 10,
                               deriv_model = TRUE,
                               conf = 0.95,
                               xrange = NULL,
                               xrange_search = NULL,
                               digits = 2,
                               seed = 123,
                               future = FALSE,
                               future_session = 'multisession',
                               cores = NULL,
                               parms_eval = FALSE,
                               idata_method = NULL,
                               parms_method = 'getPeak',
                               verbose = FALSE,
                               fullframe = NULL,
                               dummy_to_factor = NULL, 
                               usesavedfuns = FALSE,
                               clearenvfuns = NULL,
                               envir = NULL,
                               ...) {
  if(is.null(envir)) {
    envir <- parent.frame()
  }
  
  if(is.null(ndraws)) {
    ndraws <- brms::ndraws(model)
  }
  
  if(is.null(deriv_model)) {
    deriv_model <- TRUE
  }
  
  if (is.null(idata_method)) {
    idata_method <- 'm2'
  }
  
  
  # Initiate non formalArgs()
  xvar <- NULL;
  acgv_asymptote <- NULL;
  apv <- NULL;
  Parameter <- NULL;
  IDvar <- NULL;
  groupby_fistr <- NULL;
  groupby_fstr <- NULL;
  subindicatorsi <- NULL;
  Estimate <- NULL;
  ':=' <- NULL;
  . <- NULL;
  XXi <- NULL;
  
  oo <- post_processing_checks(model = model,
                               xcall = match.call(),
                               envir = envir,
                               resp = resp)
  
  xcall <- strsplit(deparse(sys.calls()[[1]]), "\\(")[[1]][1]
  
  get_xcall <- function(xcall, scall) {
    scall <- scall[[length(scall)]]
    if(any(grepl("plot_curves", scall, fixed = T)) |
       any(grepl("plot_curves.bgmfit", scall, fixed = T))) {
      xcall <- "plot_curves"
    } else if(any(grepl("growthparameters", scall, fixed = T)) |
              any(grepl("growthparameters.bgmfit", scall, fixed = T))) {
      xcall <- "growthparameters"
    } else {
      xcall <- xcall
    } 
  }
  
  if(!is.null(model$xcall)) {
    if(model$xcall == "plot_curves") {
      xcall <- "plot_curves"
    }
  } else {
    scall <- sys.calls()
    xcall <- get_xcall(xcall, scall)
  }
  
  
  model$xcall <- xcall
  
  arguments <- get_args_(as.list(match.call())[-1], xcall)
  
  arguments$model <- model
  
  if(xcall == 'plot_curves') arguments$plot <- TRUE else arguments$plot <- FALSE
  
  probs <- c((1 - conf) / 2, 1 - (1 - conf) / 2)
  probtitles <- probs[order(probs)] * 100
  probtitles <- paste("Q", probtitles, sep = "")
  set_names_  <- c('Estimate', 'Est.Error', probtitles)
  
  get.cores_ <- get.cores(arguments$cores)
  arguments$cores <- setincores <-  get.cores_[['max.cores']] 
  .cores_ps <- get.cores_[['.cores_ps']]
  
  if (future) {
    if(is.null(cores)) stop("Please set the number of cores for 'future' by  
                            using the the 'cores' argument, e.g. cores = 4")
    if (arguments$future_session == 'multisession') {
      future::plan('multisession', workers = setincores)
    } else if (arguments$future_session == 'multicore') {
      future::plan('multicore', workers = setincores)
    }
  }
  
  

  call_posterior_summary <- function(dat) {
    if (!robust) {
      . <- posterior::summarise_draws(dat,
                                      ~ mean(.x, na.rm = T),
                                      ~ sd(.x, na.rm = T),
                                      ~ quantile(.x,
                                                 probs = probs,
                                                 na.rm = T),
                                      .cores = .cores_ps)[-1] %>%
        data.frame() %>% stats::setNames(set_names_)
    } else if (robust) {
      . <- posterior::summarise_draws(
        dat,
        ~ median(.x, na.rm = T),
        ~ mad(.x, na.rm = T),
        ~ quantile(.x, probs = probs, na.rm = T),
        .cores = .cores_ps
      )[-1] %>%
        data.frame() %>% stats::setNames(set_names_)
    }
    as.data.frame(.)
  }
  
  #
  summarise_gp <-
    function(.x,
             probs,
             future,
             cores,
             peak,
             takeoff,
             trough,
             acgv,
             xrange_search,
             summary,
             robust) {
      Xnames <- names(.x)[grepl("^P._D.", names(.x))]
      .x <- .x %>% data.frame()
      
      if(!is.null(xrange_search)) {
        if(length(xrange_search) == 1) {
          if(is.symbol(xrange_search)) xrange_search <- deparse(xrange_search)
          if(xrange_search == 'range') {
            ullimits <- range(.x[[xvar]])
            .x <- .x %>% dplyr::mutate(XXi := eval(parse(text = xvar))) %>% 
              dplyr::filter(XXi %in% (ullimits[1]:ullimits[2]))
          }
        } else if(length(xrange_search) == 2) {
            ullimits <- xrange_search
            .x <- .x %>% dplyr::mutate(XXi := eval(parse(text = xvar))) %>% 
              dplyr::filter(XXi %in% (ullimits[1]:ullimits[2]))
            } else {
            stop("argument xrange_search should be either 
                 'range' or vector of length 2")
          }
      }
     
      
      if (peak) {
        if (future)
          out_1 <-
            future.apply::future_sapply(Xnames, function(x)
              sitar::getPeak(.x[[xvar]], as.numeric(.x[[x]])))
        if (!future)
          out_1 <-
            sapply(Xnames, function(x)
              sitar::getPeak(.x[[xvar]], as.numeric(.x[[x]])))
        out_1 <- t(out_1)
        colnames(out_1) <- c("APGV", "PGV")
      } else if (!peak) {
        out_1 <- NULL
      }
      
      if (takeoff) {
        if (future)
          out_2 <-
            future.apply::future_sapply(Xnames, function(x)
              sitar::getTakeoff(.x[[xvar]], as.numeric(.x[[x]])))
        if (!future)
          out_2 <-
            sapply(Xnames, function(x)
              sitar::getTakeoff(.x[[xvar]], as.numeric(.x[[x]])))
        out_2 <- t(out_2)
        colnames(out_2) <- c("ATGV", "TGV")
      } else if (!takeoff) {
        out_2 <- NULL
      }
      
      
      
      if (trough) {
        if (future)
          out_3 <-
            future.apply::future_sapply(Xnames, function(x)
              sitar::getTrough(.x[[xvar]], as.numeric(.x[[x]])))
        if (!future)
          out_3 <-
            sapply(Xnames, function(x)
              sitar::getTrough(.x[[xvar]], as.numeric(.x[[x]])))
        out_3 <- t(out_3)
        colnames(out_3) <- c("ACGV", "CGV")
      } else if (!trough) {
        out_3 <- NULL
      }
      

      
      if (acgv) {
        if(!is.null(acgv_asymptote)) stop("Currently acgv_asymptote not 
                                           supported. Please use acgv_velocity")
        if(!is.null(acgv_asymptote) & !is.null(acgv_velocity) ) {
          stop("Specify either acgv_asymptote or acgv_velocity but not both")
        }
        if(is.null(acgv_asymptote) & is.null(acgv_velocity) ) {
          stop("Specify either acgv_asymptote or acgv_velocity")
        }
        
        set_get___fun <- function(x) {
          if (!peak) pkkk <- sitar::getPeak(.x[[xvar]], as.numeric(.x[[x]]))
          if ( peak | apv) pkkk <- out_1
          get__ <- as.numeric(.x[[x]])
          set_x_for_afo <- .x[[xvar]]
          tempbind <- cbind(get__, set_x_for_afo) %>% 
            data.frame() %>% 
            filter(set_x_for_afo > pkkk [1])
          get__ <- tempbind[,1]
          set_x_for_afo <-  tempbind[,2]
          if(length(get__) != 0) {
            get___pct <- max(get__) * acgv_velocity
            target.index <- 
              which(abs(get__ - get___pct) == min(abs(get__ - get___pct)))
            get_asymptote_pct_x <- set_x_for_afo[target.index]
            if(length(get_asymptote_pct_x) > 1) {
              get_asymptote_pct_x <- mean(get_asymptote_pct_x)
            }
            out_3_temp <- c(get_asymptote_pct_x, get___pct)
          } else if(length(get__) == 0) {
            out_3_temp <- c(NA, NA)
          }
          out_3_temp
        }
        if (future)
          out_3 <-
            future.apply::future_sapply(Xnames, function(x)
              set_get___fun(x))
        if (!future)
          out_3 <-
            sapply(Xnames, function(x)
              set_get___fun(x))
        out_3 <- t(out_3)
        colnames(out_3) <- c("ACGV", "CGV")
      } else if (!acgv) {
        out_3 <- NULL
      }
      


      
      xframe <- out_1
      if (exists('out_2'))
        xframe <- cbind(xframe, out_2)
      if (exists('out_3'))
        xframe <- cbind(xframe, out_3)
      xframe <- xframe %>% as.matrix()
      pnames <- colnames(xframe)[!grepl("^P._D.", colnames(xframe))]
      
      if (!robust) {
        o_ <- posterior::summarise_draws(
          xframe,
          ~ mean(.x, na.rm = T),
          ~ sd(.x, na.rm = T),
          ~ quantile(.x, probs = probs, na.rm = T),
          .cores = .cores_ps
        )[-1] %>%
          data.frame() %>% stats::setNames(set_names_) %>%
          dplyr::mutate(Parameter = pnames) %>%
          dplyr::relocate(Parameter)
      } else if (robust) {
        o_ <- posterior::summarise_draws(
          xframe,
          ~ median(.x, na.rm = T),
          ~ mad(.x, na.rm = T),
          ~ quantile(.x, probs = probs, na.rm = T),
          .cores = .cores_ps
        )[-1] %>%
          data.frame() %>% stats::setNames(set_names_) %>%
          dplyr::mutate(Parameter = pnames) %>%
          dplyr::relocate(Parameter)
      }
      
      if (summary) {
        . <- o_[, 1:2]
      } else if (!summary) {
        . <-  o_
      }
      .
    }
  
  
  multiNewVar <- function(df, df2, varname){
    df %>% dplyr::mutate(., !!varname := df2[[varname]])
  }
  
  get_growthparameters <-
    function(out_v_,
             newdata,
             groupby_str,
             summary,
             digits,
             ...) {
      if (!summary) {
        out__ <- out_v_ %>%
          data.frame() %>%
          stats::setNames(paste0('P._D.', names(.))) %>%
          dplyr::mutate(!!xvar := newdata[[xvar]])
        for(i in IDvar) {
          out__ <- out__ %>% multiNewVar(df=., df2 = newdata, varname=i)
        } 
      } else if (summary) {
        out__ <- out_v %>% data.frame() %>%
          dplyr::select(1) %>%  data.frame() %>%
          stats::setNames(paste0('P._D.', names(.))) %>%
          dplyr::mutate(!!xvar := newdata[[xvar]])
        for(i in IDvar) {
          out__ <- out__ %>% multiNewVar(df=., df2 = newdata, varname=i)
        } 
      }
      
      if (!is.null(groupby_str)) {
        out__ <-
          cbind(out__, newdata %>% 
                  dplyr::select(dplyr::all_of(groupby_str))) %>%
          data.frame()
        out__ <-
          out__ %>% dplyr::group_by(dplyr::across(dplyr::all_of(groupby_str)))
      } else if (is.null(groupby_str)) {
        out__ <- cbind(out__, newdata)
      }
      
      if (is.null(re_formula)) {
        if (!summary) {
          parameters <- out__ %>%
            dplyr::group_modify(
              ~ summarise_gp(
                .x,
                probs = probs,
                future = future,
                cores = setincores,
                peak = peak,
                takeoff = takeoff,
                trough = trough,
                acgv = acgv,
                xrange_search = xrange_search,
                summary = summary,
                robust = robust
              )
            ) %>% dplyr::ungroup()
        } else if (summary) {
          parameters <- out__ %>%
            dplyr::group_modify(
              ~ summarise_gp(
                .x,
                probs = probs,
                future = future,
                cores = setincores,
                peak = peak,
                takeoff = takeoff,
                trough = trough,
                acgv = acgv,
                xrange_search = xrange_search,
                summary = summary,
                robust = robust
              )
            ) %>% dplyr::ungroup()
        }
      } else if (!is.null(re_formula)) {
        if (!summary) {
          parameters <- out__ %>%
            dplyr::group_modify(
              ~ summarise_gp(
                .x,
                probs = probs,
                future = future,
                cores = setincores,
                peak = peak,
                takeoff = takeoff,
                trough = trough,
                acgv = acgv,
                xrange_search = xrange_search,
                summary = summary,
                robust = robust
              )
            ) %>% dplyr::ungroup()
        } else if (summary) {
          parameters <- out__ %>%
            dplyr::group_modify(
              ~ summarise_gp(
                .x,
                probs = probs,
                future = future,
                cores = setincores,
                peak = peak,
                takeoff = takeoff,
                trough = trough,
                acgv = acgv,
                xrange_search = xrange_search,
                summary = summary,
                robust = robust
              )
            ) %>% dplyr::ungroup()
        }
      }
      parameters <- parameters %>% 
        dplyr::mutate(dplyr::across(dplyr::where(is.numeric),
                                  ~ round(., digits = digits)))
      
      return(parameters)
    }
  
  
  
  get_avg_over <- function(raw_re, newdata, by, probs, robust) {
    raw_re_c <- c()
    getitEstimate <- getitarray <- NULL
    for (i in 1:(dim(raw_re)[1])) {
      getitEstimate <- raw_re[i,]
      raw_re_c[i] <- cbind(newdata, getitEstimate) %>% data.frame() %>% 
        dplyr::group_by(dplyr::across(dplyr::all_of(by))) %>%
        dplyr::summarise(getitEstimate = mean(getitEstimate), 
                         .groups = 'drop') %>%
        dplyr::ungroup() %>%
        dplyr::select(dplyr::all_of(getitEstimate)) 
    }
    
    getitarray <- array(unlist(raw_re_c), 
                        dim=c(length(raw_re_c[[1]]), length(raw_re_c)  ))
    getitarray <- t(getitarray)
    getitarray
  }
  

  
  if (arguments$plot) {
    out_summary <- list()
    if(!is.null(arguments$...)) {
      arguments <- c(arguments, list(arguments$...))
    }
    arguments$model <- model
    
    
    
    for (argumentsi in names(arguments)) {
      if (length(arguments[[argumentsi]]) != 0) {
        if (argumentsi != "...") {
          arguments[[argumentsi]] <- eval(arguments[[argumentsi]])
        } 
      }
    }
    
    arguments[which(names(arguments) %in% "")] <- NULL
    
    list2env(arguments, envir = parent.env(new.env()))
    

    probs <- c((1 - conf) / 2, 1 - (1 - conf) / 2)
    probtitles <- probs[order(probs)] * 100
    probtitles <- paste("Q", probtitles, sep = "")
    set_names_  <- c('Estimate', 'Est.Error', probtitles)
    
    get.cores_ <- get.cores(arguments$cores)
    arguments$cores <- setincores <-  get.cores_[['max.cores']] 
    .cores_ps <- get.cores_[['.cores_ps']]
    
    if (future) {
      if(is.null(cores)) stop("Please set the number of cores for 'future' by  
                            using the the 'cores' argument, e.g. cores = 4")
      if (arguments$future_session == 'multisession') {
        future::plan('multisession', workers = setincores)
      } else if (arguments$future_session == 'multicore') {
        future::plan('multicore', workers = setincores)
      }
    }
    
    
    newdata <- get.newdata(model, newdata = newdata, 
                           resp = resp, 
                           numeric_cov_at = numeric_cov_at,
                           aux_variables = aux_variables,
                           levels_id = levels_id,
                           ipts = ipts,
                           xrange = xrange,
                           idata_method = idata_method,
                           dummy_to_factor = dummy_to_factor,
                           verbose = verbose)
    
    
    list_c <- attr(newdata, 'list_c')
    
    
    for (list_ci in names(list_c)) {
      assign(list_ci, list_c[[list_ci]])
    }
    check__ <- c('xvar', 'yvar', 'IDvar', 'cov_vars', 'cov_factor_vars', 
                 'cov_numeric_vars', 'groupby_fstr', 'groupby_fistr', 
                 'uvarby', 'subindicatorsi')
    
    for (check___ in check__) {
      if(!exists(check___)) assign(check___, NULL)
    }
    
    
    
    newdata___ <- newdata
    
    if(!is.null(avg_reffects)) {
      if (grepl("d", opt, ignore.case = T)) {
        index_opt <- gregexpr("d", opt, ignore.case = T)[[1]]
        dist.. <- substr(opt, index_opt, index_opt)
        if (dist.. != "" & grepl("^[[:upper:]]+$", dist..) )
          stop("use option 'd' (and not 'D') with avg_reffects" )
      }
      if (grepl("v", opt, ignore.case = T) ) {
        index_opt <- gregexpr("v", opt, ignore.case = T)[[1]]
        velc.. <- substr(opt, index_opt, index_opt)
        if (velc.. != "" & grepl("^[[:upper:]]+$", velc..) )
          stop("use option 'v' (and not 'V') with avg_reffects" )
      }
    }
    
    
    if(is.null(avg_reffects)) {
      if (grepl("d", opt, ignore.case = T)) {
        index_opt <- gregexpr("d", opt, ignore.case = T)[[1]]
        dist.. <- substr(opt, index_opt, index_opt)
      } else if (!grepl("d", opt, ignore.case = T)) {
        dist.. <- ""
      }
      
      if (grepl("v", opt, ignore.case = T)) {
        index_opt <- gregexpr("v", opt, ignore.case = T)[[1]]
        velc.. <- substr(opt, index_opt, index_opt)
      } else if (!grepl("v", opt, ignore.case = T)) {
        velc.. <- ""
      }
      
      if ((apv) & velc.. == "") {
        stop("You have set apv = TRUE but your opt argument",
             "\n ",
             "contains no 'v' or 'V' option")
      }
      
      if (dist.. != "") {
        if (grepl("^[[:upper:]]+$", dist..)) {
          groupby_str_d <- groupby_fistr
        } else  if (!grepl("^[[:upper:]]+$", dist..)) {
          groupby_str_d <- groupby_fstr
        }
        if (identical(groupby_str_d, character(0)))
          groupby_str_d <- NULL
        groupby_str_d <- groupby_str_d
      } else {
        groupby_str_d <- NULL
      }
      
      
      if (velc.. != "") {
        if (grepl("^[[:upper:]]+$", velc..)) {
          groupby_str_v <- groupby_fistr
        } else  if (!grepl("^[[:upper:]]+$", velc..)) {
          groupby_str_v <- groupby_fstr
        }
        # groupby_str_v <- c(avg_reffects_groupby_str_v, groupby_str_v)
        if (identical(groupby_str_v, character(0)))
          groupby_str_v <- NULL
      } else {
        groupby_str_v <- NULL
      }
      
      if (dist.. != "") {
        newdata <- newdata___
        if (grepl("^[[:upper:]]+$", dist..)) {
          arguments$re_formula <- NULL
        } else if (!grepl("^[[:upper:]]+$", dist..)) {
          arguments$re_formula <- NA
          if (!is.null(groupby_fstr)) {
            groupby_fstr_xvars <- c(groupby_fstr, xvar)
          } else if (is.null(groupby_fstr)) {
            groupby_fstr_xvars <- c(xvar)
          }
          
          newdata <- newdata %>%
            dplyr::group_by(
              dplyr::across(dplyr::all_of(groupby_fstr_xvars))
              ) %>%
            dplyr::slice(1) %>% dplyr::ungroup()
        }
        
        arguments$newdata <- newdata
        arguments$deriv <- 0
        arguments$ipts <- NULL 
        arguments$probs <- probs
        
        if (estimation_method == 'fitted') {
          out_d_ <- do.call(fitted_draws, arguments)
        } else if (estimation_method == 'predict') {
          out_d_ <- do.call(predict_draws, arguments)
        }
        
        if(is.null(out_d_)) return(invisible(NULL))
        
        
        if (!summary) {
          out_d <- call_posterior_summary((out_d_))
        } else if (summary) {
          out_d <- out_d_
        }
        

        if(!is.na(model$model_info$univariate_by)) {
          newdata <- newdata %>%
            dplyr::filter(eval(parse(text = subindicatorsi)) == 1) %>% 
            droplevels()
        }
        
        out_summary[['distance']] <-  
          cbind(newdata,
                out_d %>% data.frame() %>%
                  dplyr::mutate(curve = 'distance')) %>%
          data.frame()
        
      } else if (dist.. == "") {
        out_summary[['distance']] <- NULL
      }
      
      
      if (velc.. != "") {
        newdata <- newdata___
        if (grepl("^[[:upper:]]+$", velc..)) {
          arguments$re_formula <- NULL
        } else if (!grepl("^[[:upper:]]+$", velc..)) {
          arguments$re_formula <- NA
          if (!is.null(groupby_fstr)) {
            groupby_fstr_xvars <- c(groupby_fstr, xvar)
          } else if (is.null(groupby_fstr)) {
            groupby_fstr_xvars <- c(xvar)
            
          }
          newdata <- newdata %>%
            dplyr::group_by(
              dplyr::across(dplyr::all_of(groupby_fstr_xvars))
              ) %>%
            dplyr::slice(1) %>% dplyr::ungroup()
        }
        arguments$newdata <- newdata
        arguments$deriv <- 1
        arguments$ipts <- NULL 
        arguments$probs <- probs
        
        if (estimation_method == 'fitted') {
          out_v_ <- do.call(fitted_draws, arguments)
        } else if (estimation_method == 'predict') {
          out_v_ <- do.call(predict_draws, arguments)
        }
        
        if(is.null(out_v_)) return(invisible(NULL))
        
        
        out_v__apv_ <- out_v_
        if (!summary) {
          out_v <- call_posterior_summary((out_v_))
        } else if (summary) {
          out_v <- out_v_
        }
        
        # out_v <- out_v_
        
        if(!is.na(model$model_info$univariate_by)) {
          newdata <- newdata %>%
            dplyr::filter(eval(parse(text = subindicatorsi)) == 1) %>% 
            droplevels()
        }
        
        out_summary[['velocity']] <-
          cbind(newdata,
                out_v %>% data.frame() %>%
                  dplyr::mutate(curve = 'velocity')) %>%
          data.frame()
      } else if (velc.. == "") {
        out_summary[['velocity']] <- NULL
      }
      
      
      if (apv | takeoff | trough | acgv) {
        if(!is.na(model$model_info$univariate_by)) {
          newdata <- newdata %>%
            dplyr::filter(eval(parse(text = subindicatorsi)) == 1) %>% 
            droplevels()
        }
        
        
        out_v__apv_ <- t(out_v__apv_)
        out_summary[['parameters']] <-
          get_growthparameters(out_v__apv_, newdata, groupby_str_v, summary, 
                               digits)
      }
      out_summary[['groupby_str_d']] <- groupby_str_d
      out_summary[['groupby_str_v']] <- groupby_str_v
      out_summary[['probtitles']] <- probtitles
    } # if(is.null(avg_reffects)) {
    
    
    
    
    if(!is.null(avg_reffects)) {
      if (grepl("d", opt, ignore.case = T)) {
        index_opt <- gregexpr("d", opt, ignore.case = T)[[1]]
        dist.. <- substr(opt, index_opt, index_opt)
      } else if (!grepl("d", opt, ignore.case = T)) {
        dist.. <- ""
      }
      
      if (grepl("v", opt, ignore.case = T)) {
        index_opt <- gregexpr("v", opt, ignore.case = T)[[1]]
        velc.. <- substr(opt, index_opt, index_opt)
      } else if (!grepl("v", opt, ignore.case = T)) {
        velc.. <- ""
      }
      
      if ((apv) & velc.. == "") {
        stop("You have set apv = TRUE but your opt argument",
             "\n ",
             "contains no 'v' or 'V' option")
      }
      
     
      
      if (dist.. != "") {
        groupby_str_d <- groupby_fstr
        if (identical(groupby_str_d, character(0)))
          groupby_str_d <- NULL
        groupby_str_d <- groupby_str_d
      } else {
        groupby_str_d <- NULL
      }
      
      
      
      if (velc.. != "") {
        groupby_str_v <- groupby_fstr
        if (identical(groupby_str_v, character(0)))
          groupby_str_v <- NULL
      } else {
        groupby_str_v <- NULL
      }
      
      summary_org <- arguments$summary
      arguments$summary <- FALSE
      arguments$re_formula <- NULL
      
      groupby_str_d <- c(groupby_str_d, avg_reffects[['feby']])
      groupby_str_v <- c(groupby_str_v, avg_reffects[['feby']])
      

      # Don't let below arguments$re_formula override the above 'd' and 'v'
      if (dist.. != "") {
        newdata <- newdata___
        if (grepl("^[[:upper:]]+$", dist..)) {
        } else if (!grepl("^[[:upper:]]+$", dist..)) {
          if (!is.null(groupby_fstr)) {
            groupby_fstr_xvars <- c(groupby_fstr, xvar)
          } else if (is.null(groupby_fstr)) {
            groupby_fstr_xvars <- c(xvar)
          }
        }
        
        arguments$newdata <- newdata
        arguments$deriv <- 0
        arguments$ipts <- NULL 
        arguments$probs <- probs
        
        
        if (estimation_method == 'fitted') {
          out_d_ <- do.call(fitted_draws, arguments)
        } else if (estimation_method == 'predict') {
          out_d_ <- do.call(predict_draws, arguments)
        }
        
        if(is.null(out_d_)) return(invisible(NULL))
        
        
        arguments$summary <- summary_org
        
        if(!is.na(model$model_info$univariate_by)) {
          newdata <- newdata %>%
            dplyr::filter(eval(parse(text = subindicatorsi)) == 1) %>% 
            droplevels()
        }
        
         selectby <- avg_reffects[['reby']]
        selectover <- avg_reffects[['over']]
        selectby_over <- c(selectby, selectover)
        out_d_ <- get_avg_over(out_d_, newdata = newdata, by = selectby_over,
                               probs = probs, robust = robust)
        
        out_d <- brms::posterior_summary(out_d_, probs = probs, robust = robust)
  
        
        newdata <- newdata %>%
          dplyr::distinct(., dplyr::across(dplyr::all_of(selectby_over)), 
                          .keep_all = TRUE) %>% 
          dplyr::arrange(!! as.name(selectby_over)) %>% 
          droplevels()
        
  
        
        out_summary[['distance']] <-  
          cbind(newdata,
                out_d %>% data.frame() %>%
                  dplyr::mutate(curve = 'distance')) %>%
          data.frame()
        
      } else if (dist.. == "") {
        out_summary[['distance']] <- NULL
      }
      
      
      summary_org <- arguments$summary
      arguments$summary <- FALSE
      arguments$re_formula <- NULL
      
      # Don't let below arguments$re_formula override the above 'd' and 'v'
      if (velc.. != "") {
        newdata <- newdata___
        if (grepl("^[[:upper:]]+$", velc..)) {
        } else if (!grepl("^[[:upper:]]+$", velc..)) {
          if (!is.null(groupby_fstr)) {
            groupby_fstr_xvars <- c(groupby_fstr, xvar)
          } else if (is.null(groupby_fstr)) {
            groupby_fstr_xvars <- c(xvar)
            
          }
        }
        
        arguments$newdata <- newdata
        arguments$deriv <- 1
        arguments$ipts <- NULL 
        arguments$probs <- probs
        
        
        if (estimation_method == 'fitted') {
          out_v_ <- do.call(fitted_draws, arguments)
        } else if (estimation_method == 'predict') {
          out_v_ <- do.call(predict_draws, arguments)
        }
        
        if(is.null(out_v_)) return(invisible(NULL))
        
        
        arguments$summary <- summary_org
        
        # moved here from below for avg_reffects to work with univariate_by
        if(!is.na(model$model_info$univariate_by)) {
          newdata <- newdata %>%
            dplyr::filter(eval(parse(text = subindicatorsi)) == 1) %>% 
            droplevels()
        }
        
        selectby <- avg_reffects[['reby']]
        selectover <- avg_reffects[['over']]
        selectby_over <- c(selectby, selectover)
        out_v_ <- get_avg_over(out_v_, newdata = newdata, by = selectby_over,
                               probs = probs, robust = robust)
        
        out_v <- brms::posterior_summary(out_v_, probs = probs, robust = robust)
       
        
        newdata <- newdata %>%
          dplyr::distinct(., dplyr::across(dplyr::all_of(selectby_over)), 
                          .keep_all = TRUE) %>% 
          dplyr::arrange(!! as.name(selectby_over)) %>% 
          droplevels()
        
        out_summary[['velocity']] <-
          cbind(newdata,
                out_v %>% data.frame() %>%
                  dplyr::mutate(curve = 'velocity')) %>%
          data.frame()
      } else if (velc.. == "") {
        out_summary[['velocity']] <- NULL
      }
      
      
      if (apv) {
        if(!is.na(model$model_info$univariate_by)) {
        }
        
        newdata <- newdata %>%
          dplyr::distinct(., dplyr::across(dplyr::all_of(selectby_over)), 
                          .keep_all = TRUE) %>% 
          dplyr::arrange(!! as.name(selectby_over)) %>% 
          droplevels()
        
        out_v_ <- t(out_v_)
        out_summary[['parameters']] <-
          get_growthparameters(out_v_, newdata, groupby_str_v, summary, 
                               digits) # out_v_
      }
      out_summary[['groupby_str_d']] <- groupby_str_d
      out_summary[['groupby_str_v']] <- groupby_str_v
      out_summary[['probtitles']] <- probtitles
    } # if(!is.null(avg_reffects)) {
    return(out_summary)
  } # if (arguments$plot) {
  
  
  if (!arguments$plot) {
    newdata <- get.newdata(model, newdata = newdata, 
                           resp = resp, 
                           numeric_cov_at = numeric_cov_at,
                           aux_variables = aux_variables,
                           levels_id = levels_id,
                           ipts = ipts,
                           xrange = xrange,
                           idata_method = idata_method,
                           dummy_to_factor = dummy_to_factor,
                           verbose = verbose)
    

    list_c <- attr(newdata, 'list_c')
    for (list_ci in names(list_c)) {
      assign(list_ci, list_c[[list_ci]])
    }
    check__ <- c('xvar', 'yvar', 'IDvar', 'cov_vars', 'cov_factor_vars', 
                 'cov_numeric_vars', 'groupby_fstr', 'groupby_fistr', 
                 'uvarby', 'subindicatorsi')
    
    for (check___ in check__) {
      if(!exists(check___)) assign(check___, NULL)
    }
    
    
    if(is.null(arguments$re_formula)) {
      opt <- 'V'
    }
    if(!is.null(arguments$re_formula)) {
      opt <- 'v'
    }
    
    if(!is.null(avg_reffects)) {
      if (grepl("v", opt, ignore.case = T) ) {
        index_opt <- gregexpr("v", opt, ignore.case = T)[[1]]
        velc.. <- substr(opt, index_opt, index_opt)
        if (velc.. != "" & grepl("^[[:upper:]]+$", velc..) )
          stop("use option 'v' (and not 'V') with avg_reffects" )
      }
    }
    
    
    if(is.null(avg_reffects)) {
      if (grepl("v", opt, ignore.case = T)) {
        index_opt <- gregexpr("v", opt, ignore.case = T)[[1]]
        velc.. <- substr(opt, index_opt, index_opt)
      } else if (!grepl("v", opt, ignore.case = T)) {
        velc.. <- ""
      }
      
      if (velc.. != "") {
        if (grepl("^[[:upper:]]+$", velc..)) {
          groupby_str_v <- groupby_fistr
        } else  if (!grepl("^[[:upper:]]+$", velc..)) {
          groupby_str_v <- groupby_fstr
        }
        if (identical(groupby_str_v, character(0)))
          groupby_str_v <- NULL
      } else {
        groupby_str_v <- NULL
      }
      
      if (grepl("^[[:upper:]]+$", velc..)) {
        arguments$re_formula <- NULL
      } else if (!grepl("^[[:upper:]]+$", velc..)) {
        arguments$re_formula <- NA
        if (!is.null(groupby_fstr)) {
          groupby_fstr_xvars <- c(groupby_fstr, xvar)
        } else if (is.null(groupby_fstr)) {
          groupby_fstr_xvars <- c(xvar)
          
        }
        newdata <- newdata %>%
          dplyr::group_by(dplyr::across(dplyr::all_of(groupby_fstr_xvars))) %>%
          dplyr::slice(1) %>% dplyr::ungroup()
      }
      arguments$newdata <- newdata
      arguments$deriv <- 1
      arguments$ipts <- NULL 
      arguments$probs <- probs
     
      if (estimation_method == 'fitted') {
        out_v_ <- do.call(fitted_draws.bgmfit, arguments)
      } else if (estimation_method == 'predict') {
        out_v_ <- do.call(predict_draws, arguments)
      }
      
     if(is.null(out_v_)) return(invisible(NULL))
      
      
      out_v__apv_ <- out_v_
      if (!summary) {
        out_v <- call_posterior_summary((out_v_))
      } else if (summary) {
        out_v <- out_v_
      }
      
      if(!is.na(model$model_info$univariate_by)) {
        newdata <- newdata %>%
          dplyr::filter(eval(parse(text = subindicatorsi)) == 1) %>% 
          droplevels()
      }
      
     
      out_v__apv_ <- t(out_v__apv_)
      parameters <-
        get_growthparameters(out_v__apv_, newdata, groupby_str_v, summary, 
                             digits)
    } # if(is.null(avg_reffects)) {
    
    
    
    
    if(!is.null(avg_reffects)) {
      if (grepl("v", opt, ignore.case = T)) {
        index_opt <- gregexpr("v", opt, ignore.case = T)[[1]]
        velc.. <- substr(opt, index_opt, index_opt)
      } else if (!grepl("v", opt, ignore.case = T)) {
        velc.. <- ""
      }
      
      summary_org <- arguments$summary
      arguments$summary <- FALSE
      arguments$re_formula <- NULL
      
      groupby_str_d <- avg_reffects[['feby']]
      groupby_str_v <- avg_reffects[['feby']]
      
      if (grepl("^[[:upper:]]+$", velc..)) {
      } else if (!grepl("^[[:upper:]]+$", velc..)) {
        if (!is.null(groupby_fstr)) {
          groupby_fstr_xvars <- c(groupby_fstr, xvar)
        } else if (is.null(groupby_fstr)) {
          groupby_fstr_xvars <- c(xvar)
          
        }
      }
      
      arguments$newdata <- newdata
      arguments$deriv <- 1
      arguments$ipts <- NULL 
      arguments$probs <- probs
      
      if (estimation_method == 'fitted') {
        out_v_ <- do.call(fitted_draws, arguments)
      } else if (estimation_method == 'predict') {
        out_v_ <- do.call(predict_draws, arguments)
      }
      
      if(is.null(out_v_)) return(invisible(NULL))
      
      
      arguments$summary <- summary_org

      selectby <- avg_reffects[['reby']]
      selectover <- avg_reffects[['over']]
      selectby_over <- c(selectby, selectover)
      out_v_ <- get_avg_over(out_v_, newdata = newdata, by = selectby_over,
                             probs = probs, robust = robust)
      
      
      out_v <- out_v_
      
      if(!is.na(model$model_info$univariate_by)) {
        newdata <- newdata %>%
          dplyr::filter(eval(parse(text = subindicatorsi)) == 1) %>% 
          droplevels()
      }
      
      newdata <- newdata %>%
        dplyr::distinct(., dplyr::across(dplyr::all_of(selectby_over)), 
                        .keep_all = TRUE) %>% 
        dplyr::arrange(!! as.name(selectby_over)) %>% 
        droplevels()
      
      if(!is.na(model$model_info$univariate_by)) {
        newdata <- newdata %>%
          dplyr::filter(eval(parse(text = subindicatorsi)) == 1) %>% 
          droplevels()
      }
      
      newdata <- newdata %>%
        dplyr::distinct(., dplyr::across(dplyr::all_of(selectby_over)), 
                        .keep_all = TRUE) %>% 
        dplyr::arrange(!! as.name(selectby_over)) %>% 
        droplevels()
     
      out_v_ <- t(out_v_)
      parameters <-
        get_growthparameters(out_v_, newdata, groupby_str_v, summary, 
                             digits)
    } # if(!is.null(avg_reffects)) {
    return(parameters)
  } # if (!arguments$plot) {
  
} # end growthparameters





#' @rdname growthparameters.bgmfit
#' @export
growthparameters <- function(model, ...) {
  UseMethod("growthparameters")
}


