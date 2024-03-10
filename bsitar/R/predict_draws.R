

#' Predicted values from the posterior predictive distribution
#' 
#' @description The \strong{predict_draws()} is a wrapper around the
#'   [brms::predict.brmsfit()] function to obtain predicted values (and their
#'   summary) from the posterior distribution. See [brms::predict.brmsfit()] for
#'   details.
#' 
#' @details The \strong{predict_draws()} function computed the fitted values
#'   from the posterior distribution. The [brms::predict.brmsfit()] function
#'   from the \pkg{brms} package can used to get the predicted (distance) values
#'   when outcome (e.g., height) is untransformed. However, when the outcome is
#'   log or square root transformed, the [brms::predict.brmsfit()] function will
#'   return the fitted curve on the log or square root scale whereas the
#'   \strong{predict_draws()} function returns the fitted values on the original
#'   scale. Furthermore, the \strong{predict_draws()} also compute the first
#'   derivative of (velocity) that too on the original scale after making
#'   required back-transformation. Except for these differences, both these
#'   functions (i.e., [brms::predict.brmsfit()] and [predict_draws()]) work in
#'   the same manner. In other words, user can specify all the options available
#'   in the [brms::predict.brmsfit()].
#' 
#' @param ... Additional arguments passed to the [brms::predict.brmsfit()]
#'   function. Please see [brms::predict.brmsfit()] for details on various
#'   options available.
#' 
#' @return An array of predicted response values. See [brms::predict.brmsfit()]
#'   for details.
#' 
#' @inherit growthparameters.bgmfit params
#' @inherit fitted_draws.bgmfit params
#' @inherit brms::predict.brmsfit params
#' 
#' @export predict_draws.bgmfit
#' @export
#' 
#' @seealso [brms::predict.brmsfit()] 
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
#' # Population average distance curve
#' predict_draws(model, deriv = 0, re_formula = NA)
#' 
#' \donttest{
#' # Individual-specific distance curves
#' predict_draws(model, deriv = 0, re_formula = NULL)
#' 
#' # Population average velocity curve
#' predict_draws(model, deriv = 1, re_formula = NA)
#' 
#' # Individual-specific velocity curves
#' predict_draws(model, deriv = 1, re_formula = NULL)
#'  }
#' 
predict_draws.bgmfit <-
  function(model,
           newdata = NULL,
           resp = NULL,
           ndraws = NULL,
           re_formula = NA,
           allow_new_levels = FALSE,
           sample_new_levels = "uncertainty",
           incl_autocor = TRUE,
           numeric_cov_at = NULL,
           levels_id = NULL,
           avg_reffects = NULL,
           aux_variables = NULL,
           ipts = 10,
           deriv = 0,
           deriv_model = TRUE,
           summary = TRUE,
           robust = FALSE,
           probs = c(0.025, 0.975),
           xrange = NULL,
           xrange_search = NULL,
           parms_eval = FALSE,
           parms_method = 'getPeak',
           idata_method = NULL,
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
    
    
    
    # This in plot_conditional_effects_calling if(!eval(full.args$deriv_model)){
    plot_conditional_effects_calling <- FALSE
    syscalls1 <- sys.calls()[[1]]
    syscallsall <- paste(deparse(syscalls1), collapse = "\n")
    for (xc in 1:length(syscallsall)) {
      if(any(grepl('plot_conditional_effects', syscallsall[[xc]]))) {
        plot_conditional_effects_calling <- TRUE
      }
    }
    
    # Checks for newdata and arguments
    # For plot_conditional_effects_calling, newdata is not evaluted
    # For indirectcall i.e.,  model$xcall arguments are passed from the
    # plot_curves() and growthparameters() functions
    
    indirectcall <- FALSE
    if(!plot_conditional_effects_calling) {
      if(!is.null(model$xcall)) {
        arguments <- get_args_(as.list(match.call())[-1], model$xcall)
        full.args <- evaluate_call_args(cargs = arguments, 
                                        fargs = NULL, 
                                        dargs = NULL, 
                                        verbose = verbose)
        full.args$object <- full.args$model
        newdata <- full.args$newdata
        indirectcall <- TRUE
      } else {
        full.args <- evaluate_call_args(cargs = as.list(match.call())[-1], 
                                        fargs = formals(), 
                                        dargs = list(...), 
                                        verbose = verbose)
        
        full.args$model <- model
        newdata <- do.call(get.newdata, full.args)
      }
      full.args$newdata <- newdata
    }
    
    
    if(plot_conditional_effects_calling) {
      full.args <- evaluate_call_args(cargs = as.list(match.call())[-1], 
                                      fargs = formals(), 
                                      dargs = list(...), 
                                      verbose = verbose)
    }
    
    
    if(!is.null(model$model_info$decomp)) {
      if(model$model_info$decomp == "QR") deriv_model<- FALSE
    }
    
    expose_method_set <- model$model_info[['expose_method']]
    
    model$model_info[['expose_method']] <- 'NA' # Over ride method 'R'
    
    o <- post_processing_checks(model = model,
                                xcall = match.call(),
                                resp = resp,
                                envir = envir,
                                deriv = deriv, 
                                all = FALSE,
                                verbose = verbose)
    
    oall <- post_processing_checks(model = model,
                                   xcall = match.call(),
                                   resp = resp,
                                   envir = envir,
                                   deriv = deriv, 
                                   all = TRUE,
                                   verbose = FALSE)
    
    
    test <- setupfuns(model = model, resp = resp,
                      o = o, oall = oall, 
                      usesavedfuns = usesavedfuns, 
                      deriv = deriv, envir = envir, 
                      deriv_model = deriv_model, 
                      ...)
    
    if(is.null(test)) return(invisible(NULL))
    
    misc <- c("verbose", "usesavedfuns", "clearenvfuns", 
              "envir", "fullframe")
    
    if(!indirectcall) {
      calling.args <- post_processing_args_sanitize(model = model,
                                                    xcall = match.call(),
                                                    resp = resp,
                                                    envir = envir,
                                                    deriv = deriv, 
                                                    dots = list(...),
                                                    misc = misc,
                                                    verbose = verbose)
    } else if(indirectcall) {
      calling.args <- full.args
    }
    
    calling.args$object <- full.args$model
    
    . <- do.call(predict, calling.args)
    
    
    if(!is.null((eval(full.args$deriv)))) {
      if(eval(full.args$deriv > 0)) { 
        if(!eval(full.args$deriv_model)) {
          full.args$. <- .
          . <- do.call(mapderivqr, full.args)
        } else {
          . <- .
        }
      }
    }
    
    
    
    # Restore function(s)
    assign(o[[1]], model$model_info[['exefuns']][[o[[1]]]], envir = envir)
    
    if(!is.null(eval(full.args$clearenvfuns))) {
      if(!is.logical(eval(full.args$clearenvfuns))) {
        stop('clearenvfuns must be NULL or a logical')
      } else {
        setcleanup <- eval(full.args$clearenvfuns)
      }
    }
    
    if(is.null(eval(full.args$clearenvfuns))) {
      if(eval(full.args$usesavedfuns)) {
        setcleanup <- TRUE 
      } else {
        setcleanup <- FALSE
      }
    }
    
    # Cleanup environment if requested
    if(setcleanup) {
      tempgenv <- envir
      for (oalli in names(oall)) {
        if(exists(oalli, envir = tempgenv )) {
          remove(list=oalli, envir = tempgenv)
        }
      }
      tempgenv <- test
      for (oalli in names(oall)) {
        if(exists(oalli, envir = tempgenv )) {
          remove(list=oalli, envir = tempgenv)
        }
      }
    } # if(setcleanup) {
    
    
    # fullframe
    if(!is.null(eval(full.args$fullframe))) {
      if(eval(full.args$fullframe)) {
        if(!eval(full.args$fullframe)) {
          stop("fullframe can not be combined with summary = FALSE")
        }
        if(full.args$idata_method == 'm1') {
          stop("fullframe can not be combined with idata_method = 'm1'")
        }
      }
    }
    if(is.null(eval(full.args$fullframe))) {
      if (!is.na(model$model_info$univariate_by)) {
        if(full.args$idata_method == 'm1') setfullframe <- FALSE
        if(full.args$idata_method == 'm2') setfullframe <- TRUE
      } else {
        setfullframe <- FALSE
      }
    }
    if (!is.na(model$model_info$univariate_by)) {
      if(full.args$idata_method == 'm2') {
        uvarby <- model$model_info$univariate_by
        uvarbyresp <- paste0(uvarby, resp)
        uvarbynewdata <- eval(full.args$newdata) %>% 
          dplyr::filter(!!dplyr::sym(uvarbyresp) == 1)
        if(setfullframe) . <- cbind(., uvarbynewdata)
      }
    }
    
    . 
  }



#' @rdname predict_draws.bgmfit
#' @export
predict_draws <- function(model, ...) {
  UseMethod("predict_draws")
}


