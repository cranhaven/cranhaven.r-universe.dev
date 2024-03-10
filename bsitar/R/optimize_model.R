

#' @title Optimize SITAR model
#' 
#' @description Select the best fitting SITAR model that involves choosing the
#'   optimum degrees of freedom (\code{df}) for the natural cubic-spline curve
#'   and the appropriate transformations of the predictor \code{x} and response
#'   \code{y} variables.
#'
#' @param optimize_df A list of integers specifying the degree of freedom
#'   (\code{df}) values to be optimized. If \code{NULL} (default), the \code{df}
#'   is taken from the original model. For optimization over different
#'   \code{df}, say for example \code{df} 4 and \code{df} 5, the corresponding
#'   code is \code{optimize_df = list(4,5)}. For \code{univariate_by} and
#'   \code{multivariate} models, \code{optimize_df} can be a single integer
#'   (e.g., \code{optimize_df = 4}) or a list (e.g., \code{optimize_df =
#'   list(4,5)}), or a a list of lists. As an example, consider optimization
#'   over \code{df} 4 and \code{df} 5 for the first sub model, and \code{df} 5
#'   and \code{df} 6 for the second sub model, the corresponding code is
#'   \code{optimize_df = list(list(4,5), list(5,6))}.
#'
#' @param optimize_x A vector specifying the transformations for the predictor
#'   variable (i.e., \code{x}). The options available are \code{NULL},
#'   \code{'log'}, \code{'sqrt'}, or their combinations. Note that user need not
#'   to enclose these options in a single or double quotes as they are take care
#'   of internally. The default setting is to explore all possible combination
#'   i.e., \code{optimize_x = list(NULL, log,  sqrt)}. Similar to the
#'   \code{optimize_df}, user can specify different \code{optimize_x} for
#'   \code{univariate_by} and \code{multivariate} sub models.
#'
#' @param optimize_y A vector specifying the transformations of the the response
#'   variable (i.e., \code{y}). The approach and options available for
#'   \code{optimize_y} are same as described above for the \code{optimize_x}.
#'
#' @param exclude_default_funs A logical to indicate whether transformations for
#'   (\code{x} and \code{y}) variables used in the original model fit should be
#'   excluded. If \code{TRUE} (default), the transformations specified for the
#'   \code{x} and \code{y} variables in the original model fit are excluded from
#'   the \code{optimize_x} and \code{optimize_y}. From example, if original
#'   model is fit with \code{xvar = log} and \code{yvar = NULL}, then
#'   \code{optimize_x} is translated into \code{optimize_x = list(NULL, sqrt)},
#'   and similarly \code{optimize_y} is reset as \code{optimize_y = list(log,
#'   sqrt)}.
#'
#' @param add_fit_criteria An optional argument (default \code{NULL}) to
#'   indicate whether to add fit criteria to the returned model fit. Options
#'   available are \code{'loo'} and \code{'waic'}. Please see
#'   [brms::add_criterion()] for details.
#'
#' @param add_bayes_R An optional argument (default \code{NULL}) to indicate
#'   whether to add Bayesian R square to the returned model fit. To estimate and
#'   add \code{bayes_R2} to the model fit, the argument \code{add_bayes_R} is
#'   set as \code{add_bayes_R = 'bayes_R2'}.
#'
#' @param byresp A logical (default \code{FALSE}) to indicate if response wise
#'   fit criteria to be calculated. This argument is evaluated only for the
#'   \code{multivariate} model in which user can select whether to get joint
#'   calculation of point wise log likelihood (\code{byresp = FALSE}) or
#'   response specific (\code{byresp = TRUE}). For, \code{univariate_by} model,
#'   the only option available is to calculate separate point wise log
#'   likelihood for each sub-model, i.e., \code{byresp = TRUE}.
#'
#' @param cores The number of cores to used in parallel processing (default
#'   \code{1}). The argument \code{cores} is passed to the
#'   [brms::add_criterion()].
#'   
#' @param expose_function An optional argument logical argument to indicate
#'   whether to expose Stan function used in model fitting (\code{TRUE}) or not
#'   (\code{FALSE}). Default \code{NULL} takes \code{expose_function} from the
#'   \code{model} being optimized. Note that \code{expose_function} must be set
#'   as \code{TRUE} when adding \code{fit criteria} and/or \code{bayes_R2}.
#' 
#' @param ... Other arguments passed to \code{\link{update_model}}.
#' 
#' @inheritParams  growthparameters.bgmfit
#'
#' @return A list containing the optimized models of class \code{bgmfit}, and the
#'  the summary statistics if \code{add_fit_criteria} and/or
#'  \code{add_bayes_R} are specified.
#'  
#' @export optimize_model.bgmfit
#' @export
#' 
#' @importFrom loo pareto_k_table
#'  
#' @seealso [brms::add_criterion()]
#'
#' @inherit berkeley author
#'
#' @examples
#' 
#' model <- berkeley_mfit
#' 
#' \donttest{
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
#' # Below example shows dummy call to optimization to save time. 
#' # Note that in case degree of freedom and both  optimize_x and optimize_y are
#' # NULL (i.e., nothing to optimize), the original model object is returned.   
#' # To explicitly get this information whether model is being optimized or not, 
#' # user can set verbose = TRUE. The verbose = TRUE also useful in getting the
#' # information regarding what all arguments have been changed as compared to
#' # the original model.
#' 
#' model2 <- optimize_model(model, optimize_df = NULL, 
#'   optimize_x = NULL, 
#'   optimize_y = NULL,
#'   verbose = TRUE)
#' 
#' }
#' 
optimize_model.bgmfit <- function(model,
                                  newdata = NULL,
                                  optimize_df = NULL,
                                  optimize_x = list(NULL, log,  sqrt),
                                  optimize_y = list(NULL, log,  sqrt),
                                  exclude_default_funs = TRUE,
                                  add_fit_criteria = NULL,
                                  add_bayes_R = NULL,
                                  byresp = FALSE,
                                  digits = 2,
                                  cores = 1,
                                  expose_function = NULL, 
                                  verbose = FALSE,
                                  envir = NULL,
                                  ...) {
  
  if(is.null(envir)) {
    envir <- parent.frame()
  }
  
  # Initiate non formalArgs()
  outcome <- NULL;
  xfun <- NULL; 
  yfun <- NULL;
  Parameter <- NULL;
  Estimate <- NULL;
  . <- NULL;
  
  if (is.null(newdata)) {
    newdata <- model$model_info$bgmfit.data
  } else {
    newdata <- newdata
  }
  
  if(!is.null(optimize_x)) {
    if(!is.list(optimize_x)) stop("argument 'optimize_x' must be a list")
  }
  
  if(!is.null(optimize_y)) {
    if(!is.list(optimize_y)) stop("argument 'optimize_y' must be a list")
  }
  
  o <-
    post_processing_checks(model = model,
                           xcall = match.call(),
                           resp = NULL,
                           envir = envir,
                           deriv = 0,
                           all = FALSE)
  
  call_o <- match.call()
  call_o_args <- as.list(call_o)[-1]
  
  args_o <- as.list(model$model_info$call.full.bgmfit)[-1]
  
  if(!is.null(call_o_args$expose_function)) {
    args_o$expose_function <- call_o_args$expose_function
  }
    
  args_o_dots_ <- list(...)
  if (length(args_o_dots_) > 0) {
    for (i in names(args_o_dots_)) {
      args_o[[i]] <- args_o_dots_[[i]]
    }
  }
  
  
  
  # This to evaluate T/F to TRUE/FALSE
  for (i in names(args_o)) {
    if (is.symbol(args_o[[i]])) {
      if (args_o[[i]] == "T")
        args_o[[i]] <- eval(args_o[[i]])
      if (args_o[[i]] == "F")
        args_o[[i]] <- eval(args_o[[i]])
    }
  }
  
  for (add_fit_criteriai in add_fit_criteria) {
    if (!add_fit_criteriai %in% c("loo", "waic")) {
      stop("only loo and waic criteria are supported")
    }
  }
  
  for (bayes_Ri in add_bayes_R) {
    if (!bayes_Ri %in% c("bayes_R2")) {
      stop("only bayes_R2 as R square measure is supported")
    }
  }
  
  
  need_exposed_function <- FALSE
  if(!is.null(add_fit_criteria)) {
    need_exposed_function <- TRUE
  } else if(is.list(add_fit_criteria)) {
    if(!any(is.null(add_fit_criteria[[1]]))) need_exposed_function <- TRUE
  } else if(!is.null(add_bayes_R)) {
    need_exposed_function <- TRUE
  } else if(is.list(add_bayes_R)) {
    if(!any(is.null(add_bayes_R[[1]]))) need_exposed_function <- TRUE
  }

  
  
  
  # Not must for expose_function to be true when not adding criteria or bayes R2
  if (need_exposed_function) {
  if (!args_o$expose_function) {
      stop(
        "Argument 'expose_function' must be set to TRUE when ",
        "\n ",
        " 'add_fit_criteria' and/or 'add_bayes_R' not NULL"
      )
    }
    if(!grepl('GlobalEnv', deparse(substitute(envir)), ignore.case = T)) {
      stop(
        "The 'envir' must be '.GlobalEnv' when ",
        "\n ",
        " 'add_fit_criteria' and/or 'add_bayes_R' not NULL",
        "\n ",
        " This is a known issue ",
        "(https://github.com/paul-buerkner/brms/issues/1577)"
      )
    }
  } # if (need_exposed_function) {
  
  
  
  
  get_args_opt <- function(xo) {
    get_within_fist_last_paranthesese <- function(x__) {
      x__ <- sub('\\(', '[', x__)
      x__ <- sub("\\)([^)]*)$", "]\\1", x__)
      x__ <-
        gsub("[\\[\\]]", "", regmatches(x__, gregexpr("\\[.*?\\]", x__))[[1]])
      x__ <- gsub("\\[|\\]", "", x__)
      x__
    }
    gsub_comma_within_paranthesese <-
      function(x__, replace_comma_by) {
        tt <-
          gsub("[\\(\\)]", "", regmatches(x__, gregexpr("\\(.*?\\)", x__))[[1]])
        tt2 <- gsub(",", replace_comma_by, tt, fixed = T)
        j <- 0
        for (i in tt) {
          j <- j + 1
          x__ <- gsub(tt[j], tt2[j], x__, fixed = T)
        }
        x__
      }
    
    xxo <- gsub("[[:space:]]", "", xo)
    
    xxo_g <- gsub('\"', "", xxo)
    xxo_g2 <- 
      grepl(
        "[-]?[0-9]+[.]?[0-9]*|[-]?[0-9]+[L]?|[-]?[0-9]+[.]?[0-9]*[eE][0-9]+", 
        xxo_g)
    
    if(any(xxo_g2)) xxo_g3 <- TRUE else xxo_g3 <- FALSE
    
    numeric_dx <- xxo_g3
    
    if (xxo != "NULL" & xxo != "\"NULL\"" & !numeric_dx) {
      xxo <- get_within_fist_last_paranthesese(xxo)
      xxo <- gsub_comma_within_paranthesese(xxo, "_comma_")
      xxo <- strsplit(xxo, ",")[[1]]
      xxo <- gsub("_comma_" , ",", xxo)
      xxo <- gsub('\"', "", xxo)
    } else {
      xxo <- xxo
      xxo <- gsub('\"', "", xxo)
    }
    xxo
  }
  
  optimize_df <- get_args_opt(deparse(substitute(optimize_df)))
  optimize_x  <- get_args_opt(deparse(substitute(optimize_x)))
  optimize_y  <- get_args_opt(deparse(substitute(optimize_y)))
  
  if (exclude_default_funs) {
    optimize_x <- optimize_x[!optimize_x %in% model$model_info$xfuns]
    optimize_y <-
      optimize_y[!optimize_y %in% model$model_info$xfuns]
    if (identical(optimize_x, character(0)))
      optimize_x <- "NULL"
    if (identical(optimize_y, character(0)))
      optimize_y <- "NULL"
  }
  
  optimize_df_x_y <-
    expand.grid(optimize_df, optimize_x, optimize_y)
  
  colnames(optimize_df_x_y) <- c("df", "xfun", "yfun")
  
  add_summary_waic <- NULL
  Count <- Est.Error <- Inference <- Min..n_eff <- where <- NULL
  Min.n_eff <- Percent <- Proportion <- Range <- SE <- NULL
  
  
  combine_summaries <- function(model_list, summary_obj) {
    ic = 0
    list_c <- list()
    for (model_listi in 1:length(model_list)) {
      if (!is.null(model_list[[model_listi]][[summary_obj]])) {
        ic <- ic + 1
        list_c[[ic]] <- model_list[[model_listi]][[summary_obj]]
      }
      summary_of_obj <-
        list_c %>% do.call(rbind, .) %>% data.frame()
    }
    if (nrow(summary_of_obj) < 1)
      summary_of_obj <- NULL
    summary_of_obj
  }
  
  # resp = NULL is only used as a placeholder that too only for multivariate
  # If NULL, then combined log likelihood used for multivariate model
  # otherwise separate log likelihood  for each response

  add_citeria_fun <- function(fit,
                              add_fit_criteria = NULL,
                              add_bayes_R = NULL,
                              resp = NULL,
                              digits = 2,
                              df,
                              xfun_print,
                              yfun_print,
                              ...) {
    
   # if (!fit$model_info$call.full.bgmfit$expose_function) {
      assign(o[[1]], fit$model_info[['exefuns']][[o[[1]]]], envir = envir)
   # }
    
    if (!is.null(add_fit_criteria)) {
      what_ <- paste(add_fit_criteria, collapse = ", ")
      message(" Adding", " ", what_, " ", "...")
      if(verbose) cat("\n")
      if (is.na(fit$model_info$univariate_by) |
          !fit$model_info$multivariate) {
        if (!fit$model_info$multivariate) {
          suppressWarnings(fit <- brms::add_criterion(fit,
                                                      add_fit_criteria, 
                                                      cores = cores))
        }
        if (fit$model_info$multivariate) {
          if (is.null(resp)) {
            suppressWarnings(fit <- brms::add_criterion(fit,
                                                        add_fit_criteria, 
                                                        cores = cores))
          }
          if (!is.null(resp)) {
            for (aci in fit$model_info$ys) {
              suppressWarnings(fit <- brms::add_criterion(
                fit,
                add_fit_criteria,
                resp = aci,
                cores = cores
              ))
              aci_names <- paste0(names(fit$criteria), aci)
              names(fit$criteria) <- aci_names
            }
            aci_names <- c()
            for (aci in fit$model_info$ys) {
              aci_names <- c(aci_names, paste0(add_fit_criteria, aci))
            }
            names(fit$criteria) <- aci_names
          }
        }
      }
      
      if (!is.na(fit$model_info$univariate_by)) {
        for (aci in fit$model_info$ys) {
          suppressWarnings(fit <- brms::add_criterion(
            fit,
            add_fit_criteria,
            resp = aci,
            cores = cores
          ))
          aci_names <- paste0(names(fit$criteria), aci)
          names(fit$criteria) <- aci_names
        }
        aci_names <- c()
        for (aci in fit$model_info$ys) {
          aci_names <- c(aci_names, paste0(add_fit_criteria, aci))
        }
        names(fit$criteria) <- aci_names
      }
    } # if (!is.null(add_fit_criteria))
    
    
    if (!is.null(add_bayes_R)) {
      what_ <- paste(add_bayes_R, collapse = ", ")
      if(verbose) message(" Adding", " ", what_, " ", "...")
      if(verbose) cat("\n")
      if (is.na(fit$model_info$univariate_by)) {
        if (!fit$model_info$multivariate) {
          aci_names <- paste0(add_bayes_R, '')
          suppressWarnings(fit$criteria[[aci_names]] <-
                             brms::bayes_R2(fit, cores = cores))
          fit$criteria[[aci_names]] <-
            fit$criteria[[aci_names]] %>%
            data.frame() %>% dplyr::mutate(Parameter = rownames(.)) %>%
            dplyr::relocate(Parameter)
          rownames(fit$criteria[[aci_names]]) <- NULL
        }
        if (fit$model_info$multivariate) {
          if (is.null(resp)) {
            aci_names <- paste0(add_bayes_R, '')
            suppressWarnings(fit$criteria[[aci_names]] <-
                               brms::bayes_R2(fit,
                                              cores = cores))
            fit$criteria[[aci_names]] <-
              fit$criteria[[aci_names]] %>%
              data.frame() %>% dplyr::mutate(Parameter = rownames(.)) %>%
              dplyr::relocate(Parameter)
            rownames(fit$criteria[[aci_names]]) <- NULL
          }
          if (!is.null(resp)) {
            for (aci in fit$model_info$ys) {
              aci_names <- paste0(add_bayes_R, aci)
              suppressWarnings(fit$criteria[[aci_names]] <-
                                 brms::bayes_R2(fit,
                                                resp = aci,
                                                cores = cores))
              fit$criteria[[aci_names]] <-
                fit$criteria[[aci_names]] %>%
                data.frame() %>% dplyr::mutate(Parameter = rownames(.)) %>%
                dplyr::relocate(Parameter)
              rownames(fit$criteria[[aci_names]]) <- NULL
            }
          }
        }
      }
      
      
      
      
      if (!is.na(fit$model_info$univariate_by)) {
        for (aci in fit$model_info$ys) {
          aci_names <- paste0(add_bayes_R, aci)
          suppressWarnings(fit$criteria[[aci_names]] <-
                             brms::bayes_R2(fit,
                                            resp = aci,
                                            cores = cores))
          fit$criteria[[aci_names]] <-
            fit$criteria[[aci_names]] %>%
            data.frame() %>% dplyr::mutate(Parameter = rownames(.)) %>%
            dplyr::relocate(Parameter)
          rownames(fit$criteria[[aci_names]]) <- NULL
        }
      }
    } # if (!is.null(add_bayes_R)) {
    
    
    
    add_summary_waic <- function(x, digits = 1) {
      summary_waic <- x
      summary_waic$pointwise <- NULL
      summary_waic <- summary_waic$estimates
      summary_waic <- summary_waic %>% data.frame()
      summary_waic <- summary_waic %>%
        dplyr::mutate(dplyr::across(dplyr::where(is.numeric),
                                    ~ round(., digits = digits)))
      summary_waic$Parameter <- row.names(summary_waic)
      row.names(summary_waic) <- NULL
      summary_waic <-
        summary_waic %>% dplyr::relocate(Parameter, Estimate, SE)
      summary_waic
    }
    
    add_summary_bayes_R2 <- function(x, digits = 2) {
      summary_bayes_R <- x
      summary_bayes_R <- summary_bayes_R %>% data.frame()
      summary_bayes_R <- summary_bayes_R %>%
        dplyr::mutate(dplyr::across(dplyr::where(is.numeric),
                                    ~ round(., digits = digits)))
      row.names(summary_bayes_R) <- NULL
      summary_bayes_R$Parameter <- 'bayes_R2'
      summary_bayes_R$SE <- summary_bayes_R$Est.Error
      summary_bayes_R <-
        summary_bayes_R %>% dplyr::select(-c(Est.Error))
      summary_bayes_R <- summary_bayes_R %>%
        dplyr::relocate(Parameter, Estimate, SE)
      summary_bayes_R
    }
    
    
    
    add_summary_loo <- function(x, digits = 1) {
      summary_loo <- x
      summary_loo$pointwise <- NULL
      summary_loo <- summary_loo$estimates
      summary_loo <- summary_loo %>% data.frame()
      summary_loo <- summary_loo %>%
        dplyr::mutate(dplyr::across(dplyr::where(is.numeric),
                                    ~ round(., digits = digits)))
      summary_loo$Parameter <- row.names(summary_loo)
      row.names(summary_loo) <- NULL
      summary_loo <-
        summary_loo %>% dplyr::relocate(Parameter, Estimate, SE)
      summary_loo
    }
    
    add_diagnostic_loo <- function(x, digits = 1) {
      summary_loo_diagnostic <- loo::pareto_k_table(x) %>% data.frame()
      row.names(summary_loo_diagnostic) <- NULL
      summary_loo_diagnostic$Range <- attr(loo::pareto_k_table(x),
                                           "dimnames")[[1]]
      summary_loo_diagnostic$Inference <-
        c('Good', "Ok", "Bad", "Very bad")
      summary_loo_diagnostic$Percent <-
        round(summary_loo_diagnostic$Proportion * 100, digits)
      summary_loo_diagnostic$Min.n_eff  <-
        summary_loo_diagnostic$Min..n_eff
      summary_loo_diagnostic$Min.n_eff <-
        round(summary_loo_diagnostic$Min.n_eff)
      summary_loo_diagnostic <- summary_loo_diagnostic %>%
        dplyr::select(-c(Proportion, Min..n_eff))
      summary_loo_diagnostic <- summary_loo_diagnostic %>%
        dplyr::relocate(Range, Inference, Count, Percent, Min.n_eff)
      summary_loo_diagnostic
    }
    
    
    if ('waic' %in% add_fit_criteria) {
      enverr. <- parent.frame()
      tryCatch(
        expr = {
          if (!is.na(fit$model_info$univariate_by)) {
            list_c_ <- list()
            for (aci in fit$model_info$ys) {
              getit_ <- paste0('waic', aci)
              list_c_[[aci]] <-
                add_summary_waic(fit$criteria[[getit_]], 
                                 digits = digits) %>%
                dplyr::mutate(outcome = aci) %>% dplyr::relocate(outcome)
            }
            summary_waic <-
              list_c_ %>%  do.call(rbind, .) %>% data.frame()
          } else if (fit$model_info$multivariate & !is.null(resp)) {
            list_c_ <- list()
            for (aci in fit$model_info$ys) {
              getit_ <- paste0('waic', aci)
              list_c_[[aci]] <-
                add_summary_waic(fit$criteria[[getit_]], 
                                 digits = digits) %>%
                dplyr::mutate(outcome = aci) %>% dplyr::relocate(outcome)
            }
            summary_waic <-
              list_c_ %>%  do.call(rbind, .) %>% data.frame()
          } else if (fit$model_info$multivariate & is.null(resp)) {
            getit_ <- paste0('waic', '')
            summary_waic <-
              add_summary_waic(fit$criteria[[getit_]], digits = digits)
          } else if (is.na(fit$model_info$univariate_by) &
                     !fit$model_info$multivariate) {
            getit_ <- paste0('waic', '')
            summary_waic <-
              add_summary_waic(fit$criteria[[getit_]], digits = digits)
          }
          summary_waic$df <- df
          summary_waic$xfun <- xfun_print
          summary_waic$yfun <- yfun_print
          summary_waic <-
            summary_waic %>% dplyr::relocate(df, xfun, yfun)
          rownames(summary_waic) <- NULL
        },
        error = function(e) {
          assign('err.', TRUE, envir = enverr.)
        }
      )
      err. <- get('err.', envir = enverr.)
      if (err.) {
        summary_waic <- NULL
      } else {
        summary_waic <- summary_waic
      }
      fit$summary_waic <- summary_waic
    }
    
    
    
    
    
    if ('bayes_R2' %in% add_bayes_R) {
      enverr. <- parent.frame()
      assign('err.', FALSE, envir = enverr.)
      tryCatch(
        expr = {
          if (!is.na(fit$model_info$univariate_by)) {
            list_c_ <- list()
            for (aci in fit$model_info$ys) {
              getit_ <- paste0(add_bayes_R, aci)
              list_c_[[aci]] <-
                add_summary_bayes_R2(fit$criteria[[getit_]], 
                                     digits = digits) %>%
                dplyr::mutate(outcome = aci) %>% dplyr::relocate(outcome)
            }
            summary_bayes_R2 <-
              list_c_ %>%  do.call(rbind, .) %>% data.frame()
          } else if (fit$model_info$multivariate & !is.null(resp)) {
            list_c_ <- list()
            for (aci in fit$model_info$ys) {
              getit_ <- paste0(add_bayes_R, aci)
              list_c_[[aci]] <-
                add_summary_bayes_R2(fit$criteria[[getit_]], 
                                     digits = digits) %>%
                dplyr::mutate(outcome = aci) %>% dplyr::relocate(outcome)
            }
            summary_bayes_R2 <-
              list_c_ %>%  do.call(rbind, .) %>% data.frame()
          } else if (fit$model_info$multivariate & is.null(resp)) {
            getit_ <- paste0('bayes_R2', '')
            summary_bayes_R2 <-
              add_summary_bayes_R2(fit$criteria[[getit_]], 
                                   digits = digits)
          } else if (is.na(fit$model_info$univariate_by) &
                     !fit$model_info$multivariate) {
            getit_ <- paste0('bayes_R2', '')
            summary_bayes_R2 <-
              add_summary_bayes_R2(fit$criteria[[getit_]], 
                                   digits = digits)
          }
          summary_bayes_R2$df <- df
          summary_bayes_R2$xfun <- xfun_print
          summary_bayes_R2$yfun <- yfun_print
          summary_bayes_R2 <-
            summary_bayes_R2 %>% dplyr::relocate(df, xfun, yfun)
          rownames(summary_bayes_R2) <- NULL
        },
        error = function(e) {
          assign('err.', TRUE, envir = enverr.)
        }
      )
      err. <- get('err.', envir = enverr.)
      if (err.) {
        summary_bayes_R2 <- NULL
      } else {
        summary_bayes_R2 <- summary_bayes_R2
      }
      fit$summary_bayes_R2 <-
        summary_bayes_R2 %>% dplyr::select(-Parameter)
    }
    
    
    
    if ('loo' %in% add_fit_criteria) {
      if ('loo' %in% add_fit_criteria) {
        enverr. <- parent.frame()
        assign('err.', FALSE, envir = enverr.)
        tryCatch(
          expr = {
            if (!is.na(fit$model_info$univariate_by)) {
              list_c_ <- list()
              for (aci in fit$model_info$ys) {
                getit_ <- paste0('loo', aci)
                list_c_[[aci]] <-
                  add_summary_loo(fit$criteria[[getit_]], 
                                  digits = digits) %>%
                  dplyr::mutate(outcome = aci) %>% dplyr::relocate(outcome)
              }
              summary_loo <-
                list_c_ %>%  do.call(rbind, .) %>% data.frame()
            } else if (fit$model_info$multivariate &
                       !is.null(resp)) {
              list_c_ <- list()
              for (aci in fit$model_info$ys) {
                getit_ <- paste0('loo', aci)
                list_c_[[aci]] <-
                  add_summary_loo(fit$criteria[[getit_]], 
                                  digits = digits) %>%
                  dplyr::mutate(outcome = aci) %>% dplyr::relocate(outcome)
              }
              summary_loo <-
                list_c_ %>%  do.call(rbind, .) %>% data.frame()
            } else if (fit$model_info$multivariate &
                       is.null(resp)) {
              getit_ <- paste0('loo', '')
              
              summary_loo <-
                add_summary_loo(fit$criteria[[getit_]], digits = digits)
            } else if (is.na(fit$model_info$univariate_by) &
                       !fit$model_info$multivariate) {
              getit_ <- paste0('loo', '')
              summary_loo <-
                add_summary_loo(fit$criteria[[getit_]], digits = digits)
            }
            summary_loo$df <- df
            summary_loo$xfun <- xfun_print
            summary_loo$yfun <- yfun_print
            summary_loo <-
              summary_loo %>% dplyr::relocate(df, xfun, yfun)
            rownames(summary_loo) <- NULL
          },
          error = function(e) {
            assign('err.', TRUE, envir = enverr.)
          }
        )
        err. <- get('err.', envir = enverr.)
        if (err.) {
          summary_loo <- NULL
        } else {
          summary_loo <- summary_loo
        }
        fit$summary_loo <- summary_loo
      }
      
      if ('loo' %in% add_fit_criteria) {
        enverr. <- parent.frame()
        assign('err.', FALSE, envir = enverr.)
        tryCatch(
          expr = {
            if (!is.na(fit$model_info$univariate_by)) {
              list_c_ <- list()
              for (aci in fit$model_info$ys) {
                getit_ <- paste0('loo', aci)
                list_c_[[aci]] <-
                  add_diagnostic_loo(fit$criteria[[getit_]], 
                                     digits = digits) %>%
                  dplyr::mutate(outcome = aci) %>% dplyr::relocate(outcome)
              }
              diagnostic_loo <-
                list_c_ %>%  do.call(rbind, .) %>% data.frame()
            } else if (fit$model_info$multivariate &
                       !is.null(resp)) {
              list_c_ <- list()
              for (aci in fit$model_info$ys) {
                getit_ <- paste0('loo', aci)
                list_c_[[aci]] <-
                  add_diagnostic_loo(fit$criteria[[getit_]], 
                                     digits = digits) %>%
                  dplyr::mutate(outcome = aci) %>% dplyr::relocate(outcome)
              }
              diagnostic_loo <-
                list_c_ %>%  do.call(rbind, .) %>% data.frame()
            } else if (fit$model_info$multivariate &
                       is.null(resp)) {
              getit_ <- paste0('loo', '')
              diagnostic_loo <-
                add_diagnostic_loo(fit$criteria[[getit_]], 
                                   digits = digits)
            } else if (is.na(fit$model_info$univariate_by) &
                       !fit$model_info$multivariate) {
              diagnostic_loo <-
                add_diagnostic_loo(fit$criteria[[getit_]], 
                                   digits = digits)
            }
            diagnostic_loo$df <- df
            diagnostic_loo$xfun <- xfun_print
            diagnostic_loo$yfun <- yfun_print
            diagnostic_loo <-
              diagnostic_loo %>% dplyr::relocate(df, xfun, yfun)
            rownames(diagnostic_loo) <- NULL
          },
          error = function(e) {
            assign('err.', TRUE, envir = enverr.)
          }
        )
        err. <- get('err.', envir = enverr.)
        if (err.) {
          diagnostic_loo <- NULL
        } else {
          diagnostic_loo <- diagnostic_loo
        }
        fit$diagnostic_loo <- diagnostic_loo
      }
    } # if('loo' %in% add_fit_criteria) {
    
    return(fit)
  } # add_citeria_fun
  
  
  
  optimize_fun <- function(.x, model) {
    message("\nOptimizing model no. ",
            .x,
            " (total ",
            nrow(optimize_df_x_y),
            " models)")
    exe_row <- optimize_df_x_y[.x, ]
    df <- levels(droplevels(exe_row$df))
    xfun <- levels(droplevels(exe_row$xfun))
    yfun <- levels(droplevels(exe_row$yfun))
    if (df == 'NULL')
      df <-
      paste0("list(", paste(model$model_info$dfs, collapse = ","), ")")
    else
      df <- df
    if (xfun == 'NULL')
      xfun <- NULL
    else
      xfun <- xfun
    if (yfun == 'NULL')
      yfun <- NULL
    else
      yfun <- yfun
    
    if (is.null(xfun))
      xfun_print <- deparse(xfun)
    else
      xfun_print <- xfun
    if (is.null(yfun))
      yfun_print <- deparse(yfun)
    else
      yfun_print <- yfun
    
    if(verbose) {
      cat("\n")
      cat(paste0("df = ", df, "; xfun = ", xfun_print, "; yfun = ", yfun_print),
          "\n")
    }
    
    
    optimization_info <-
      paste0("df = ", df, "; xfun = ", xfun_print, "; yfun = ", yfun_print)
    
    
    args_o$model <- model
    args_o$df    <- eval(parse(text = df))
    args_o$xfun  <- xfun
    args_o$yfun  <- yfun
    args_o$data  <- newdata %>% data.frame()
    
    
    
    args_o$model  <- NULL
    
    args_o_new <- args_o
    calling    <- model$model_info$call.full.bgmfit
    
    args_o_org <- calling
    args_o_org[[1]] <- NULL
    
    args_o_new$data <- NULL
    args_o_org$data <- NULL
    
    if(is.na(model$model_info$univariate_by) &
       !model$model_info$multivariate) {
      if(length(args_o_new$df) == 1)   args_o_new$df   <- args_o_new$df[[1]]
      if(length(args_o_new$xfun) == 1) args_o_new$xfun <- args_o_new$xfun[[1]]
      if(length(args_o_new$yfun) == 1) args_o_new$yfun <- args_o_new$yfun[[1]]
    }
    
    
    all_same_args_c <- all_same_args <- c()
    # args_o_org_updated <- list()
    for (args_oi in names(args_o_new)) {
      all_same_args_c <- c(all_same_args_c, identical(args_o_org[[args_oi]],
                                                      args_o_new[[args_oi]]) 
      )
    }
    
    
    all_same_args_c_diffs <- args_o_new[!all_same_args_c]
    
    if(length(all_same_args_c_diffs) > 0) {
      all_same_args <- FALSE 
    } else {
      all_same_args <- TRUE
    }
    
    mandatory_opts <- c('df', 'xfun', 'yfun')
    
    
    if(all_same_args) {
      if(verbose) {
        cat("\n")
        cat("The arguemnets supplied for optimization are identical to the", 
            "\n ",
            "original model fit. Therefore, returning the original model fit")
        cat("\n")
      }
      fit <- NULL
    } else if(!all_same_args) {
      user_call   <- calling
      user_call   <- rlang::call_match(user_call, bsitar::bsitar)
      newargs     <- all_same_args_c_diffs
      for (newargsi in names(newargs)) {
        user_call[[newargsi]] <- NULL
      }
      user_call_data_name <- user_call$data
      assign(deparse(user_call_data_name), newdata)
      user_call <- rlang::call_modify(user_call, !!!newargs)
      # Setting it to FALSE because we are exposing it anyways below
      user_call$expose_function <- FALSE
      fit <- eval(user_call)
      if(args_o$expose_function) {
        fit <- expose_model_functions(fit, envir = envir)
      }
    }
    
    
    if(!is.null(fit)) {
      fit$model_info$optimization_info <- optimization_info
      fit$model_info$optimize_df <- df
      fit$model_info$optimize_x <- xfun_print
      fit$model_info$optimize_y <- yfun_print
      
      # Add fit_criteria and bares_R to the fit
      # Also, add summary data frames for criteria and R square
      
      # setresp to anything so that even multivariate will be response wise
      # if desired, this behavior
      # if(length(fit$model_info$ys) == 1) setresp <- NULL
      # if(length(fit$model_info$ys) > 1) setresp <- 'TRUE'
      
      if (fit$model_info$multivariate) {
        if (byresp) {
          setresp <- 'TRUE'
        } else if (!byresp) {
          setresp <- NULL
        }
      } else if (!fit$model_info$multivariate) {
        setresp <- NULL
      }
      
      if (!is.null(add_fit_criteria)) {
        fit <- add_citeria_fun(
          fit,
          add_fit_criteria = add_fit_criteria,
          add_bayes_R =  NULL,
          resp = setresp,
          digits = digits,
          df = df,
          xfun_print = xfun_print,
          yfun_print = yfun_print
        )
      }
      
      if (!is.null(add_bayes_R)) {
        fit <- add_citeria_fun(
          fit,
          add_fit_criteria = NULL,
          add_bayes_R =  add_bayes_R,
          resp = setresp,
          digits = digits,
          df = df,
          xfun_print = xfun_print,
          yfun_print = yfun_print
        )
      }
    } # if(!is.null(fit)) {
    return(fit)
  }
  
  optimize_list <- lapply(1:nrow(optimize_df_x_y), function(.x)
    optimize_fun(.x, model))
  
  if(!is.null(optimize_list[[1]])) {
    loo_fit             <- combine_summaries(optimize_list, 'summary_loo')
    loo_diagnostic_fit  <-
      combine_summaries(optimize_list, 'diagnostic_loo')
    waic_fit            <-
      combine_summaries(optimize_list, 'summary_waic')
    bayes_R2_fit        <-
      combine_summaries(optimize_list, 'summary_bayes_R2')
    
    # Parameter column is not created earlier for the 'bayes_R2_fit'
    if(!is.null(bayes_R2_fit)) {
      bayes_R2_fit <- bayes_R2_fit %>% 
        dplyr::mutate(Parameter = 'bayes_R2') %>% 
        dplyr::relocate(df, xfun, yfun, Parameter)
    }
    
    
    attributes(optimize_list) <- NULL
    
    optimize_summary <- data.frame()
    
    if(exists('loo_fit')) optimize_summary <- optimize_summary %>% 
      dplyr::bind_rows(., loo_fit)
    
    if(exists('loo_diagnostic_fit')) optimize_summary <- optimize_summary %>% 
      dplyr::bind_rows(., loo_diagnostic_fit)
    
    if(exists('waic_fit')) optimize_summary <- optimize_summary %>% 
      dplyr::bind_rows(., waic_fit)
    
    if(exists('bayes_R2_fit')) optimize_summary <- optimize_summary %>% 
      dplyr::bind_rows(., bayes_R2_fit)
    
    out <- list(models = optimize_list, optimize_summary = optimize_summary)
    
    return(out)
  } # if(!is.null(optimize_list[[1]])) {
 
}



#' @rdname optimize_model.bgmfit
#' @export
optimize_model <- function(model, ...) {
  UseMethod("optimize_model")
}
