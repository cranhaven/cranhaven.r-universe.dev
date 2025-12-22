# set the printing arguments that are potentially unspecified when functions
# like summary are called
set_print_vars <- function(object, print_regsDML, print_safety,
                           print_DML, print_regDML, print_regDML_all_gamma) {
  if (is.null(print_regsDML) && !is.null(object$regsDML_statistics)) {
    # if the regsDML printing argument is not set, but the regsDML results are
    # available in object, then return the results for regsDML only
    # if no other printing methods are set
    print_regsDML <- TRUE
    print_DML <- ifelse(is.null(print_DML), FALSE, print_DML)
    print_regDML <- ifelse(is.null(print_regDML), FALSE, print_regDML)
    print_safety <- ifelse(is.null(print_safety), FALSE, print_safety)
    print_regDML_all_gamma <-
      ifelse(is.null(print_regDML_all_gamma), FALSE, print_regDML_all_gamma)
  } else if (is.null(print_regsDML) && is.null(object$regsDML_statistics)) {
    # if the regsDML printing argument is not set, and no regsDML arguments
    # are available, output everything that is available except all_gamma
    print_regsDML <- FALSE
    print_DML <-
      ifelse(is.null(print_DML), !is.null(object$DML_statistics), print_DML)
    print_regDML <-
      ifelse(is.null(print_regDML), !is.null(object$regDML_statistics), print_regDML)
    print_safety <-
      ifelse(is.null(print_safety), !is.null(object$regDML_safety_statistics$beta_safety), print_safety)
    print_regDML_all_gamma <-
      ifelse(is.null(print_regDML_all_gamma), FALSE, print_regDML_all_gamma)
  } else {
    # if print_regsDML is specified, set all other printing variables
    # to FALSE that are not specified
    print_DML <- ifelse(is.null(print_DML), FALSE, print_DML)
    print_regDML <- ifelse(is.null(print_regDML), FALSE, print_regDML)
    print_safety <-  ifelse(is.null(print_safety), FALSE, print_safety)
    print_regDML_all_gamma <-
      ifelse(is.null(print_regDML_all_gamma), FALSE, print_regDML_all_gamma)
  }

  ans <- c(print_regsDML = print_regsDML,
           print_safety = print_safety,
           print_DML = print_DML,
           print_regDML = print_regDML,
           print_regDML_all_gamma = print_regDML_all_gamma)
  # if nothing can be printed (print_regsDML is FALSE and no other variables
  # have been set), stop
  if (sum(ans) == 0) {
    stop("nothing to return: no printing arguments specified")
  }
  stopifnot(is.logical(ans))
  ans
}

# Check if the quantities that should be outputted by summary, coef, and
# confint have actually been computed in regsdml.
check_feasibility <- function(object,
                              print_regsDML,
                              print_safety,
                              print_DML,
                              print_regDML,
                              print_regDML_all_gamma,
                              parm) {
  # if regDMLall should be outputted, the corresponding results need
  # to have been computed in object
  do_regDML_all_gamma <- !is.null(object$regDML_all_gamma_statistics)
  if (!do_regDML_all_gamma && (!is.null(parm) || print_regDML_all_gamma)) {
    stop("argument do_regDML_all_gamma was FALSE in regsdml,
no regDML_all_gamma output available. Please set parm to NULL.")
  }

  # if DML should be outputted, the corresponding results need to have been
  # computed in object.
  do_DML <- !is.null(object$DML_statistics)
  if (print_DML && !do_DML) {
    stop("argument do_DML was FALSE in regsdml, no DML output available.
Please set print_DML to FALSE.")
  }

  # if regDML should be outputted, the corresponding results need
  # to have been computed in object
  do_regDML <- !is.null(object$regDML_statistics)
  if (print_regDML && !do_regDML) {
    stop("argument do_regDML was FALSE in regsdml, no regDML output available.
Please set print_regDML to FALSE.")
  }

  # if regsDML should be outputted, the corresponding results need
  # to have been computed in object
  do_regsDML <- !is.null(object$regsDML_statistics)
  if (print_regsDML && !do_regsDML) {
    stop("argument do_regsDML was FALSE in regsdml, no regsDML output available.
Please set print_regsDML to FALSE.")
  }

  # if the results for the safety device should be outputted,
  # the corresponding results need to have been computed in object
  did_safety <- !is.null(object$regDML_safety_statistics$beta_safety)
  if (print_safety && !did_safety) {
    stop("argument do_safety was FALSE in regsdml, or the safety device was
infeasible, no safety output available.
Please set print_safety to FALSE.")
  }
}

##' S3method coef regsdml
coef.regsdml <- function(object,
                         print_regsDML = NULL,
                         print_safety = NULL,
                         print_DML = NULL,
                         print_regDML = NULL,
                         print_regDML_all_gamma = !is.null(parm),
                         parm = NULL,
                         print_gamma = FALSE, ...) {
  # check if the object is of class regsdml
  stopifnot(inherits(object, "regsdml"))

  # assign printing variables if they are not specified, that is,
  # they still equal NULL
  print_vars <- set_print_vars(object = object,
                               print_regsDML = print_regsDML,
                               print_safety = print_safety,
                               print_DML = print_DML,
                               print_regDML = print_regDML,
                               print_regDML_all_gamma = print_regDML_all_gamma)
  print_regsDML <- print_vars["print_regsDML"]
  print_safety <- print_vars["print_safety"]
  print_DML <- print_vars["print_DML"]
  print_regDML <- print_vars["print_regDML"]
  print_regDML_all_gamma <- print_vars["print_regDML_all_gamma"]
  # remove names
  names(print_regsDML) <- names(print_safety) <- names(print_DML) <-
    names(print_regDML) <- names(print_regDML_all_gamma) <- NULL

  check_feasibility(object = object,
                    print_regsDML = print_regsDML,
                    print_DML = print_DML,
                    print_safety = print_safety,
                    print_regDML = print_regDML,
                    print_regDML_all_gamma = print_regDML_all_gamma,
                    parm = parm)

  # length of the output object = how many things should be outputted
  total_outputs <-
    as.numeric(print_regsDML + print_regDML + print_DML +
                 print_safety * !is.null(object$regDML_safety_statistics$beta_safety)) +
    length(parm)

  # get some basic quantities from the input object
  d <- attributes(object)$d
  # assign return object based on if the gamma values should be outputted
  # or not
  betas <- if (print_gamma) {
    matrix(0, nrow = d + 1, ncol = total_outputs)
  } else {
    matrix(0, nrow = d, ncol = total_outputs)
  } # end betas
  colnames_betas <- c(rep("", total_outputs))

  # set the counter: will run from 1 to total_outputs
  # this counter will be increased after every computation in the following
  # if-statements
  counter <- 1

  # regsDML
  if (print_regsDML) {
    # get the gamma used
    gamma_aN <- object$regsDML_statistics$gamma_aN
    # fill respective columns of the return object
    betas[, counter] <- if (print_gamma) {
      rbind(gamma_aN, object$regsDML_statistics$beta_regsDML)
    } else {
      object$regsDML_statistics$beta_regsDML
    }
    # adapt the column names of the return object
    colnames_betas[counter] <- "regsDML"

    # increase the counter
    counter <- counter + 1
    # make sure the names of the estimated coefficients are available
    # so they can be taken as the rownames of the return object
    xx_colnames <- rownames(object$regsDML_statistics$beta_regsDML)
  }

  # safety device
  if (print_safety) {
    # if the safety device was applicable, output results.
    # otherwise, issue a warning
    if (!is.null(object$regDML_safety_statistics$beta_safety)) {
      gamma_safety <- object$regDML_safety_statistics$gamma_safety
      betas[, counter] <- if (print_gamma) {
        rbind(gamma_safety, object$regDML_safety_statistics$beta_safety)
      } else {
        object$regDML_safety_statistics$beta_safety
      } # end betas[, counter]

      colnames_betas[counter] <- "safety-device"
      counter <- counter + 1
      xx_colnames <- rownames(object$regDML_safety_statistics$beta_safety)
    } else { # if no safety output is available but its printing is TRUE
      warning("safety device was not applicable, thus cannot be returned")
    }
  }

  # DML
  if (print_DML) {
    betas[, counter] <- if (print_gamma) {
      rbind(Inf, object$DML_statistics$beta_DML)
    } else {
      object$DML_statistics$beta_DML
    } # end betas[, counter]

    colnames_betas[counter] <- "DML"
    counter <- counter + 1
    xx_colnames <- rownames(object$DML_statistics$beta_DML)
  }

  # regDML
  if (print_regDML) {
    gamma_aN <- object$regDML_statistics$gamma_aN
    betas[, counter] <- if (print_gamma) {
      rbind(gamma_aN, object$regDML_statistics$beta_regDML)
    }  else {
      object$regDML_statistics$beta_regDML
    }
    colnames_betas[counter] <- "regDML"

    counter <- counter + 1
    xx_colnames <- rownames(object$regDML_statistics$beta_regDML)
  }

  # regDML_all_gamma
  if (print_regDML_all_gamma) {
    betas[, counter:(counter + length(parm) - 1)] <- if (print_gamma) {
      gamma <- attributes(object)$gamma
      rbind(gamma[parm],
            do.call(cbind,
                    do.call(rbind,
                            object$regDML_all_gamma_statistics)[, "beta_regDML_all_gamma"])[, parm])
    } else {
      do.call(cbind, do.call(rbind, object$regDML_all_gamma_statistics)[, "beta_regDML_all_gamma"])[, parm]
    }

    xx_colnames <- rownames(do.call(rbind, object$regDML_all_gamma_statistics)[, "beta_regDML_all_gamma"][[1]])
    colnames_betas[counter:(counter + length(parm) - 1)] <- rep("regDMLall", length(parm))
  }

  # adapt row and column names of the return object
  colnames(betas) <- colnames_betas
  rownames(betas) <- if (print_gamma) {
    c("gamma", xx_colnames)
  } else {
    xx_colnames
  }

  betas
}

##' S3method summary regsdml
summary.regsdml <- function(object,
                            print_regsDML = NULL,
                            print_safety = NULL,
                            print_DML = NULL,
                            print_regDML = NULL,
                            print_regDML_all_gamma = !is.null(parm),
                            parm = NULL,
                            correlation = FALSE,
                            print_gamma = FALSE, ...) {
  # check if the object is of class regsdml
  stopifnot(inherits(object, "regsdml"))

  # assign printing variables if they are not specified, that is,
  # they still equal NULL
  print_vars <- set_print_vars(object = object,
                               print_regsDML = print_regsDML,
                               print_safety = print_safety,
                               print_DML = print_DML,
                               print_regDML = print_regDML,
                               print_regDML_all_gamma = print_regDML_all_gamma)
  print_regsDML <- print_vars["print_regsDML"]
  print_safety <- print_vars["print_safety"]
  print_DML <- print_vars["print_DML"]
  print_regDML <- print_vars["print_regDML"]
  print_regDML_all_gamma <- print_vars["print_regDML_all_gamma"]
  names(print_regsDML) <- names(print_safety) <- names(print_DML) <-
    names(print_regDML) <- names(print_regDML_all_gamma) <- NULL

  # check if quantities that should be outputted have been computed in object
  check_feasibility(object = object,
                    print_regsDML = print_regsDML,
                    print_DML = print_DML,
                    print_safety = print_safety,
                    print_regDML = print_regDML,
                    print_regDML_all_gamma = print_regDML_all_gamma,
                    parm = parm)

  # get some basic quantities from object
  d <- attributes(object)$d
  if (print_regDML_all_gamma) {
    gamma <- attributes(object)$gamma
  }

  # total length of the output object, that is, how many methods should
  # be returned
  length_ans <-
    as.numeric(print_regsDML + print_regDML + print_DML +
                 print_safety * !is.null(object$regDML_safety_statistics$beta_safety)) +
    length(parm)
  # initialize objects to return that are filled out subsequently
  ans <- vector(mode = "list", length = length_ans)
  ans_names <- vector()
  if (correlation) {
    varcov <- vector(mode = "list", length = length_ans)
    varcov_names <- vector()
  }

  # extract names of betas
  xx_colnames <- if (print_regsDML) {
    rownames(object$regsDML_statistics$beta_regsDML)
  } else if (print_safety) {
    rownames(object$regDML_safety_statistics$beta_safety)
  } else if (print_DML) {
    rownames(object$DML_statistics$beta_DML)
  } else if (print_regDML) {
    rownames(object$regDML_statistics$beta_regDML)
  } else if (print_regDML_all_gamma) {
    rownames(object$regDML_all_gamma_statistics[[1]]$beta_regDML_all_gamma)
  }

  default_ans <-
    matrix(NA_real_, nrow = d, ncol = 4L,
           dimnames = list(xx_colnames,
                           c("Estimate", "Std. Error", "z value", "Pr(>|z|)")))
  # set the counter. It will run from 1 to length_ans
  counter <- 1

  # regsDML
  if (print_regsDML) {
    # extract quantities from object
    regsDML_stats <- object$regsDML_statistics
    message_regsDML <- object$regsDML_statistics$message_regsDML
    # build return object
    add_info <- if (grepl("reg", message_regsDML)) {
      gamma_aN <- object$regsDML_statistics$gamma_aN
      ifelse(print_gamma,
             paste(" (",
                   formatC(gamma_aN, format = "e", digits = 2),
                   ")", sep = ""),
             "")
    } else {
      " (DML)"
    } # end add_info

    ans_names <- c(ans_names, paste("regsDML",  add_info, sep = ""))
    ans[[counter]] <- default_ans
    ans[[counter]][, 1] <- regsDML_stats$beta_regsDML
    ans[[counter]][, 2] <- regsDML_stats$sd_regsDML
    ans[[counter]][, 3] <- abs(ans[[counter]][, 1]) / ans[[counter]][, 2]
    ans[[counter]][, 4] <- regsDML_stats$pval_regsDML

    # build variance-covariance matrices if requested
    if (correlation) {
      varcov[[counter]] <- regsDML_stats$var_regsDML
      varcov_names <- c(varcov_names,
                        paste("regsDML", add_info, sep = ""))
    }

    # increase the counter
    counter <- counter + 1
  }

  # safety
  if (print_safety) {
    # extract quantities from object
    safety_stats <- object$regDML_safety_statistics

    # if the safety device was applicable, output results.
    # otherwise, issue a warning
    if (!is.null(safety_stats$beta_safety)) {
      safety_factor <- as.character(attributes(object)$safety_factor)
      gammasafety <-
        as.character(formatC(object$regDML_safety_statistics$gamma_safety,
                             format = "e", digits = 2))
      # build return object
      func_tmp_1 <- function(safety_factor, gammasafety) {
        ifelse(print_gamma,
               sprintf(" (factor = %s, gamma = %s)", safety_factor, gammasafety),
               "")
      }
      ans_names <- c(ans_names,
                     paste("safety-device",
                           func_tmp_1(safety_factor = safety_factor,
                                      gammasafety = gammasafety),
                           sep = ""))
      ans[[counter]] <- default_ans
      ans[[counter]][, 1] <- safety_stats$beta_safety
      ans[[counter]][, 2] <- safety_stats$sd_safety
      ans[[counter]][, 3] <- abs(ans[[counter]][, 1]) / ans[[counter]][, 2]
      ans[[counter]][, 4] <- safety_stats$pval_safety

      # build asymptotic variance-covariance matrix if requested
      if (correlation) {
        varcov[[counter]] <- safety_stats$var_safety
        func_tmp_2 <- function(safety_factor, gammasafety) {
          paste("safety-device",
                ifelse(print_gamma,
                       sprintf(" (factor = %s, gamma = %s)",
                               safety_factor, gammasafety),
                       ""),
                sep = "")
        }
        varcov_names <- c(varcov_names,
                          func_tmp_2(safety_factor = safety_factor,
                                     gammasafety = gammasafety))
      }

      # increase counter
      counter <- counter + 1
    } else { # if no safety output is available but its printing is TRUE
      warning("safety device was not applicable, thus cannot be returned")
    }
  }

  # DML
  if (print_DML) {
    # extract quantities from object
    DML_stats <- object$DML_statistics
    # build return object
    ans_names <- c(ans_names,
                   paste("DML", sep = ""))
    ans[[counter]] <- default_ans
    ans[[counter]][, 1] <- DML_stats$beta_DML
    ans[[counter]][, 2] <- DML_stats$sd_DML
    ans[[counter]][, 3] <- abs(ans[[counter]][, 1]) / ans[[counter]][, 2]
    ans[[counter]][, 4] <- DML_stats$pval_DML

    # build asymptotic variance-covariance matrix if requested
    if (correlation) {
      varcov[[counter]] <- DML_stats$var_DML
      varcov_names <- c(varcov_names, paste("DML", sep = ""))
    }

    # increase counter
    counter <- counter + 1
  }

  # regDML
  if (print_regDML) {
    # extract quantities from object
    regDML_stats <- object$regDML_statistics
    gammaopt <-
      as.character(formatC(regDML_stats$gamma_aN, format = "e", digits = 2))
    # build return object
    func_tmp_3 <- function(print_gamma, gammaopt) {
      paste("regDML",
            ifelse(print_gamma,
                   sprintf(" (%s)", gammaopt),
                   ""),
            sep = "")
    }
    ans_names <- c(ans_names,
                   func_tmp_3(print_gamma = print_gamma, gammaopt = gammaopt))
    ans[[counter]] <- default_ans
    ans[[counter]][, 1] <- regDML_stats$beta_regDML
    ans[[counter]][, 2] <- regDML_stats$sd_regDML
    ans[[counter]][, 3] <- abs(ans[[counter]][, 1]) / ans[[counter]][, 2]
    ans[[counter]][, 4] <- regDML_stats$pval_regDML

    # build asymptotic variance-covariance matrix if requested
    if (correlation) {
      varcov[[counter]] <- regDML_stats$var_regDML
      func_tmp_4 <- function(print_gamma, gammaopt) {
        paste("regDML",
              ifelse(print_gamma,
                     sprintf(" (%s)", gammaopt),
                     ""),
              sep = "")
      }
      varcov_names <-
        c(varcov_names,
          func_tmp_4(print_gamma = print_gamma, gammaopt = gammaopt))
    }

    # increase counter
    counter <- counter + 1
  }

  # regDML_all_gamma
  if (print_regDML_all_gamma) {
    # extract quantities from object
    regDML_all_gamma_stats <- object$regDML_all_gamma_statistics
    # build return object
    func_tmp_5 <- function(x, print_gamma) {
      paste("regDMLall",
            ifelse(print_gamma,
                   paste(" (",
                         formatC(x, format = "e", digits = 2),
                         ")",
                         sep = ""),
                   ""),
            sep = "")
    }
    new_names <- sapply(gamma[parm], func_tmp_5, print_gamma = print_gamma)
    ans_names <- c(ans_names, new_names)

    # build asymptotic variance-covariance matrix if requested
    if (correlation) {
      func_tmp_6 <- function(x, print_gamma) {
        paste("regDMLall",
              ifelse(print_gamma,
                     paste(" (",
                           formatC(x, format = "e", digits = 2),
                           ")", sep = ""),
                     ""),
              sep = "")
      }
      new_names <- sapply(gamma[parm], func_tmp_6, print_gamma = print_gamma)
      varcov_names <- c(varcov_names, new_names)
    }

    for (i in parm) {
      ans[[counter]] <- default_ans
      ans[[counter]][, 1] <- regDML_all_gamma_stats[[i]]$beta_regDML_all_gamma
      ans[[counter]][, 2] <- regDML_all_gamma_stats[[i]]$sd_regDML_all_gamma
      ans[[counter]][, 3] <- abs(ans[[counter]][, 1]) / ans[[counter]][, 2]
      ans[[counter]][, 4] <- regDML_all_gamma_stats[[i]]$pval_regDML_all_gamma

      if (correlation) {
        varcov[[counter]] <- regDML_all_gamma_stats[[i]]$var_regDML_all_gamma
      }

      # increase counter
      counter <- counter + 1
    }
  }

  # prepare object to return
  names(ans) <- ans_names
  ans_tot <- if (correlation) {
    names(varcov) <- varcov_names
    c(ans, varcov)
  } else {
    ans
  }

  attr(ans_tot, "correlation") <- correlation
  class(ans_tot) <- "summary.regsdml"
  ans_tot
}

##' S3method print summary.regsdml
print.summary.regsdml <- function(x, ...) {
  # titles of the two sections: there is a coefficients section and, if
  # requested, a section displaying the variance-covariance matrices.
  correlation <- attr(x, "correlation")
  num_meth <- if (correlation) {
    length(x) / 2
  } else {
    length(x)
  }

  # print coefficients
  cat("\nCoefficients :\n")
  coefs <- x[seq_len(num_meth)]
  class(coefs) <- "listof"
  print.listof(coefs)

  # print variance-covariance matrices
  if (correlation) {
    cat("\nVariance-covariance matrices :\n")
    varcov <- x[(num_meth + 1):(2 * num_meth)]
    class(varcov) <- "listof"
    print.listof(varcov)
  }

  invisible(x)
}

##' S3method confint regsdml
confint.regsdml <- function(object,
                            parm = NULL,
                            level = 0.95,
                            print_regsDML = NULL,
                            print_safety = NULL,
                            print_DML = NULL,
                            print_regDML = NULL,
                            print_regDML_all_gamma = !is.null(parm),
                            print_gamma = FALSE, ...) {

  # check if object is fitted with the function regsdml
  stopifnot(inherits(object, "regsdml"))

  # assign printing variables if they are not specified, that is,
  # they still equal NULL
  print_vars <- set_print_vars(object = object,
                               print_regsDML = print_regsDML,
                               print_safety = print_safety,
                               print_DML = print_DML,
                               print_regDML = print_regDML,
                               print_regDML_all_gamma = print_regDML_all_gamma)
  print_regsDML <- print_vars["print_regsDML"]
  print_safety <- print_vars["print_safety"]
  print_DML <- print_vars["print_DML"]
  print_regDML <- print_vars["print_regDML"]
  print_regDML_all_gamma <- print_vars["print_regDML_all_gamma"]
  names(print_regsDML) <- names(print_safety) <- names(print_DML) <-
    names(print_regDML) <- names(print_regDML_all_gamma) <- NULL

  check_feasibility(object = object,
                    print_regsDML = print_regsDML,
                    print_DML = print_DML,
                    print_safety = print_safety,
                    print_regDML = print_regDML,
                    print_regDML_all_gamma = print_regDML_all_gamma,
                    parm = parm)

  # extract some basic information from object
  alpha <- 1 - level
  if (print_regDML_all_gamma) {
    gamma <- attributes(object)$gamma
  }

  level_fit <- as.character(attributes(object)$level)

  # total number of return quantities
  length_CI <-
    as.numeric(print_regsDML + print_regDML + print_DML +
                 print_safety * !is.null(object$regDML_safety_statistics$beta_safety)) +
    length(parm)
  # initialize return objects
  CI_names <- vector()
  CI_all <- vector(mode = "list", length = length_CI)

  # set counter that will be increased after every return quantity has
  # been added to the return objects
  counter <- 1

  # regsDML
  if (print_regsDML) {
    # extract some information to build the method's title
    message_regsDML <- object$regsDML_statistics$message_regsDML
    add_info <- if (grepl("reg", message_regsDML)) {
      gamma_aN <- object$regsDML_statistics$gamma_aN
      func_tmp_1 <- function(gamma_aN) {
        paste(" (", formatC(gamma_aN, format = "e", digits = 2), ")", sep = "")
      }
      ifelse(print_gamma, func_tmp_1(gamma_aN = gamma_aN), "")
    } else {
      " (DML)"
    } # end add_info
    # compute and save CIs
    func_tmp_2 <- function(add_info) {
      paste("regsDML", add_info, sep = "")
    }
    CI_names <- c(CI_names, func_tmp_2(add_info = add_info))
    CI_all[[counter]] <- if (level == level_fit) {
      object$regsDML_statistics$CI_regsDML
    } else {
      get_CI_DML(beta = object$regsDML_statistics$beta_regsDML,
                 sd = object$regsDML_statistics$sd_regsDML,
                 alpha = alpha)
    }
    # increase counter
    counter <- counter + 1
  }

  # safety device
  if (print_safety) {
    # if the safety device was applicable, output results.
    # otherwise, issue a warning
    if (!is.null(object$regDML_safety_statistics$beta_safety)) {
      # extract some information to build the method's title
      safety_factor <- as.character(attributes(object)$safety_factor)
      gammasafety <-
        as.character(formatC(object$regDML_safety_statistics$gamma_safety,
                             format = "e", digits = 2))
      # compute, build, and save CIs
      func_tmp_3 <- function(print_gamma, safety_factor, gammasafety) {
        ifelse(print_gamma, sprintf(" (factor = %s, gamma = %s)",
                                    safety_factor, gammasafety),
               "")
      }
      CI_names <- c(CI_names, paste("safety-device",
                                    func_tmp_3(print_gamma = print_gamma,
                                               safety_factor = safety_factor,
                                               gammasafety = gammasafety),
                                    sep = ""))
      CI_all[[counter]] <- if (level == level_fit) {
        object$regDML_safety_statistics$CI_safety
      } else {
        get_CI_DML(beta = object$regDML_safety_statistics$beta_safety,
                   sd = object$regDML_safety_statistics$sd_safety,
                   alpha = alpha)
      } # end CI_all[[counter]]
      # increase the counter
      counter <- counter + 1
    } else { # if no safety output is available but its printing is TRUE
      warning("safety device was not applicable, thus cannot be returned")
    }
  }

  # DML
  if (print_DML) {
    # compute and save CIs
    CI_names <- c(CI_names, "DML")
    CI_all[[counter]] <- if (level == level_fit) {
      object$DML_statistics$CI_DML
    } else {
      get_CI_DML(beta = object$DML_statistics$beta_DML,
                 sd = object$DML_statistics$sd_DML,
                 alpha = alpha)
    } # end CI_all[[counter]]
    # increase the counter
    counter <- counter + 1
  }

  # regDML
  if (print_regDML) {
    # extract some information to build the method's title
    gammaopt <-
      as.character(formatC(object$regDML_statistics$gamma_aN,
                           format = "e", digits = 2))
    # compute, build, and save CIs
    func_tmp_4 <- function(print_gamma) {
      ifelse(print_gamma, sprintf(" (%s)", gammaopt), "")
    }
    CI_names <- c(CI_names,
                  paste("regDML", func_tmp_4(print_gamma), sep = ""))
    CI_all[[counter]] <- if (level == level_fit) {
      object$regDML_statistics$CI_regDML
    } else {
      get_CI_DML(beta = object$regDML_statistics$beta_regDML,
                 sd = object$regDML_statistics$sd_regDML,
                 alpha = alpha)
    } # end CI_all[[counter]]
    # increase the counter
    counter <- counter + 1
  }

  # regDML_all_gamma
  if (print_regDML_all_gamma) {
    # build titles for the individual CIs from this method
    func_tmp_5 <- function(x, print_gamma) {
      paste("regDMLall",
            ifelse(print_gamma,
                   paste(" (",
                         formatC(x, format = "e", digits = 2),
                         ")",
                         sep = ""),
                   ""),
            sep = "")
    }
    new_names <- sapply(gamma[parm], func_tmp_5, print_gamma = print_gamma)
    CI_names <- c(CI_names, new_names)
    # compute the CIs
    for (i in parm) {
      CI_all[[counter]] <- if (level == level_fit) {
        object$regDML_all_gamma_statistics[[i]]$CI_regDML_all_gamma
      } else {
        get_CI_DML(beta = object$regDML_all_gamma_statistics[[i]]$beta_regDML_all_gamma,
                   sd = object$regDML_all_gamma_statistics[[i]]$sd_regDML_all_gamma,
                   alpha = alpha)
      }
      # increase the counter
      counter <- counter + 1
    }
  }

  # assign names, and return the object
  names(CI_all) <- CI_names
  attr(CI_all, "level") <- level
  class(CI_all) <- "confint.regsdml"
  CI_all
}

##' S3method print confint.regsdml
print.confint.regsdml <- function(x, ...) {
  # if the return quantities should be printed and not saved in an object,
  # display a title
  cat(sprintf("\nTwo-sided confidence intervals at level %s : \n\n", attr(x, "level")))

  class(x) <- "listof"
  print.listof(x)
  invisible(x)
}

##' S3method print regsdml
print.regsdml <- function(x, ...) {
  # check if the object is of class regsdml
  stopifnot(inherits(x, "regsdml"))

  # output the summary of object
  print.summary.regsdml(summary.regsdml(object = x, ...), ...)
}

##' S3method vcov regsdml
vcov.regsdml <- function(object,
                         print_regsDML = NULL,
                         print_safety = NULL,
                         print_DML = NULL,
                         print_regDML = NULL,
                         print_regDML_all_gamma = !is.null(parm),
                         parm = NULL,
                         print_gamma = FALSE, ...) {
  # check if the object is of class regsdml
  stopifnot(inherits(object, "regsdml"))

  # get summary that contains all the variance-covariance matrices
  res <- summary.regsdml(object = object,
                         print_regsDML = print_regsDML,
                         print_safety = print_safety,
                         print_DML = print_DML,
                         print_regDML = print_regDML,
                         print_regDML_all_gamma = print_regDML_all_gamma,
                         parm = parm,
                         correlation = TRUE,
                         print_gamma = print_gamma)
  len_vcov <- length(res) / 2
  vcov <- res[(len_vcov + 1):(2 * len_vcov)]
  class(vcov) <- "vcov.regsdml"
  vcov
}

##' S3method print summary.regsdml
print.vcov.regsdml <- function(x, ...) {
  # print variance-covariance matrices
  cat("\nVariance-covariance matrices :\n")
  class(x) <- "listof"
  print.listof(x)

  invisible(x)
}
