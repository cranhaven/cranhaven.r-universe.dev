#' Class of models fitted by \pkg{RegDDM}
#'
#' @description
#' \code{regddmfit} is an S3 object storing the fitted models of \pkg{RegDDM}.
#' It contains information used to fit the model and the resulting
#' \code{\link[rstan]{stanfit}} and can be summarized and printed using
#' \code{\link[=summary.regddmfit]{summary}} and \code{\link[=print.regddmfit]{print}}.
#'
#' @details
#' Use \code{methods(class = "regddmfit")} for a list of available methods.
#'
#' @param data1 Subject-level data frame.
#' @param data2 Trial-level data frame.
#' @param model A list containing 0-5 formulas, specifying the dependence
#' structure between variables.
#' @param family Family of distribution of the outcome.
#' @param stan_fit Fitted \code{stan} model.
regddmfit <- function(data1,data2,model,family,stan_fit){
  x = list(
    data1 = data1,
    data2 = data2,
    model = model,
    family = family,
    stan_fit = stan_fit
  )
  class(x) <- "regddmfit"
  return(x)
}

#' Summary of \pkg{RegDDM} fit object
#'
#' @description
#' Summarize the posterior distributions of estimated parameters and group them into
#' four categories.
#'
#' @param object A \code{\link{regddmfit}} object to summary
#' @param ... parameters passed to \code{\link[rstan]{summary,stanfit-method}}
#'
#' @return The summary method returns a named list of \code{glm_coefficiets},
#'    \code{subject_ddm_param}, \code{group_param}, and \code{missing_value}.
#'    Each element is a \code{\link[tibble]{tibble}} data frame of posterior
#'    summary statistics of the regression coefficient, DDM parameter of each
#'    subject, group mean and standard deviation of DDM parameters and covariates,
#'    plus the estimated missing values.
#'
#' @seealso \code{\link{print.regddmfit}} a printed summary of the \code{regddmfit} object.
#'
#' @export
summary.regddmfit = function(object, ...){
  # For unfitted models, return NULL
  if(is.null(object$stan_fit)){
    warning("The RegDDM model hasn't been fit. Returning NULL.")
    return(NULL)
  }
  # Otherwise, return summary statistics.
  return(summary_results(object$stan_fit, object$data1, ...))
}

#' Printed summary of \pkg{RegDDM} fit object
#'
#' @description
#' Summarize the posterior distributions of estimated regression coefficients
#' and print necessary information.
#'
#' @param x An \code{\link{regddmfit}} object to print
#' @param digits digits of the output results. Default value is 3.
#' @param ... Unused...
#'
#' @seealso \code{\link{summary.regddmfit}} Table summaries of the \code{regddmfit} object.
#'
#' @return No values are returned.
#'
#' @export
print.regddmfit = function(x, digits = 3, ...){
  tmp_summary = summary.regddmfit(x)
  output = "RegDDM Model Summary\n"
  output = paste0(output, "Number of subjects: ", nrow(x$data1), "\n")
  output = paste0(output, "Number of trials: ", nrow(x$data2), "\n")
  output = paste0(output, "Model:\n")
  for(m in x$model){
    output = paste0(output, "  ", deparse(m), "\n")
  }
  output = paste0(output, "Family: ", x$family, "\n")

  # For unfitted models
  if(is.null(x$stan_fit)){
    output = paste0(output, "Sampling not done!")
    cat(output)
    return(NULL)
  }

  sim = x$stan_fit@sim
  longest_time = rstan::get_elapsed_time(x$stan_fit)
  longest_time = round(max(longest_time[,1]+longest_time[,2]), 0)
  output = paste0(
    output, "Sampling: ",
    stringr::str_interp(
      "${sim$chains} chains, ${sim$warmup} warmups and ${sim$iter} iterations were used. "
    ),
    stringr::str_interp("Longest elapsed time is ${longest_time} s.\n\n")
  )

  output = paste0(output, "Regression coefficients:\n")
  glm_coefficients = dplyr::select(
    tmp_summary$glm_coefficiets,
    "variable", "mean", "sd", "2.5%", "97.5%", "n_eff", "Rhat"
  )
  glm_coefficients = as.data.frame(glm_coefficients)
  output = paste0(
    output,
    paste(utils::capture.output(print(glm_coefficients,digits = digits)), collapse = "\n")
  )
  output = paste0(output, "\nMaximum R-hat: ", round(tmp_summary$max_rhat, digits))

  cat(output)
}


#' Get the rstan fit of regddmfit objects
#' @description
#' Get the \code{\link[rstan]{stanfit}} object of the \code{\link{regddmfit}}
#' object to perform further analysis and diagnosis.
#'
#' @param fit A \code{regddmfit} object to summary
#' @return A \code{stanfit} object.
#' @export
get_stan_fit <- function(fit){
  return(fit$stan_fit)
}



