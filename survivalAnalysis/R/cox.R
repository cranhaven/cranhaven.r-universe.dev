
.coxCallCoefficients <- function(coxphOrSummaryOrFormula)
{
  if (is_formula(coxphOrSummaryOrFormula))
    formula <- coxphOrSummaryOrFormula
  else
    formula <- coxphOrSummaryOrFormula[["call"]][["formula"]]
  rhs <- f_rhs(formula)
  return (symbol_string_list(!!rhs))
}

.coxEndpoint <- function(coxphOrSummaryOrFormula)
{
  if (is_formula(coxphOrSummaryOrFormula))
    formula <- coxphOrSummaryOrFormula
  else
    formula <- coxphOrSummaryOrFormula[["call"]][["formula"]]
  lhs <- f_lhs(formula)
  symbolStrings <- symbol_string_list(!!lhs)
  return(symbolStrings[[1]][[2]])
}

#' Turns a multivariate analysis result to a data frame
#'
#' Extracts useful information into a data frame which is ready
#' for printing or further analysis
#'
#' @param result An object of class "SurvivalAnalysisMultivariateResult"
#'     as returned by \code{\link{analyse_multivariate}}
#' @param factor_id_sep The frame contains one column "factor.id" which is a composite of covariate name and,
#'     if categorical, the factor level (one line for each factor level except for the reference level)
#' @param sort_by A vars() list of one or more symbolic column names.
#'    This frame contains the variables "Lower_CI", "HR", "Upper_CI", "Inv_Lower_CI", "Inv_HR", "Inv_Upper_CI", "p".
#'    You can choose to sort by any combination. Use desc() to sort a variable in descending order.
#'
#' @return A tibble.
#' @export
multivariate_as_data_frame <- function(result, factor_id_sep=":", sort_by = NULL)
{
  cox_as_data_frame(result$coxphSummary,
                    unmangle_dict=result$unmangleDict,
                    factor_id_sep = factor_id_sep,
                    sort_by = sort_by)
}


#' Turns a coxph result to a data frame
#'
#' Extracts useful information from a coxph/summary.coxph into a data frame which is ready
#' for printing or further analysis
#'
#' @param coxphsummary The summary.coxph or coxph result object
#' @param unmangle_dict An unmangle dict of mangled column name -> readable column name (as created by analyse_multivariate)
#' @param factor_id_sep The frame contains one column "factor.id" which is a composite of covariate name and,
#'     if categorical, the factor level (one line for each factor level except for the reference level)
#' @param sort_by A vars() list of one or more symbolic column names.
#'    This frame contains the variables "Lower_CI", "HR", "Upper_CI", "Inv_Lower_CI", "Inv_HR", "Inv_Upper_CI", "p".
#'    You can choose to sort by any combination. Use desc() to sort a variable in descending order.
#'
#' @return A tibble.
#' @export
cox_as_data_frame <- function(coxphsummary, unmangle_dict=NULL, factor_id_sep=":", sort_by = NULL)
{
  if (inherits(coxphsummary, "coxph"))
  {
    coxphsummary <- summary(coxphsummary)
  }
  if (!inherits(coxphsummary, "summary.coxph"))
  {
    if (inherits(coxphsummary, "coxph.null"))
    {
      warning("cox_as_data_frame: Encountered a null model. Returning NULL.")
      return ()
    }
    stop("cox_as_data_frame: Invalid argument. Must have class summary.coxph")
  }
  allHRCIs <- matrix(ncol = 7, nrow = 0)
  colnames(allHRCIs) <- c("Lower_CI", "HR", "Upper_CI", "Inv_Lower_CI", "Inv_HR", "Inv_Upper_CI", "p")
  for (i in 1:dim(coxphsummary$coefficients)[1])
  {
    hrcis     <- matrix(
      c( exp(coxphsummary$coefficients[i, "coef"] + c(qnorm(0.025), 0, qnorm(0.975)) * coxphsummary$coefficients[i, "se(coef)"]),
         exp(-coxphsummary$coefficients[i, "coef"] + c(qnorm(0.025), 0, qnorm(0.975)) * coxphsummary$coefficients[i, "se(coef)"]),
         coxphsummary$coefficients[i, "Pr(>|z|)"]),
      nrow = 1, byrow = TRUE)
    allHRCIs <- rbind(allHRCIs, hrcis)
  }

  symbol_substring <- function(str, symbols)
  {
    # order by longest first, so that shorter symbols do not match the beginning of longer symbols
    symbols <- symbols[order(-str_length(symbols), symbols)]
    # regexp to match at the start of the string
    regexps <- str_c("^", symbols)
    for (i in seq_along(symbols))
    {
      result <- str_locate(str, regexps[[i]])
      if (truthy(result))
      {
        return(symbols[[i]])
      }
    }
  }

  value_names <- function(coefficient_labels, symbol_names)
  {
    value_names <- map2_chr(coefficient_labels, symbol_names,
                            ~str_sub(.x, str_locate(.x, .y)[1,2]+1))
    ifelse(value_names=="", "<continuous>", value_names)
  }

  factor_ids <- function(factor_names, factor_values)
  {
    ifelse(factor_values == "<continuous>",
           factor_names,
           str_c(factor_names, factor_id_sep, factor_values))
  }

  # a composite of symbol/factor name and factor value
  coefficient_labels <- rownames(coxphsummary$coefficients)
  call_symbols <- purrr::simplify(.coxCallCoefficients(coxphsummary))

  data.frame(allHRCIs) %>%
    mutate(
      factor.name = map_chr(coefficient_labels, symbol_substring, call_symbols),
      factor.value = value_names(coefficient_labels, factor.name),
      # unmangle only after using it for value extraction
      factor.name = lookup_chr(unmangle_dict, factor.name, default = identity),
      factor.id = factor_ids(factor.name, factor.value)) %>%
    when(invalid(sort_by) ~ .,
         ~ arrange(., !!!sort_by)) %>%
    select(factor.id, factor.name, factor.value, HR, Lower_CI, Upper_CI, Inv_HR, Inv_Lower_CI, Inv_HR, Inv_Upper_CI, p)
}


#' Access individual components of multivariate survival analysis
#'
#' Allows access to the \code{\link{analyse_multivariate}} result object.
#'
#' @param result An object of class SurvivalAnalysisMultivariateResult
#'               as returned by \code{\link{analyse_multivariate}}
#' @param term   The item to be retrieved:
#'    \itemize{
#'      \item \code{"coxph"} containing the result of the \code{\link[survival]{coxph}} function
#'      \item \code{"summary"} containing the result of the \code{\link{summary}} of the "coxph" result
#'      \item \code{"summary_data_frame"} containing summary as a data frame (see \code{\link{multivariate_as_data_frame}})
#'      \item \code{"p"} A vector of p values for the covariates, equivalent to the "p" column of "summary_data_frame"
#'      \item \code{"overall"} A named list with human-readable labels giving information about the overall fit,
#'                             including the three flavors of p values contained in "summary"
#'    }
#' @return object as specified by \code{term}, or NULL if not contained in \code{result}

#' @export
#'
#' @examples
#' library(magrittr)
#' library(dplyr)
#' survival::colon %>%
#'    analyse_multivariate(vars(time, status),
#'                         vars(rx, sex, age, obstruct, perfor, nodes, differ, extent)) %>%
#'    pluck_multivariate_analysis("p")
#'    print
pluck_multivariate_analysis  <- function(result, term)
{
  if (!inherits(result, "SurvivalAnalysisMultivariateResult"))
  {
    stop("pluck_survival_analysis called with inappropriate object, type ", type_of(result), " class ", class(result))
  }

  if (term == "coxph")
  {
    result[["coxph"]]
  }
  else if (term == "summary")
  {
    result[["summary"]]
  }
  else if (term == "summary_data_frame")
  {
    result[["summaryAsFrame"]]
  }
  else if (term == "p")
  {
    pluck(result, "summaryAsFrame", "p")
  }
  else if (term == "overall")
  {
    result[["overall"]]  }
  else
  {
    stop("pluck_multivariate_analysis called with inappropriate term ", term)
  }
}

#' Formats a SurvivalAnalysisMultivariateResult for printing
#'
#' @param x The result generated by \code{\link{analyse_multivariate}}
#' @param ... Further arguments passed from other methods.
#' @param p_precision,hr_precision Precision with which to print floating point values
#' @param p_less_than_cutoff Cut-off for small p values. Values smaller than this will be displayed like "<..."
#'
#' @return A formatted string, ready for output with cat()
#' @export
format.SurvivalAnalysisMultivariateResult <- function(x,
                                                      ...,
                                                      p_precision = 3,
                                                      hr_precision = 2,
                                                      p_less_than_cutoff = 0.001)
{
  format_df <- function(df)
  {
    df %>%
      as.data.frame(optional=TRUE) %>% # keep column names unmangled
      {capture.output(print(., row.names=FALSE))} %>%
      str_c(collapse = "\n")
  }

  dfs <- list()

  dfs[["Overall:"]] <-
    x$overall %>%
    list_modify(covariates = str_c(x$overall$covariates, collapse=", ")) %>%
    as_tibble() %>%
    format_p_values_at(vars(`Likelihood ratio test p`,
                            `Wald test p`,
                            `Score (logrank) test p`),
                       decimal_places = p_precision,
                       prefix="",
                       less_than_cutoff = p_less_than_cutoff)


  dfs[["Hazard Ratios:"]] <-
    x$summaryAsFrame %>%
    format_numbers_at(vars(HR, Lower_CI, Upper_CI,
                           Inv_HR, Inv_Lower_CI, Inv_Upper_CI),
                      decimal_places = hr_precision) %>%
    format_p_values_at(vars(p),
                       decimal_places = p_precision,
                       prefix="",
                       less_than_cutoff = p_less_than_cutoff)


  df_texts <- map(dfs, format_df)

  c(rbind(names(dfs),
          df_texts,
          rep("", length(dfs))) # inserts additional new lines
  ) %>%
    str_c(collapse="\n")
}

#' Print the essentials of a SurvivalAnalySurvivalAnalysisMultivariateResult
#'
#' @inheritParams format.SurvivalAnalysisMultivariateResult
#' @return The formatted string, invisibly.
#' @export
print.SurvivalAnalysisMultivariateResult <- function(x,
                                                     ...,
                                                     p_precision = 3,
                                                     hr_precision = 2,
                                                     p_less_than_cutoff = 0.001)
{
  str <- format.SurvivalAnalysisMultivariateResult(x)
  cat(str)
  cat("\n")
  invisible(x)
}


# Perform multivariate analysis for the given survival endpoint, including the given covariates.
# The covariates can be given as character (column names) or symbols (using vars()).
# For categorical variables, the Cox regression uses pseudo variables for each level
# relative to a reference category, resulting in n-1 variables for n levels of a categorical endpoint.
# Per default, the reference level is the first factor level. You can specify a different level
# by passing a named vector: factor name -> value of reference level to reference_level_dict.
#' Multivariate analysis (Cox Regression)
#'
#' Performs Cox regression on right-censored data using a multiple covariates.
#'
#' This method builds upon the \code{survival} package and returns a comprehensive result object
#' for survival analysis containing the coxph results.
#' A \code{format}/\code{print} method is provided that prints the essential statistics.
#'
#' @param data A data frame containing the time/status information and, if used, the covariate.
#' @param time_status A vector of length 2 giving the time and status fields.
#'    It is recommended to use vars() and symbolic column names or code that is tidily-evaluated on \code{data}.
#'    You can also pass a character vector with the column names or a numeric vector with column indices.
#' @param covariates The covariates.
#'    Pass symbolic columns names or code that is tidily-evaluated on \code{data}.
#'    Column names or column indices are also possible.
#'    In any case, factors with appropriate labels will be generated which in all printouts.
#'    You can use \code{covariate_name_dict} and \code{covariate_label_dict} to rename these factors and their levels.
#' @param interaction_covariates Interactions (optional). Same format as covariates.
#'    Covariates to include together with their interaction (*, not + in the formula)
#' @param strata Strata (optional). Same format as covariates.
#'    For each strata level (if multiple fields, unique combinations of levels) a separate baseline hazard is fit.
#' @param covariate_name_dict A dictionary (named list or vector) of old->new covariate names
#' @param covariate_label_dict A dictionary (named list or vector) of old->new covariate value level labels
#' @param reference_level_dict For categorical variables, the Cox regression uses pseudo variables for each level
#'    relative to a reference category, resulting in n-1 variables for n levels of a categorical covariate.
#'    Hazard ratios will be relative to the reference level, which is defined as having hazard ratio 1.0.
#'    Per default, the reference level is the first factor level.
#'    You can specify a different level by passing a named vector: factor name -> value of reference level.
#'    Note that this is independent of covariate_label_dict, i.e. specify the factor level as it is in \code{data}#'
#' @param sort_frame_by A vars() list of one or more symbolic column names.
#'    The result contains a data frame of the cox regression results (\code{\link{cox_as_data_frame}}).
#'    This frame contains the variables "Lower_CI", "HR", "Upper_CI", "Inv_Lower_CI", "Inv_HR", "Inv_Upper_CI", "p".
#'    You can specify by which variables the frame should be sorted. Default: Hazard Ratio.
#'
#' @return An object of class "SurvivalAnalysisResult" and "SurvivalAnalysisMultivariateResult".
#'    You can use this result as a black box for further functions in this package,
#'    \code{\link[=format.SurvivalAnalysisMultivariateResult]{format}} or
#'    \code{\link[=print.SurvivalAnalysisMultivariateResult]{print}} it,
#'    retrieve information as a data frame via \code{\link{multivariate_as_data_frame}} or
#'    access individual pieces via \code{\link{pluck_multivariate_analysis}}
#'
#' @seealso \code{\link{forest_plot}}
#' @export
#'
#' @examples
#' library(magrittr)
#' library(dplyr)
#' survival::colon %>%
#'    analyse_multivariate(vars(time, status),
#'                         vars(rx, sex, age, obstruct, perfor, nodes, differ, extent)) %>%
#'    print()
analyse_multivariate <- function(data,
                                 time_status,
                                 covariates,
                                 interaction_covariates = NULL,
                                 strata = NULL,
                                 covariate_name_dict = NULL,
                                 covariate_label_dict = NULL,
                                 reference_level_dict = NULL,
                                 sort_frame_by = vars(HR))
{
  covariates <- maybe_missing(covariates, default=NULL)
  if (invalid(covariates) && invalid(interaction_covariates))
    stop("multivariate analysis: Must provide covariates (at least one, usually multiple)")

  values <- list()

  survival_col_dfs <- .build_survival_columns(data, time_status)
  factor_col_dfs <- .build_columns(data, covariates)
  interaction_col_dfs <- .build_columns(data, interaction_covariates)
  strata_col_dfs <- .build_columns(data, strata)
  # ...col_dfs are named lists of original name -> tibbles with one column, where column name is syntactically correct

  time_status <- map_chr(survival_col_dfs, colnames)
  covariates <- map_chr(factor_col_dfs, colnames)
  interactions <- map_chr(interaction_col_dfs, colnames)
  strata <- map_chr(strata_col_dfs, colnames)

  all_col_dfs <- c(survival_col_dfs, factor_col_dfs,
                   interaction_col_dfs, strata_col_dfs)
  data <- bind_cols(all_col_dfs, .name_repair = "minimal")
  # for our use, build a dict original name -> mangled name
  colname_dict <- set_names(colnames(data), nm=names(all_col_dfs))
  # store a dict of mangled name -> original name
  values[["colname_unmangle_dict"]] <- invert_value_and_names(colname_dict)

  values[["data"]] <- data
  values[["data_attributes"]] <- list(survival_ids = time_status,
                                      factor_ids = covariates,
                                      interaction_ids = interactions,
                                      strata_ids = strata)

  # remove dict entries for columns which we did not get at all as covariates
  reference_level_dict <- reference_level_dict[names(reference_level_dict) %in% names(colname_dict)]
  if (!invalid(reference_level_dict))
  {
    categorical_factor_colnames <- lookup_chr(colname_dict, names(reference_level_dict))
    for (i in seq_along(categorical_factor_colnames))
    {
      colname <- categorical_factor_colnames[[i]]
      dict_value <- reference_level_dict[[i]]
      data %<>%
        mutate(!!colname := fct_relevel(!!sym(colname), dict_value))
    }
  }

  survFormula <- as.formula(str_c("Surv(",time_status[[1]],",",time_status[[2]],") ~ ",
                                  str_c(covariates, collapse = " + "),
                                  str_c(interactions, collapse = " * "),
                                  str_c(map_chr(strata, ~str_c(" + strata(", ., ")")),
                                        collapse = "")
                                  ))
  eval_unquoted(
    values$coxph <- coxph(!!survFormula, data = data)
  )
  values$summary <- summary(values$coxph)
  values$summaryAsFrame <-
    cox_as_data_frame(values$summary,
                      unmangle_dict = values[["colname_unmangle_dict"]],
                      sort_by = sort_frame_by) %>%
    mutate(factor.name = lookup_chr(covariate_name_dict, factor.name, default = identity),
           factor.value = lookup_chr(covariate_label_dict, factor.value, default = identity))
  values$overall <- list("n" = values %>% pluck("summary", "n", .default = na_dbl),
                         "covariates" = names(factor_col_dfs),
                         "Likelihood ratio test p" = values %>% pluck("summary", "logtest", "pvalue", .default = na_dbl),
                         "Wald test p" = values %>% pluck("summary", "waldtest", "pvalue", .default = na_dbl),
                         "Score (logrank) test p" = values %>% pluck("summary", "sctest", "pvalue", .default = na_dbl))
  if (!invalid(interactions))
    values$overall$interactions <- names(interaction_col_dfs)
  if (!invalid(strata))
    values$overall$strata <- names(strata_col_dfs)

  values[["formula"]] <- as.character(values$summary$call)[2]
  class(values) <- c("SurvivalAnalysisResult", "SurvivalAnalysisMultivariateResult", "list")
  return(values)
}

#' @export
#' @rdname analyse_multivariate
analyze_multivariate <- analyse_multivariate


# Legacy
multivariate_cox <- function(data,
                             survival_fields,
                             factors,
                             strata = NULL,
                             factor_name_dict = NULL,
                             factor_label_dict = NULL,
                             reference_level_dict = NULL,
                             sort_frame_by = vars(HR))
{
  analyse_multivariate(data,
                       survival_fields,
                       factors,
                       strata,
                       factor_name_dict,
                       factor_label_dict,
                       reference_level_dict,
                       sort_frame_by)
}

