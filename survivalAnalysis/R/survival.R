#' Univariate survival analysis
#'
#' Performs survival analysis on right-censored data using a single covariate, or no covariate.
#'
#' This method builds upon the \code{survival} package and returns a comprehensive result object
#' for survival analysis containing the survfit, survdiff and coxph results.
#' A \code{format}/\code{print} method is provided that prints the essential statistics.
#' Kaplan-Meier plots are readily generated using the \code{\link{kaplan_meier_plot}} or
#' \code{\link{kaplan_meier_grid}} functions.
#'
#' @param data A data frame containing the time/status information and, if used, the covariate.
#' @param time_status A vector of length 2 giving the time and status fields.
#'    It is recommended to use vars() and symbolic column names or code that is tidily-evaluated on \code{data}.
#'    You can also pass a character vector with the column names or a numeric vector with column indices.
#' @param by The term by which survival curves will be separated.
#'    Pass \code{NULL} or omit to generate a single curve and only descriptive statistics.
#'    Pass symbolic columns names or code that is tidily-evaluated on \code{data} to generate more than one curve,
#'    and the appropriate statistics to compare the curves. A column name or column index is also possible.
#'    In any case, the parameter will be used to create a factor with appropriate labels.
#'    This factor will appear in all printouts and plots.
#'    You can use \code{by_label_map} and \code{by_order_vector} to rename and reorder this factor.
#' @param by_label_map A dictionary (named list or vector) of old->new labels of the factor created using \code{by}.
#'    The factor will be renamed accordingly, and also reordered by the order of the vector.
#' @param by_order_vector A vector of the labels of the factor created using \code{by},
#'    after renaming them based on \code{by_label_map} (so specify the "new" level).
#'    The factor will be ordered according to the order of this vector.
#'    It need not contain all elements, only those found will be reorder at the top.
#' @param cox_reference_level The result will include a univariate Cox regression. Use this parameter to specify
#'    the level of the factor generated using \code{by} that you want to use a the reference level
#'    (Hazard ratios will be relative to the reference level, which is defined as having hazard ratio 1.0)
#'    Note that the given string applies after all renaming has been done, so specify the "new" level.
#' @param p_adjust_method If there are more than two levels in the \code{by} factor,
#'    the result will include the return value of pairwise_survdiff, which performs p adjustment.
#'    You can specify the desired method here.
#'    Note that other p values are not corrected, this is beyond the scope of this method.
#' @param plot_args Named list of arguments that will be stored for later use in plotting methods,
#'    such as kaplan_meier_plot. There they will take precedence over arguments given to that method.
#'    This is useful when plotting multiple results with a set of default arguments, of which some
#'    such as title or axis scale differ per-plot.
#'
#' @return An object of class "SurvivalAnalysisResult" and "SurvivalAnalysisUnivariateResult".
#'    You can use this result as a black box for further functions in this package,
#'    \code{\link[=format.SurvivalAnalysisUnivariateResult]{format}} or
#'    \code{\link[=print.SurvivalAnalysisUnivariateResult]{print}} it,
#'    retrieve information as a data frame via \code{\link{survival_data_frames}} or
#'    access individual pieces via \code{\link{pluck_survival_analysis}}
#'
#' @export
#'
#' @examples
#' library(magrittr)
#' library(dplyr)
#' survival::aml %>%
#'   analyse_survival(vars(time, status), x) %>%
#'   print

analyse_survival  <- function(data,
                              time_status,
                              by,
                              by_label_map = NULL,
                              by_order_vector = NULL,
                              cox_reference_level = NULL,
                              p_adjust_method = "none",
                              plot_args = list())
{
  factorQuosure <- enquo(by)

  if (!is.data.frame(data))
  {
    stop("Expecting a data frame as data argument, got an object of class ", class(data))
  }

  if (is_missing(time_status))
  {
    stop("The time_status parameter is missing")
  }

  values <- list()

  # build data, evaluating "by" and time_status, making syntactic names
  col_dfs <- list()
  # build factor column
  if (quo_is_null(factorQuosure) || quo_is_missing(factorQuosure))
  {
    has_strata <- F
  }
  else
  {
    has_strata <- T
    data %>%
      .build_column(factorQuosure, make_factor = TRUE) ->
    col_dfs
  }

  # build survival columns
  .build_survival_columns(data, time_status) %>%
    prepend(col_dfs) ->
  col_dfs

  # col_dfs is a named list original names -> tibbles with one column, where column name is syntactically correct
  # build the subset data frame
  data <- bind_cols(col_dfs, .name_repair = "minimal")
  # build a dict of mangled name -> original name
  colname_unmangle_dict <- set_names(names(col_dfs), colnames(data))
  values[["colname_unmangle_dict"]] <- colname_unmangle_dict
  values[["by"]] = names(col_dfs)[[1]]

  if (has_strata)
  {
    time_status <- colnames(data)[2:3]
    factorId <- colnames(data)[[1]]
    factorSymbol <- sym(factorId)

    data %<>%
      # check if it is a factor, rename and reorder as told by the user
      mutate(!!factorSymbol := rename_reorder_factor(!!factorSymbol, by_label_map, by_order_vector)) %>%
      # drop NA in our endpoints (which would be ignored anyway),
      drop_na(one_of(time_status)) %>%
      # but then adjust the factor levels, some may be gone.
      mutate(!!factorSymbol := fct_drop(!!factorSymbol))
  }
  else
  {
    time_status <- colnames(data)
    data %<>%
      drop_na()
  }

  # Is all gone?
  if (nrow(data) == 0)
  {
    warning("Survival analysis with endpoint ",
            str_c(time_status, collapse = " "),
            ": No survival data available (all NA?), returning NULL.")
    return(NULL)
  }

  # We had a factor, but after removing na etc., there is only one stratum left
  if (has_strata && length(unique(data[[factorId]])) < 2)
  {
    warning(str_c("Survival analysis with factor ", colname_unmangle_dict[[factorId]],
                  ": Only one or no factor value, no diffs possible."))
    has_strata <- F
  }

  if (has_strata)
  {
    # Note: after our modification above, there is no difference between factor levels and labels!
    values[["factorLabels"]] <- levels(data[[factorId]])

    coxFactorId <- factorId
    coxFactorSymbol <- factorSymbol

    if (valid(cox_reference_level))
    {
      coxFactorId <- str_c(factorId, ".factorid.cox")
      coxFactorSymbol <- sym(coxFactorId)
      # add to unmangle dict
      colname_unmangle_dict[[coxFactorId]] <- colname_unmangle_dict[[factorId]]
      values[["colname_unmangle_dict"]] <- colname_unmangle_dict

      if (!is_character(cox_reference_level))
      {
        cox_reference_level <- as.character(cox_reference_level)
      }
      data %<>%
        mutate(!!coxFactorSymbol := fct_relevel(!!factorSymbol, cox_reference_level))
      values[["coxReferenceLevel"]] <- cox_reference_level
    }
    else
    {
      values[["coxReferenceLevel"]] <- pluck(values, "factorLabels", 1)
    }

    data %>%
      group_by(!!factorSymbol) %>%
      summarise(n=n()) %>%
      arrange(n) %>%
      {set_names(.$n, nm=.[[factorId]] %>% str_c())} ->
    values[["factorFrequencies"]]
  }
  else
  {
    factorId <- "1"
    coxFactorId <- "1"
    # values$factorLabels remains NULL
  }

  values[["has_strata"]] <- has_strata
  values[["data"]] <- data
  values[["data_attributes"]] <- list(factor_id = factorId,
                                      survival_ids = time_status)
  values[["plot_args"]] <- plot_args

  # Build formulas
  formulas <- .survivalBuildFormulas(time_status, factorId)
  formula <- formulas[["formula"]]
  formulaOverall <- formulas[["formulaOverall"]]
  formulaCox <- .survivalBuildFormulas(time_status, coxFactorId)[["formula"]]

  eval_unquoted(
  {
    values$fit <- survfit(!!formula, data = data)
    values$fitOverall <- survfit(!!formulaOverall, data = data)
    values$coxph <- coxph(!!formulaCox, data = data)
  })
  if (has_strata)
  {
    eval_unquoted(
    {
      values$diff <- survdiff(!!formula, data = data)

      if (length(values$factorLabels) > 2)
      {
        values$diff_pairwise <-
          pairwise_survdiff(!!formula, data = data, p.adjust.method = p_adjust_method)
      }
      # create a named list factor level -> coxph of this level vs. rest
      values$coxph_onehot <-
        set_names(
          map(values$factorLabels,
              function(level) {
                data %>%
                  mutate(!!factorSymbol := fct_rev(
                    fct_other(!!factorSymbol, keep=level, other_level="control")
                  )) %>%
                  coxph(!!formula, data = .)
              }),
          nm = values$factorLabels
        )
    })
  }


  class(values) <- c("SurvivalAnalysisResult", "SurvivalAnalysisUnivariateResult", "list")
  return(values)
}

#' @export
#' @rdname analyse_survival
analyze_survival <- analyse_survival

