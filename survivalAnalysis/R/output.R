
.timespan_scaling <- function(timespan_unit)
{
  if (startsWith(timespan_unit, "month"))
  {
    30.436875
  }
  else if (startsWith(timespan_unit, "year"))
  {
    365.2425
  }
  else if (startsWith(timespan_unit, "day"))
  {
    1
  }
  else
  {
    warning("Unsupported timespan scaling: ", timespan_unit)
    1
  }
}

.n_strata <- function(result)
{
  length(result$fit$strata)
}

.has_strata <- function(result)
{
  result[["has_strata"]]
}

.stratum_labels <- function(result)
{
  result[["factorLabels"]]
}

.overall_metadata <- function(result,
                              format_numbers,
                              p_precision,
                              time_precision,
                              p_less_than_cutoff,
                              timespanUnit)
{
  timespanScaling <- .timespan_scaling(timespanUnit)
  overall_table <- summary(result$fitOverall)[["table"]] / timespanScaling

  # strange things happen
  lcl_label <- "0.95LCL"
  ucl_label <- "0.95UCL"
  if (!lcl_label %in% names(overall_table))
  {
    lcl_label <- "0,95LCL"
    ucl_label <- "0,95UCL"
  }

  tibble("log.rank.p"=pValueOfSurvDiff(result$diff),
         "n"=sum(result$fit$n),
         "median"=overall_table[["median"]],
         "Lower.CI"=overall_table[[lcl_label]],
         "Upper.CI"=overall_table[[ucl_label]],
         "min"=min(result$fitOverall$time) / timespanScaling,
         "max"=max(result$fitOverall$time) / timespanScaling) %>%
    mutate(label="overall:") ->
  df

  if (format_numbers)
  {
    df %<>%
      format_p_values_at("log.rank.p",
                         decimal_places = p_precision,
                         less_than_cutoff = p_less_than_cutoff) %>%
      format_numbers_at(c("median", "Lower.CI", "Upper.CI", "min", "max"), time_precision)
  }

  df
}

.per_stratum_metadata <- function(result,
                                  format_numbers,
                                  time_precision,
                                  timespanUnit)
{
  if (!.has_strata(result))
  {
    return(NULL)
  }

  timespanScaling <- .timespan_scaling(timespanUnit)

  table <- summary(result$fit)$table

  # strange things happen
  lcl_label <- "0.95LCL"
  ucl_label <- "0.95UCL"
  if (!lcl_label %in% colnames(table))
  {
    lcl_label <- "0,95LCL"
    ucl_label <- "0,95UCL"
  }

  perFactor <- data.frame(table[,c("records", "events", "median", lcl_label, ucl_label)])
  colnames(perFactor) <- c("records", "events", "median", "Lower.CI", "Upper.CI")
  perFactor %<>%
    mutate(label = .stratum_labels(result)) %>%
    mutate_at(c("median", "Lower.CI", "Upper.CI"), ~./timespanScaling)

  if (format_numbers)
  {
    perFactor %<>%
      format_numbers_at(c("median", "Lower.CI", "Upper.CI"), time_precision)
  }

  perFactor
}

.per_stratum_hrs <- function(result,
                             format_numbers,
                             p_precision,
                             hr_precision,
                             p_less_than_cutoff)
{
  if (!.has_strata(result))
  {
    return(NULL)
  }

  coxphsummary <- summary(result$coxph)
  allHRCIs <- matrix(ncol = 4, nrow = 0)
  colnames(allHRCIs) <- c("Lower.CI", "HR", "Upper.CI", "p")
  reference_level <- result[["coxReferenceLevel"]]
  factorLabels <- .stratum_labels(result)
  cox_labels <- factorLabels[factorLabels != reference_level]
  for (i in 1:dim(coxphsummary$coefficients)[1])
  {
    hrcis     <- matrix(
      c( exp(coxphsummary$coefficients[i, "coef"] + c(qnorm(0.025), 0, qnorm(0.975)) * coxphsummary$coefficients[i, "se(coef)"]),
         coxphsummary$coefficients[i, "Pr(>|z|)"],
         exp(-coxphsummary$coefficients[i, "coef"] + c(qnorm(0.025), 0, qnorm(0.975)) * coxphsummary$coefficients[i, "se(coef)"]),
         coxphsummary$coefficients[i, "Pr(>|z|)"] ),
      nrow = 2, byrow = TRUE)
    #TODO: Check if > 2 factors
    hrlabel <- str_c(cox_labels[i], " vs. ", reference_level)
    hrlabelInv <- str_c(reference_level, " vs. ", cox_labels[i])
    rownames(hrcis) <- c(hrlabel, hrlabelInv)
    allHRCIs <- rbind(allHRCIs, hrcis)
  }

  allHRCIs %<>%
    as_tibble() %>%
    mutate(label=rownames(allHRCIs))

  if (format_numbers)
  {
    allHRCIs %<>%
      format_numbers_at(vars(Lower.CI, HR, Upper.CI), hr_precision) %>%
      format_p_values_at(vars(p),
                         decimal_places = p_precision,
                         prefix="",
                         less_than_cutoff = p_less_than_cutoff)
  }

  allHRCIs
}

.stratum_pairwise_p <- function(result,
                                format_numbers,
                                p_precision)
{
  if (.n_strata(result) <= 2)
  {
    return(NULL)
  }

  result$diff_pairwise$p.value %>%
    as_tibble() ->
  df

  if (format_numbers)
  {
    df %<>%
      format_numbers_at(seq(ncol(.)), decimal_places = p_precision)
  }

  df %>%
    mutate(label=rownames(result$diff_pairwise$p.value))
}


.survival_rate <- function(result,
                           times,
                           percentage_decimal_places,
                           time_precision,
                           timespan_unit)
{
  timespan_scaling <- .timespan_scaling(timespan_unit)

  # The "extend" argument mitigates the risk of error when asking for time points without subjects
  summary_data <- summary(result$fit, times, extend=TRUE)

  summary_data[c("time", "n.risk", "n.event", "surv", "lower", "upper")] %>%
    as_tibble() %>%
    select(time,
           n_at_risk = n.risk,
           n_events = n.event,
           rate = surv,
           Lower.CI = lower,
           Upper.CI = upper) %>%
    mutate(
      time = time / timespan_scaling,
      time_label = as_formatted_number(time, time_precision),
      across(rate:Upper.CI, .names = "{.col} percentage",
             ~as_percentage_label(.x, percentage_decimal_places))
      ) ->
    rates

  if (.has_strata(result))
  {
    rates %<>%
      mutate(
        stratum = result$by,
        stratum_value = result$factorLabels[summary_data$strata]) %>%
      relocate(stratum, stratum_value)
  }
  rates
}


#' Extract results from univariate survival analysis structured as data frames
#'
#' @param format_numbers If true, all numbers will be formatted for printing according to the following options
#'                       and will be returned as strings
#' @param result The result generated by \code{\link{analyse_survival}}
#' @param p_precision,hr_precision,time_precision Precision with which to print floating point values
#' @param p_less_than_cutoff Cut-off for small p values. Values smaller than this will be displayed like "<..."
#' @param timespan_unit Unit for time spans: "days", "months" or "years".
#'
#' @return A named list list of data frame objects:\itemize{
#'         \item cohortMetadata: information about the full cohort
#'         \item if there are strata (analysis performed "by" a covariate): \itemize{
#'               \item strataMetadata: information about each stratum
#'               \item hazardRatios: hazard ratios for combinations of strata
#'               \item only if there are more than two strata: \itemize{
#'                     \item pairwisePValues: Matrix of pairwise (uncorrected) p values
#'               }
#'         }
#'    }
#' @export
survival_data_frames <- function(result,
                                 format_numbers = TRUE,
                                 p_precision = 3,
                                 hr_precision = 2,
                                 p_less_than_cutoff = 0.001,
                                 time_precision = 1,
                                 timespan_unit = c("days", "months", "years"))
{
  if (!inherits(result, "SurvivalAnalysisUnivariateResult"))
  {
    stop("survival_data_frames called with inappropriate object, type ", type_of(result), " class ", class(result))
  }

  timespan_unit <- match.arg(timespan_unit)

  l <- list(
    "cohortMetadata" = .overall_metadata(result,
                                         format_numbers,
                                         p_precision,
                                         time_precision,
                                         p_less_than_cutoff,
                                         timespan_unit)
  )

  if (.has_strata(result))
  {
    l <- c(l, list(
      "strataMetadata" = .per_stratum_metadata(result,
                                               format_numbers,
                                               time_precision,
                                               timespan_unit),
      "hazardRatios" = .per_stratum_hrs(result,
                                        format_numbers,
                                        p_precision,
                                        hr_precision,
                                        p_less_than_cutoff)

    ))

    if (.n_strata(result) > 2)
    {
      l <- c(l, list(
        "pairwisePValues" = .stratum_pairwise_p(result,
                                                format_numbers,
                                                p_precision)
      ))
    }
  }
  l
}


#' Access individual components of univariate survival analysis
#'
#' Allows access to the \code{\link{analyse_survival}} result object.
#'
#' @param result An object of class SurvivalAnalysisUnivariateResult
#'               as returned by \code{\link{analyse_survival}}
#' @param term   The item to be retrieved:
#'    \itemize{
#'      \item \code{"survfit"} containing the result of the \code{\link[survival]{survfit}} function
#'      \item \code{"survdiff"} containing the result of the \code{\link[survival]{survdiff}} function
#'      \item \code{"survfit_overall"} containing the result of the \code{\link[survival]{survfit}}
#'            function without terms, i.e. the full group not comparing subgroups
#'      \item \code{"coxph"} containing the result of the \code{\link[survival]{coxph}} function
#'      \item \code{"p"} The log-rank p value (if \code{by} provided at least two strata)
#'    }
#' @return object as specified by \code{term}, or NULL if not contained in \code{result}

#' @export
#'
#' @examples
#' library(magrittr)
#' library(dplyr)
#' survival::aml %>%
#'   analyse_survival(vars(time, status), x) %>%
#'   pluck_survival_analysis("p") %>%
#'   print
pluck_survival_analysis  <- function(result, term)
{
  if (!inherits(result, "SurvivalAnalysisUnivariateResult"))
  {
    stop("pluck_survival_analysis called with inappropriate object, type ", type_of(result), " class ", class(result))
  }

  if (term == "survfit")
  {
    result[["fit"]]
  }
  else if (term == "survfit_overall")
  {
    result[["fitOverall"]]
  }
  else if (term == "coxph")
  {
    result[["coxph"]]
  }
  else if (term == "survdiff")
  {
    result[["diff"]]
  }
  else if (term == "p")
  {
    pValueOfSurvDiff(result[["diff"]])
  }
  else
  {
    stop("pluck_survival_analysis called with inappropriate term ", term)
  }
}

#' Formats a SurvivalAnalysisUnivariateResult for printing
#'
#' @param x The result generated by \code{\link{analyse_survival}}
#' @param label A label describing the result
#' @param p_precision,hr_precision,time_precision Precision with which to print floating point values
#' @param p_less_than_cutoff Cut-off for small p values. Values smaller than this will be displayed like "<..."
#' @param include_end_separator Append "\\n---\\n"? Comes handy if printing multiple results following each other
#' @param timespan_unit Unit for time spans: "days", "months" or "years".
#' @param ... Further arguments passed from other methods.
#'
#' @return A formatted string, ready for output with cat()
#' @export
format.SurvivalAnalysisUnivariateResult <- function(x,
                                                    ...,
                                                    label = NULL,
                                                    p_precision = 3,
                                                    hr_precision = 2,
                                                    p_less_than_cutoff = 0.001,
                                                    time_precision = 1,
                                                    include_end_separator = FALSE,
                                                    timespan_unit = c("days", "months", "years"))
{
  if (!inherits(x, "SurvivalAnalysisUnivariateResult"))
  {
    stop("format.SurvivalAnalysisUnivariateResult called with inappropriate object, type ",
         type_of(x), " class ", class(x))
  }

  timespan_unit <- match.arg(timespan_unit)

  dfs <- list()
  dfs[["Overall Analysis:"]] <-
    .overall_metadata(x,
                      TRUE,
                      p_precision,
                      time_precision,
                      p_less_than_cutoff,
                      timespan_unit)
  dfs[["Descriptive Statistics per Subgroup:"]] <-
    .per_stratum_metadata(x,
                          TRUE,
                          time_precision,
                          timespan_unit)
  dfs[["Pair-wise p-values (log-rank, uncorrected):"]] <-
    .stratum_pairwise_p(x, TRUE, p_precision)

  dfs[[if_else(.n_strata(x)>2,
               "Pair-wise Hazard Ratios:",
               "Hazard Ratio:")]] <-
    .per_stratum_hrs(x,
                     TRUE,
                     p_precision,
                     hr_precision,
                     p_less_than_cutoff)

  format_df <- function(df)
  {
    if (is.null(df))
    {
      return(NULL)
    }

    df %>%
      as.data.frame() %>%
      remove_rownames() %>%
      column_to_rownames("label") %>%
      capture.output() %>%
      str_c(collapse = "\n")
  }

  df_texts <- map(dfs, format_df)
  is_null <- map_lgl(dfs, is.null)

  c(rbind(names(dfs)[!is_null],
          df_texts[!is_null],
          rep("", sum(!is_null))) # inserts additional new lines
    ) %>%
    str_c(collapse="\n") ->
  str

  if (!valid(label) && .has_strata(x))
  {
    label <- str_c("Analysis by ", x[["by"]])
  }

  if (valid(label))
  {
    str <- str_c(label, ":\n", str)
  }

  if (!.has_strata(x))
  {
    str <- str_c(str, "<only one group in analysis>", sep="\n")
  }
  if (include_end_separator)
  {
    str <- str_c(str, "\n---\n")
  }
  else
  {
    str <- str_c(str, "\n")
  }

  str
}

#' Print the essentials of a SurvivalAnalysisUnivariateResult
#'
#' @inheritParams format.SurvivalAnalysisUnivariateResult
#' @return The formatted string, invisibly.
#' @export
print.SurvivalAnalysisUnivariateResult <- function(x,
                                                   ...,
                                                   label = NULL,
                                                   p_precision = 3,
                                                   hr_precision = 2,
                                                   time_precision = 1,
                                                   include_end_separator = FALSE,
                                                   timespan_unit = c("days", "months", "years"))
{
  str <- format.SurvivalAnalysisUnivariateResult(x,
                                                 ...,
                                                 label=label,
                                                 p_precision = p_precision,
                                                 hr_precision = hr_precision,
                                                 time_precision = time_precision,
                                                 include_end_separator = include_end_separator,
                                                 timespan_unit = timespan_unit)
  cat(str)
  cat("\n")
  invisible(x)
}

#' Print the essentials of a SurvivalAnalysisUnivariateResult.
#'
#' Write complete textual information for one or multiple survival
#' analysis results in a text file.
#'
#' As write_survival takes potentially multiple objects, it cannot
#' return its input in a cleanly defined way.
#' You can still elegantly combine \code{write_survival} in a pipe followed by
#' \code{\link{kaplan_meier_plot}} or \code{\link{kaplan_meier_grid}}
#' for a single input object if you apply the
#' tee pipe operator \code{\%T>\%} in front of \code{write_survival}.
#'
#' @param ... Results generated by \code{\link{analyse_survival}},
#'  or \code{\link{analyse_multivariate}}, or lists of such objects
#' @param file A connection, or a character string naming the file to print to.
#' (see \code{\link{cat}})
#' @param label A label describing the result,
#'    or a vector of the same size as results in ... (will then be mapped 1:1)
#' @param p_precision,hr_precision,time_precision Precision with which to print floating point values
#' @param include_end_separator Boolean:
#'    Append "\\n---\\n" as separator?
#'    Comes handy if printing multiple results following each other
#' @param timespan_unit Unit for time spans: "days", "months" or "years"
#'
#' @return None (invisible NULL).
#'
#' @export
write_survival <- function(...,
                           file,
                           label = NULL,
                           p_precision = 3,
                           hr_precision = 2,
                           time_precision = 1,
                           include_end_separator = FALSE,
                           timespan_unit = c("days", "months", "years"))
{
  # take dots and splice bare lists
  results <- flatten_if(list2(...), is_bare_list)
  timespan_unit <- match.arg(timespan_unit)

  if (is.null(label))
  {
    label <- list(NULL) # need to wrap for pmap
  }

  pmap(list(results,
            label=label),
       format,
       p_precision = p_precision,
       hr_precision = hr_precision,
       time_precision = time_precision,
       include_end_separator = include_end_separator,
       timespan_unit = timespan_unit
  ) %>%
    flatten_chr() %>%
    str_c(collapse = "") %>%
    cat(file = file)
}

# Takes the result of survival_analysis and formats
# the essential information into a multi-line string, which is returned invisibly.
#' Convenience formatting and printing of result
#'
#' Takes the given result, formats and prints it
#'
#' @param result The result generated by \code{\link{analyse_survival}}
#' @inheritParams format.SurvivalAnalysisUnivariateResult
#' @param label Optional label to include
#' @param print Print string to console
#'
#' @return The formatted string, invisibly. Ready for output with cat or saving to a file.
#' @export
survival_essentials <- function(result,
                                label = NULL,
                                p_precision = 3,
                                hr_precision = 2,
                                time_precision = 1,
                                include_end_separator = TRUE,
                                timespan_unit = "days",
                                print = TRUE)
{
  if (invalid(label))
  {
    label <- quo_name(enquo(result))
  }

  str <- format.SurvivalAnalysisUnivariateResult(result,
                                                 label,
                                                 p_precision,
                                                 hr_precision,
                                                 time_precision,
                                                 include_end_separator,
                                                 timespan_unit)
  if (print)
    cat(str)

  invisible(str)
}

#' Compute survival rates by KM estimate for given time points for an
#' univariate survival analysis
#'
#' @param result The result generated by \code{\link{analyse_survival}}
#' @param time_points Time points to compute survival rate at
#' @param percentage_decimal_places,time_precision Precision with which to print floating point values in their label form
#' @param timespan_unit Unit for time spans: "days", "months" or "years".
#'
#' @return A data frame with time, number at risk, number with event,
#' survival rate with CI, and time and rate formatted for printing
#' @export
survival_rates <- function(result,
                           time_points,
                           percentage_decimal_places = 1,
                           time_precision = 0,
                           timespan_unit = c("days", "months", "years"))
{
  if (is.null(result))
  {
    warning("survival_rates called with NULL object. Returning empty tibble.")
    return(tibble())
  }
  if (!inherits(result, "SurvivalAnalysisUnivariateResult"))
  {
    stop("survival_rates called with inappropriate object, type ",
         type_of(result), " class ", class(result))
  }

  timespan_unit <- match.arg(timespan_unit)

  .survival_rate(result,
                 time_points,
                 percentage_decimal_places,
                 time_precision,
                 timespan_unit)
}

#' Write survival rates for one or multiple survival
#' analysis results in a CSV file.
#'
#' As write_survival takes potentially multiple objects, it cannot
#' return its input in a cleanly defined way.
#' You can still elegantly combine \code{write_survival} in a pipe followed by
#' \code{\link{kaplan_meier_plot}} or \code{\link{kaplan_meier_grid}}
#' for a single input object if you apply the
#' tee pipe operator \code{\%T>\%} in front of \code{write_survival}.
#'
#' @param ... Results generated by \code{\link{analyse_survival}},
#' or lists of such objects
#' @param file A connection, or a character string naming the file to print to.
#' (see \code{\link{cat}})
#' @param time_points Time points to compute survival rate at
#' @param label A label describing the result,
#'    or a vector of the same size as results in ... (will then be mapped 1:1).
#'    Recommended to distinguish result lines from multiple results in ...
#' @param writer A writer function such as write.csv
#' @param writer_args Parameters to pass to the writer function
#' @param percentage_decimal_places,time_precision Precision with which to print floating point values in their label form
#' @param timespan_unit Unit for time spans: "days", "months" or "years"
#'
#' @return None (invisible NULL).
#'
#' @export
write_survival_rates <- function(...,
                                 file,
                                 time_points,
                                 label = NULL,
                                 writer = write.csv,
                                 writer_args = list(),
                                 percentage_decimal_places = 1,
                                 time_precision = 0,
                                 timespan_unit = c("days", "months", "years"))
{
  # take dots and splice bare lists
  results <- flatten_if(list2(...), is_bare_list)
  timespan_unit <- match.arg(timespan_unit)

  if (is.null(label))
  {
    label <- list(NULL) # need to wrap for pmap
  }

  rates_with_labels <- function(result, .label)
  {
    result %>%
      survival_rates(time_points = time_points,
                     percentage_decimal_places = percentage_decimal_places,
                     time_precision = time_precision,
                     timespan_unit = timespan_unit) %>%
      mutate(label=.label, .before=1)
  }

  pmap(list(results,
            label),
       rates_with_labels
  ) %>%
    list_rbind() %>%
    {inject(writer(., file=file, !!!writer_args))}
}
