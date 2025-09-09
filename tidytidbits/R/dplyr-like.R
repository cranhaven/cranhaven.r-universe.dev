#' Lump rows of a tibble
#'
#' A verb for a dplyr pipeline:
#' In the given data frame, take the .level column as a set of levels and the .count column
#' as corresponding counts. Return a data frame where the rows are lumped according to levels/counts
#' using the parameters n, prop, other_level, ties.method like for \code{\link{lump}()}.
#' The resulting row for other_level has \code{level=other level}, \code{count=sum(count of all lumped rows)}.
#' For the remaining columns, either a default concatenation is used, or you can provide
#' custom summarising statements via the summarising_statements parameter.
#' Provide a list named by the column you want to summarize, giving statements wrapped in quo(),
#' using syntax as you would for a call to summarise().
#'
#' @param .df A data frame
#' @param .level Column name (symbolic) containing a set of levels
#' @param .count Column name (symbolic) containing counts of the levels
#' @param summarising_statements The "lumped" rows need to have all their columns summarised into one row.
#'    This parameter is a vars() list of arguments as if used in a call to \code{\link[dplyr]{summarise}()},
#'    name is column name, value is language.
#'    If not provided for a column, a default summary will be used
#'    which takes the sum if numeric, concatenates text, or uses any() if logical.
#' @param remaining_levels Levels that should explicitly not be lumped
#' @inheritParams lump
#'
#' @seealso \code{\link{lump}}
#'
#' @return The lumped data frame
#' @export
lump_rows <- function(.df, .level, .count, summarising_statements = quos(),
                      n, prop, remaining_levels, other_level = "Other",
                      ties.method = c("min", "average", "first", "last", "random", "max"))
{
  default_summarise <- function(v)
  {
    if (is_list(v))
      list(unique(unlist(v, recursive = F)))
    else if (is.factor(v))
    {
      present_levels <- unique(v)
      if (length(present_levels) == 1)
        present_levels
      else
        str_c(present_levels, collapse = ", ")
    }
    else if (is_double(v) || is_integer(v))
      sum(v, na.rm = T)
    else if (is_character(v))
    {
      if (length(unique(v)) == 1)
        v[1]
      else
        str_c(str_replace_na(v), collapse = ", ")
    }
    else if (is_logical(v))
      any(v)
    else
      NA
  }
  .level <- enquo(.level)
  .count <- enquo(.count)
  with_other <- !is.null(other_level) && !is.na(other_level)

  original_level <- .df %>% pull(!!.level)
  original_level_chr <- as.character(original_level)
  .df %<>% mutate(!!quo_name(.level) := as.character(!!.level))

  need_default_summarise <- tidyselect::vars_select(colnames(.df),
                                                    -one_of(group_vars(.df)),
                                                    -!!.level, -!!.count,
                                                    -one_of(names(summarising_statements)))
  default_summarising_statements <- list()
  for (field_need_default in need_default_summarise)
    default_summarising_statements[[field_need_default]] <- quo(default_summarise(!!sym(field_need_default)))

  # generate lumping dictionary: level -> level if remaining, NA if lumped
  if (!missing(remaining_levels))
  {
    lumping_dict <- .df %>% pull(!!.level)
    lumping_dict <- set_names(if_else(lumping_dict %in% remaining_levels, lumping_dict, na_chr), lumping_dict)
  }
  else
  {
    lumping_dict <- lump(.df %>% pull(!!.level),
                         .df %>% pull(!!.count),
                         n = n,
                         prop = prop,
                         other_level = NA,
                         ties.method = ties.method)
  }

  # if no rows are lumped, just return
  if (!sum(is.na(lumping_dict)) > 1)
    return(.df)


  if (with_other)
  {
    .df %>%
      filter(!(!!.level) %in% lumping_dict) %>%
      summarise(!!quo_name(.level) := other_level,
                !!quo_name(.count) := sum(!!.count),
                !!!summarising_statements,
                !!!default_summarising_statements) ->
      other_row
  }

  .df %<>% filter((!!.level) %in% lumping_dict)

  if (with_other)
  {
    .df %<>% bind_rows(other_row)
  }

  if (is.factor(original_level))
  {
    new_level <- .df %>% pull(!!.level)
    new_level_levels <- c(original_level_chr[original_level_chr %in% unique(new_level)], other_level)
    #.df[[quo_name(.level)]] <- factor(new_level, levels = unique(new_level_levels))
  }
  .df
}

#' Format numeric columns for display
#'
#' Combines \code{\link[dplyr]{mutate_at}()} and \code{\link{as_formatted_number}()}
#'
#' @param .tbl A data frame
#' @param .vars A vars() list of symbolic columns
#' @inheritParams as_formatted_number
#'
#' @return Value of mutate_at
#' @export
#'
#' @seealso \code{\link{format_p_values_at}}
#'
#' @examples
#' library(tibble)
#' library(magrittr)
#' library(dplyr)
#' tibble(a=c(0.1, 0.238546)) %>%
#'     format_numbers_at(vars(a))
format_numbers_at <- function(.tbl, .vars,
                              decimal_places = 1,
                              remove_trailing_zeroes = T)
{
  mutate_at(.tbl, .vars,
            ~as_formatted_number(.,
                                 decimal_places = decimal_places,
                                 remove_trailing_zeroes = remove_trailing_zeroes)
  )
}

#' Format numeric columns for display
#'
#' Combines \code{\link[dplyr]{mutate_at}()} and \code{\link{as_formatted_p_value}()}
#'
#' @param .tbl A data frame
#' @param .vars A vars() list of symbolic columns
#' @inheritParams as_formatted_p_value
#'
#' @return Value of mutate_at
#' @export
#'
#' @seealso \code{\link{format_numbers_at}}
#'
#' @examples
#' library(tibble)
#' library(magrittr)
#' library(dplyr)
#' tibble(p=c(0.05, 0.0001)) %>%
#'     format_numbers_at(vars(p))
format_p_values_at <- function(.tbl, .vars,
                               decimal_places = 3,
                               prefix = "p",
                               less_than_cutoff = 0.001,
                               remove_trailing_zeroes = T,
                               alpha = 0.05,
                               ns_replacement = NULL)
{
  mutate_at(.tbl, .vars, ~as_formatted_p_value(.,
                                               decimal_places = decimal_places,
                                               prefix = prefix,
                                               less_than_cutoff = less_than_cutoff,
                                               remove_trailing_zeroes = remove_trailing_zeroes,
                                               alpha = alpha,
                                               ns_replacement = ns_replacement)
  )
}


#' Count according to grouping
#'
#' Similar to \code{\link[dplyr]{count}()}, but also adds the relative proportion and
#' a percent-formatted string of the relative proportion,
#' and allows to specify the column names.
#'
#' @param .tbl A data frame
#' @param ... Columns / expressions by which to group / which shall be used for counting.
#' @param column_names vector if size 1 to 3, giving the names of (in order if unnamed, or named with n, rel, percent)
#'     the column containing the count, the relative proportion, and the latter formatted as a percent label.
#'     If a name is not contained, it will not be added (requires named vector).
#' @param percentage_label_decimal_places Decimal precision of the percent label
#' @param add_grouping Shall a pre-existing grouping be preserved for counting (adding the newly specified grouping)?
#'     Default is yes, which differs from group_by.
#' @param na.rm Shall NA values be removed prior to counting?
#'
#' @return The counted data frame
#' @export
#'
#' @examples
#' library(magrittr)
#' if (requireNamespace("survival", quietly = TRUE))
#' {
#'    survival::aml %>%
#'    count_by(x)
#' }
count_by <- function(.tbl,
                     ...,
                     column_names = c("n", "rel", "percent"),
                     percentage_label_decimal_places = 1,
                     add_grouping = T,
                     na.rm = F)
{
  stopifnot(has_length(column_names))
  grouping <- quos(...)
  previous_grouping <- groups(.tbl)

  if (is_named(column_names))
  {
    renames <- c()
    removals <- c()
    if ("n" %in% names(column_names))
      renames[[column_names[["n"]] ]] <- "n"
    else
      removals <- c(removals, "n")

    if ("rel" %in% names(column_names))
      renames[[column_names[["rel"]] ]] <- ".rel.count.by."
    else
      removals <- c(removals, ".rel.count.by.")

    if ("percent" %in% names(column_names))
      renames[[column_names[["percent"]] ]] <- ".percent.count.by."
    else
      removals <- c(removals, ".percent.count.by.")
  }
  else
  {
    renames <- set_names( c("n", ".rel.count.by.", ".percent.count.by."),
                          column_names)
    renames <- renames[!is.na(names(renames))]
    removals <- renames[is.na(names(renames))]
  }

  if (na.rm)
  {
    grouping_lang <- keep(grouping, ~quo_is_call(.x))
    grouping_symbol <- keep(grouping, ~quo_is_symbol(.x))
    complete <- rep(T, nrow(.tbl))
    if (has_length(grouping_lang))
      complete <- complete & complete.cases(.tbl %>% transmute(!!!grouping_lang))
    if (has_length(grouping_symbol))
      complete <- complete & complete.cases(.tbl %>% select(!!!grouping_symbol))

    .tbl <- .tbl[complete, ]
  }

  # we use tally, which uses "n" as hard-coded column name
  if ("n" %in% colnames(.tbl))
  {
    .tbl %<>% select(-n)
  }

  .tbl %<>%
    group_by(!!!grouping, .add = add_grouping) %>%
    tally() %>%
    mutate(.rel.count.by.=n/sum(n),
           .percent.count.by. = as_percentage_label(.data$.rel.count.by.,
                                                    percentage_label_decimal_places)
           ) %>%
    rename(!!!renames) %>%
    select(-one_of(removals))

  if (add_grouping)
  {
    .tbl %<>% group_by(!!!previous_grouping, .add=F)
  }
  .tbl
}

#' Count by multiple variables
#'
#' @param .tbl A data frame
#' @param .vars A list of variables (created using vars()) for which \code{\link{count_by}} is to be called
#' @param .grouping Additional grouping to apply prior to counting
#' @param label_style Character vector containing one of "wide" and "long" or both.\itemize{
#'     \item "wide": Include labels in wide format, i.e., for each variable one column named as variable
#'     and giving the label for the corresponding count, but NA for all rows from different variables
#'     \item "long": Include two meta columns, one giving the variable that is counted (value from .vars),
#'     the second giving the label (which value/category of the variable is counted?).
#'     }
#' @param na_label If na.rm=F, label to use for counting NA values
#' @param long_label_column_names Character vector of size 2: If label_style contains "long",
#'    the names for the additional meta columns for variable and category
#' @inheritParams count_by
#'
#' @return A data frame concatenated from individual count_by results, with labels as per label_style.
#' @export
#'
#' @examples
#' library(magrittr)
#' library(datasets)
#' library(dplyr)
#' mtcars %>% count_at(vars(gear, cyl))
count_at <- function(.tbl,
                     .vars,
                     .grouping = vars(),
                     label_style = "long",
                     long_label_column_names = c("variable", "category"),
                     column_names = c("n", "rel", "percent"),
                     na_label = "missing",
                     percentage_label_decimal_places = 1,
                     add_grouping = T,
                     na.rm = F)
{
  label_style <- match.arg(label_style, c("wide", "long"), several.ok = T)
  labels <- list()
  if ("long" %in% label_style)
    labels <- append(labels, long_label_column_names)
  if ("wide" %in% label_style)
    labels <- append(labels, .vars)

  map_dfr(.vars, function(.var)
  {
    var_name <- quo_name(.var)
    na_replacement_list <- set_names(list(na_label), var_name)
    .tbl %>%
      mutate(!!var_name := as.character(!!.var)) %>%
      count_by(!!!.grouping, !!.var,
               column_names = column_names,
               percentage_label_decimal_places = percentage_label_decimal_places,
               add_grouping = add_grouping,
               na.rm = na.rm) %>%
      replace_na(na_replacement_list)
  }) %>%
    mutate(!!long_label_column_names[[1]] := map_chr(.vars, quo_name)[first_which_non_na_at(., !!!.vars)],
           !!long_label_column_names[[2]] := first_non_nas_at(., !!!.vars)) %>%
    select(!!!labels, !!!.grouping, !!!column_names)
}


#' Add results of prop.test to data frame
#'
#' Adds prop.test results as columns to data frame based on data in columns
#' For use with a tibble in a pipe:
#' Using one-group prop.test, adds confidence intervals (with given conf.level)
#' for the proportion of x positive results in n trials,
#' and the p value that the proportion is equal to p (default: 0.5)
#' (to add the estimated proportion itself, use \code{\link{count_by}})
#'
#' @param .df A data frame
#' @param x The column/vector with the number of positive results
#' @param n The column/vector/constant with the number of trials
#' @param p Assumed proportion: Will add a p-value that the proportion is equal to p (default: 0.5)
#' @param CI_lower_name,CI_upper_name,p_name Column names of the added columns
#' @param alternative,conf.level,correct As for \code{\link{prop.test}}
#'
#' @return Data frame with columns added
#' @export
#'
#' @seealso \code{\link{count_by}()}
#'
#' @examples
#' library(magrittr)
#' if (requireNamespace("survival", quietly = TRUE))
#' {
#'    survival::aml %>%
#'    count_by(x) %>%
#'    add_prop_test(n, sum(n), rel)
#' }
add_prop_test <- function(.df, x, n, p = NULL,
                          CI_lower_name = "CI_lower",
                          CI_upper_name = "CI_upper",
                          p_name = "p",
                          alternative = c("two.sided", "less", "greater"),
                          conf.level = 0.95,
                          correct = TRUE)
{
  x <- enquo(x)
  n <- enquo(n)
  p <- enquo(p)
  if (quo_is_null(p))
    p <- 0.5

  mutate(.df,
         .prop_test=pmap(list(!!x,
                              !!n,
                              !!p),
                         function(x,n,p)
                         {
                           if (p == 0)
                             p <- .Machine$double.eps
                           prop.test(x, n, p,
                                     alternative = alternative,
                                     conf.level = conf.level,
                                     correct = correct)
                         }),
         !!CI_lower_name := map_dbl(.data$.prop_test, ~.$conf.int[[1]]),
         !!CI_upper_name := map_dbl(.data$.prop_test, ~.$conf.int[[2]]),
         !!p_name := map_dbl(.data$.prop_test, "p.value")
  ) %>%
    select(-.prop_test)
}

#' Create data frame formed like a contingency-table
#'
#' Counts by the specified two variables and the pivots the
#' count data frame wider to a two-dimensional contingency table.
#' Please note that the resulting data frame is suitable for convenient
#' output or use with functions that work on matrix-like data,
#' but does not fulfill the tidy data criteria.
#'
#' @param .tbl A data frame
#' @param var1 First column to count by
#' @param var2 Second column to count by
#' @param na.rm Shall NA values be removed prior to counting?
#' @param add_margins Add row- and column wise margins as extra column and row
#'
#' @return A data frame
#' @export
#'
#' @examples
#' library(magrittr)
#' if (requireNamespace("datasets", quietly = TRUE))
#' {
#'    mtcars %>% contingency_table_by(cyl, gear)
#' }
contingency_table_by <- function(.tbl,
                                 var1,
                                 var2,
                                 na.rm = F,
                                 add_margins = F)
{
  var1 <- enquo(var1)
  var2 <- enquo(var2)

  .tbl %>%
    count_by(!!var1, !!var2,
             na.rm = na.rm,
             column_names = c(n="n")) %>%
    pivot_wider(id_cols = 1,
                names_from = 2,
                values_from = 3,
                values_fill = 0,
                names_prefix = str_c(as_label(var2), ":")
    ) ->
    table

  if (add_margins)
  {
    table %>%
      rowwise() %>%
      mutate(sum = sum(c_across(where(is.numeric)))) %>%
      ungroup() %>%
      bind_rows(summarise(., across(where(is.numeric), sum)) %>%
                  mutate("{{var1}}" := "sum"))
  }
  else
  {
    table
  }
}

#' Convert contingency table to classical R matrix
#'
#' Converts the result of \code{\link{contingency_table_by}}
#' to a classical matrix
#'
#' @param table_frame Result of \code{\link{contingency_table_by}}
#'
#' @return A matrix
#' @export
contingency_table_as_matrix <- function(table_frame)
{
  table_frame %>%
    select(where(is.numeric)) %>%
    as.matrix()
}

default_categorical_test_choice <- function(matrix)
{
  if (min(matrix) <= 5)
    c(fisher=fisher.test)
  else
    c(chisq=chisq.test)
}

#' Categorical test in a pipe
#'
#' Performs classical categorical tests on two columns of a
#' data frame.
#' Per default, will perform \code{\link{chisq.test}}
#' or \code{\link{fisher.test}} on the
#' contingency table created by var1 and var2.
#'
#' Returns a one-line data frame as result and thus plays nicely
#' with for example \code{\link[purrr]{map_dfr}}.
#'
#' @inheritParams contingency_table_by
#' @param test_function_generator A function receiving the matrix to test and
#' returning a named vector with the test function to use. The default uses
#' fisher.test if one count is 5 or lower, otherwise chisq.test.
#' Test functions must return a value with at least one component named "p.value".
#' @param ... Passed on to the test function
#'
#' @return A one-row data frame with the columns:\itemize{
#'     \item "var1,var2": The tested variables
#'     \item "test": Label of the test function (default: fisher or chisq)
#'     \item "p-value": P value
#'     \item "result": List column with full result object (default: htest)
#'     \item "contingency_table": List column with contingency table data frame
#'     as return by \code{\link{contingency_table_by}}
#'     }
#' @export
#'
#' @examples
#' library(magrittr)
#' if (requireNamespace("datasets", quietly = TRUE))
#' {
#'    mtcars %>% categorical_test_by(cyl >= 6, gear)
#' }
categorical_test_by <- function(.tbl, var1, var2, na.rm = T,
                                test_function_generator = NULL,
                                ...)
{
  var1 <- enquo(var1)
  var2 <- enquo(var2)

  table_frame <- contingency_table_by(.tbl, !!var1, !!var2, na.rm = na.rm)
  matrix <- contingency_table_as_matrix(table_frame)
  if (is.null(test_function_generator))
  {
    test_function_generator = default_categorical_test_choice
  }
  test <- test_function_generator(matrix)
  result <- test[[1]](matrix, ...)

  tibble(var1 = as_label(var1),
         var2 = as_label(var2),
         test = names(test)[[1]],
         `p-value` = result$p.value,
         result = list(result),
         contingency_table = list(table_frame)
  )
}


