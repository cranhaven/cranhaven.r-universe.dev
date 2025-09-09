
# An implementation of order() which retains the order of the given values
#' Ordering function: identity order
#'
#' This can be used in a place where a function with a signature like \code{\link{order}} is required.
#' It simply retains the original order.
#'
#' @param x a vector
#' @param ... Effectively ignored
#' @export
#'
#' @return An integer vector
identity_order <- function(x, ...)
{
  seq(1, length(x))
}

#' Forest plots for survival analysis.
#'
#' Creates a forest plot from SurvivalAnalysisResult objects.
#' Both univariate (\code{\link{analyse_survival}}) results, typically with use_one_hot=TRUE,
#' and multivariate (\code{\link{analyse_multivariate}}) results are acceptable.
#'
#' The plot has a left column containing the labels (covariate name, levels for categorical variables, optionally subgroup size),
#' the actual line plot in the middle column, and a right column to display the hazard ratios and their confidence intervals.
#' A rich set of parameters allows full customizability to create publication-ready plots.
#'
#' @param ... The SurvivalAnalysisResult objects.
#'     You can also pass one list of such objects, or use explicit splicing (!!! operator).
#'     If not \code{use_one_hot}, also a list of coxph objects, or a mix is acceptable.
#' @param use_one_hot If not use_one_hot (default), will take univariate or multivariate results and plot hazard ratios
#'     against the reference level (as provided to the \code{\link{analyse_survival}} or \code{\link{analyse_multivariate}}
#'     function, or, per default, the first factor level), resulting in k-1 values for k levels.
#'     If use_one_hot == TRUE, will only accept univariate results from \code{\link{analyse_survival}} and plot HRs of one factor
#'     level vs. remaining cohort, resulting in k values for k levels.
#' @param factor_labeller,endpoint_labeller
#'     Either\itemize{
#'     \item A function which returns labels for the input:
#'         First argument, a vector of either (factor.ids) or (endpoints), resp.
#'         If the function takes ... or two arguments, as second argument a data frame with (at least)
#'         the columns survivalResult, endpoint, factor.id, factor.name, factor.value, HR, Lower_CI, Upper_CI, p, n,
#'         where survivalResult is the corresponding result object passed to forest_plot;
#'         Note the function must be vectorized, if you have a non-vectorized function taking single arguments,
#'         you may want to have a look at purrr::map_chr or purrr::pmap_chr.
#'     \item a dictionaryish list, looks up by (endpoints) or (factor.ids).
#'         The factor.id value: For continous factors, the factor name (column name in data frame);
#'         For categorical factors, factor name, factor_id_sep, and the factor level value.
#'         (note: If use_one_hot = FALSE, the HR is factor level value vs. cox reference given to survival_analysis;
#'          if use_one_hot = TRUE, the HR is the factor level value vs. remaining population)
#'      }
#' @param orderer  A function which returns an integer ordering vector for the input:
#'     \itemize{
#'     \item if the supplied function takes exactly one argument, a data frame with (at least)
#'     the columns survivalResult, endpoint, factor.id, factor.name, factor.value, HR, Lower_CI, Upper_CI, p, n, subgroup_n
#'     where survivalResult is the corresponding result object passed to forest_plot;
#'     \item or, if the function takes more than one argument, or its arguments include ..., the nine vectors
#'     (endpoint, factor.name, factor.value, HR, Lower_CI, Upper_CI, p, n, subgroup_n):
#'     a vector of endpoints (as given to Surv(endpoint, ...) in coxph),
#'     a vector of factors (as given to the right hand side of the coxph formula), and
#'     numeric vectors of the HR, lower CI, upper CI, p-value
#'     \item You can create a function from ordered vectors via orderer_function_from_sorted_vectors,
#'     or call order() with one or more of these vectors.
#'     \item Alternatively, you can provide a quosure of code, or a right-hand side formula;
#'     it will be executed such that the above nine vectors are available as symbols.
#'     }
#'     Example:
#'     \itemize{
#'         \item \code{orderer = quo(order(endpoint, HR))}
#'         \item   equivalent to \code{orderer = ~order(endpoint, HR)}
#'         \item   equivalent to \code{orderer = function(df) df \%$\% order(endpoint, HR)}
#'         \item   equivalent to \code{orderer = function(df) { order(df$endpoint, df$HR) }}
#'         \item   equivalent to \code{orderer = function(endpoint, factor.name, factor.value, HR, ...) order(endpoint, HR)}
#'      }
#' @param categorizer A function which returns one logical value if a breaking line should be
#'     inserted _above_ the input: Same semantics as for orderer.
#'     !Please note!: The order of the data is not yet ordered as per your orderer!
#'     If you do calculations depending on order, first order with your own orderer function.
#'     A proper implementation is easy using \code{\link[tidytidbits]{sequential_duplicates}},
#'     for example \code{categorizer=~!sequential_duplicates(endpoint, ordering = order(endpoint, HR))}
#' @param relative_widths relation of the width of the plots, labels, plot, values. Default is 1:1:1.
#' @param ggtheme ggplot2 theme to use
#' @param labels_displayed Combination of "endpoint", "factor", "n", determining what is shown on the left-hand table
#'     and in which order.
#' @param label_headers  Named vector with name=<allowed values of labels_displayed>, value=<your heading>.
#' @param values_displayed Combination of "HR", "CI", "p", "subgroup_n", determining what is shown on the right-hand table
#'     and in which order. Note: subgroup_n is only applicable if oneHot=TRUE.
#' @param value_headers  Named vector with name=<allowed values of values_displayed>, value=<your heading>.
#' @param HRsprintfFormat,psprintfFormat sprintf() format strings for hazard ratio and p value
#' @param p_lessthan_cutoff The lower limit below which p value will be displayed as "less than".
#'     If p_lessthan_cutoff == 0.001, the a p value of 0.002 will be displayed as is, while 0.0005 will become "p < 0.001".
#' @param log_scale Plot on log scale, which is quite common and gives symmetric length for the CI bars.
#'     Note that HRs of 0 (did not converge) will not be plotted in this case.
#' @param HR_x_breaks Breaks of the x scale for plotting HR and CI
#' @param HR_x_limits Limits of the x scale for plotting HR and CI.
#'     Default (HR_x_lim = NULL) depends on log_scale and existing limits.
#'     Pass NA to use the existing minimum and maximum values without interference.
#'     Pass a vector of size 2 to specify (min, max) manually
#' @param factor_id_sep Allows you to customize the separator of the factor id, the documentation of factor_labeller.
#' @param na_rm Only used in the multivariate case (use_one_hot = FALSE). Should null coefficients (NA/0/Inf) be removed?
#' @param title,title_relative_height,title_label_args A title on top of the plot, taking a fraction of title_relative_height of the returned plot.
#'     The title is drawn using \code{\link[cowplot]{draw_label}}; you can specify any arguments to this function by giving title_label_args
#'     Per default, font attributes are taken from the "title" entry from the given ggtheme, and the label
#'     is drawn centered as per \code{\link[cowplot]{draw_label}} defaults.
#' @param base_papersize numeric vector of length 2, c(width, height), unit inches.
#'     forest_plot will store a suggested "papersize" attribute in the return value, computed from
#'     base_papersize and the number of entries in the plot (in particular, the height will be adjusted)
#'     The attribute is read by save_pdf.
#'     It will also store a "forestplot_entries" attribute which you can use for your own calculations.
#'
#' @return A ggplot2 plot object
#' @seealso \code{\link{forest_plot_grid}}
#' @export
#'
#' @examples
#' library(magrittr)
#' library(dplyr)
#' survival::colon %>%
#'    analyse_multivariate(vars(time, status),
#'                         vars(rx, sex, age, obstruct, perfor, nodes, differ, extent)) %>%
#'    forest_plot()
forest_plot <- function(...,
                        use_one_hot = FALSE,
                        factor_labeller = identity,
                        endpoint_labeller = identity,
                        orderer = identity_order,
                        categorizer = NULL,
                        relative_widths = c(1,1,1),
                        ggtheme = theme_bw(),
                        labels_displayed = c("endpoint", "factor"),
                        label_headers = c("endpoint"="Endpoint", "factor"="Subgroup", "n"="n"),
                        values_displayed = c("HR", "CI", "p"),
                        value_headers = c("HR"="HR", "CI"="CI", "p"="p", "n"="n", "subgroup_n"="n"),
                        HRsprintfFormat = "%.2f",
                        psprintfFormat = "%.3f",
                        p_lessthan_cutoff = 0.001,
                        log_scale = TRUE,
                        HR_x_breaks = seq(0,10),
                        HR_x_limits = NULL,
                        factor_id_sep = ":",
                        na_rm = TRUE,
                        title = NULL,
                        title_relative_height = 0.1,
                        title_label_args = list(),
                        base_papersize = dinA(4))
{
  # 1) Create data frame
  cox_results_df(..., use_one_hot = use_one_hot, factor_id_sep = factor_id_sep) %>%
    # 2) Create plot
    forest_plot.df(factor_labeller,
                   endpoint_labeller,
                   orderer,
                   categorizer,
                   relative_widths,
                   ggtheme,
                   labels_displayed,
                   label_headers,
                   values_displayed,
                   value_headers,
                   HRsprintfFormat,
                   psprintfFormat,
                   p_lessthan_cutoff,
                   log_scale,
                   HR_x_breaks,
                   HR_x_limits,
                   factor_id_sep,
                   na_rm,
                   title,
                   title_relative_height,
                   title_label_args,
                   base_papersize)
}

# Utility function for forest_plot: Converts SurvivalAnalysisResult objects,
# or one list of such objects, or a combination of single objects and lists which are spliced (!!!),
# or coxph objects, or a mix thereof,
# to a data frame containing the columns:
# survivalResult, endpoint, factor.id, factor.name, factor.value, HR, Lower_CI, Upper_CI, p, n, subgroup_n
cox_results_df <- function(..., use_one_hot = FALSE, factor_id_sep=":")
{
  args <- .survivalResultArguments(...)

  coxph_obj <- function(coxph, survivalResult = NULL, factorLevelOneHot = NULL)
  {
    if (invalid(coxph) || inherits(coxph, "coxph.null"))
      return(NULL)
    stopifnot(inherits(coxph, "coxph"))
    obj <- list()
    obj$coxph <- coxph
    obj$coxphSummary <- summary(obj$coxph)
    # for the normal approach, it's one reference level vs. rest, so n is "full" n
    obj$n <- obj$coxphSummary$n
    if (!invalid(survivalResult))
    {
      obj$unmangleDict <- survivalResult[["colname_unmangle_dict"]]
      obj$survivalResult <- survivalResult
      obj$factorLevelOneHot <- factorLevelOneHot
      # only availabe with univariate results
      obj$factorFrequencies <- survivalResult$factorFrequencies
    }
    return(obj)
  }

  if (use_one_hot)
  {
    # must have univariate results only
    map(args, function(arg) {
      if (!inherits(arg, "SurvivalAnalysisUnivariateResult"))
        stop("Arguments must be SurvivalAnalysisUnivariateResult (from analyse_survival)")
    })

    # args: list of results, of which coxph_onehot: list of coxph
    # So for multiple categories, we get multiple objs from each arg
    objs <- flatten(map(args,
                        function(arg)
                        {
                          factorLevels <- names(arg[["coxph_onehot"]])
                          map2(arg[["coxph_onehot"]], factorLevels,
                               ~coxph_obj(coxph=.x, survivalResult=arg, factorLevelOneHot=.y))
                        }))
  }
  else
  {
    # args: list of results, of which coxph: one coxph; or list of coxphs
    objs <- map(args, function(arg) {
      if (inherits(arg, "coxph"))
        coxph_obj(arg)
      else if (inherits(arg, "SurvivalAnalysisResult"))
        coxph_obj(arg[["coxph"]], arg)
      else
        stop("Argument is neither a survival_analysis result, not a coxph result")
    }
    )
  }

  null_cox <- map_lgl(objs, is.null)
  if (any(null_cox))
  {
    warning("Some arguments have no cox model or a null model (index position ",
            str_c(which(null_cox), collapse = ", "), ")")
    objs <- objs[!null_cox]
  }

  adjust_subgroup_n <- function(cox_data_frame, obj)
  {
    # for all multivariate cases
    if (is.null(obj$factorFrequencies))
    {
      na_int
    }
    # special uv case
    if (!is.null(obj$factorLevelOneHot))
    {
      obj$factorFrequencies[[obj$factorLevelOneHot]]
    }
    # general uv case
    lookup_int(obj$factorFrequencies, cox_data_frame$factor.value)
  }

  objs %>%
    # for each summary:
    map(function(obj) {
      # get essentials in data frame
      cox_as_data_frame(obj$coxphSummary, unmangle_dict=obj$unmangleDict, factor_id_sep=factor_id_sep) %>%
        # add an endpoint column
        mutate(n = obj$n,
               subgroup_n = adjust_subgroup_n(., obj),
               endpoint = lookup_chr(obj$unmangleDict, .coxEndpoint(obj$coxphSummary), default=identity),
               survivalResult = list(obj$survivalResult))
    }
    ) %>%
    # concat all in one df
    reduce(rbind)
}

#' @describeIn forest_plot Creates a forest plot from the given data frame
#' @param .df Data frame containing the columns
#'  \code{survivalResult, endpoint, factor.id, factor.name, factor.value, HR, Lower_CI, Upper_CI, p, n, subgroup_n}
#'  giving the information that is to be presented in the forest plot
forest_plot.df <- function(.df,
                           factor_labeller = identity,
                           endpoint_labeller = identity,
                           orderer = identity_order,
                           categorizer = NULL,
                           relative_widths = c(1,1,1),
                           ggtheme = theme_bw(),
                           labels_displayed = c("endpoint", "factor"),
                           label_headers = c("endpoint"="Endpoint", "factor"="Subgroup", "n"="n"),
                           values_displayed = c("HR", "CI", "p"),
                           value_headers = c("HR"="HR", "CI"="CI", "p"="p", "n"="n", "subgroup_n"="n"),
                           HRsprintfFormat = "%.2f",
                           psprintfFormat = "%.3f",
                           p_lessthan_cutoff = 0.001,
                           log_scale = TRUE,
                           HR_x_breaks = seq(0,10),
                           HR_x_limits = NULL,
                           factor_id_sep = ":",
                           na_rm = TRUE,
                           title = NULL,
                           title_relative_height = 0.1,
                           title_label_args = list(),
                           base_papersize = dinA(4))    # filter and order columns
{
  call_labeller <- function(fun, column_symbol, df)
  {
    column_symbol <- enquo(column_symbol)
    v <- pull(df, !!column_symbol)
    if (is_function(fun))
    {
      fun_args <- formals(fun)
      if (length(fun_args)>1 || has_name(fun_args, "..."))
        fun(v, df)
      else
        fun(v)
    }
    else if (is_dictionaryish(fun))
      return(lookup_chr(fun, v, default = identity))
    else
      stop("Labeller is neither a lookup function, nor a dictionaryish list")
  }

  call_orderer <- function(column_vars, df)
  {
    stopifnot(is_function(orderer) || is_quosure(orderer) || is_formula(orderer))
    if (is_function(orderer))
    {
      fun_args <- formals(orderer)
      if (length(fun_args)>1 || has_name(fun_args, "..."))
      {
        for (var in column_vars)
          df %>% pull(!!var) %>% append_object(columns) -> columns
        do.call(orderer, columns)
      }
      else
        orderer(df)
    }
    else if (is_quosure(orderer) || is_formula(orderer))
    {
      eval_tidy(as_quosure(orderer), data = df)
    }
  }

  call_categorizer <- function(column_vars, df)
  {
    if (is_function(categorizer))
    {
      fun_args <- formals(categorizer)
      if (length(fun_args)>1 || has_name(fun_args, "..."))
      {
        for (var in column_vars)
          df %>% pull(!!var) %>% append_object(columns) -> columns
        do.call(categorizer, columns)
      }
      else
        categorizer(df)
    }
    else if (is_quosure(categorizer) || is_formula(categorizer))
    {
      eval_tidy(as_quosure(categorizer), data = df)
    }
    else
    {
      if (invalid(categorizer))
        FALSE
      # support (boolean) vectors
      else if (is_vector(categorizer))
        as.logical(purrr::simplify(categorizer))
      else
        stop("forest_plot: Unsupported value for categorizer: ", categorizer)
    }
  }

  vars_to_pass_to_hooks <- vars(endpoint, factor.name, factor.value, HR, Lower_CI, Upper_CI, p, n, subgroup_n)

  .df %>%
    select(endpoint, factor.id, factor.name, factor.value, HR, Lower_CI, Upper_CI, p, n, subgroup_n) %>%
    # execute na_rm, before creating the ordered_index!
    filter(!(na_rm & (is.na(HR) | near(HR, 0) | is.na(Lower_CI) | is.na(Upper_CI) | is.infinite(Upper_CI)))) %>%
    # do labelling and categorizing (here rather than above to provide full column to methods)
    mutate(endpointLabel = call_labeller(endpoint_labeller, endpoint, .),
           factorLabel = call_labeller(factor_labeller, factor.id, .),
           breakAfter  = call_categorizer(vars_to_pass_to_hooks, .)) %>% mutate(#,
             # determine the order of the plot used by both the plot and the table.
             # It is intuitive to order top to bottom, but we plot bottom to top, so use the reverse.
             # We get an order such that df[ordering,] would be ordered; we store it as an ordered index
             # ordered_index = order(ordering) such that df would be ordered if ordered_index was ordered,
             # or, df[ordering,] == df[order(ordered_index),])
             ordered_index = order(rev(call_orderer(vars_to_pass_to_hooks, .)))
           ) ->
    hrDf

  # find the longest string for each label category, already transformed by gather() to key->value.
  # the first starts a 0, the last value indicates the limit of the axis
  # thus we get relative values, the absolute numbers have no meaning
  space_needed <- function(keys, values)
  {
    unique_keys <- unique(keys)
    spacing <- strwidth(" ", family = ggtheme$text$family, units = "in")
    label_x_pos <- cumsum(c(0, map_dbl(unique_keys,
                                       # for each key
                                       function(unique_key)
                                         # max string width for all values of that key
                                         spacing +
                                         max(map_dbl(values[keys == unique_key],
                                                     # for each value
                                                     function(v)
                                                     {
                                                       # split in lines
                                                       lines <- c(str_split(v, "\\n", simplify = TRUE))
                                                       # max string width of all lines of the value
                                                       max(map_dbl(lines,
                                                                   ~strwidth(., family = ggtheme$text$family, units = "in")))
                                                     })))))
    # make it a dict
    names(label_x_pos) <- c(unique_keys, "xlim")
    return(label_x_pos)
  }

  # As we insert /above/, /add/ 0.5
  line_increment <- 0.5
  hlines <- hrDf %>% filter(breakAfter) %>% transmute(breaks = ordered_index + line_increment) %>% pull(breaks)

  label_selectors = c("endpoint", "factor", "n")
  default_label_headers = c("endpoint"="Endpoint", "factor"="Subgroup", "n"="n")
  label_headers <- list_modify(as_list(default_label_headers), !!!as_list(label_headers))
  if (any(!labels_displayed %in% label_selectors))
    stop("Unknown fields in values_displayed: ",
         str_c(labels_displayed[!labels_displayed %in% label_selectors], collapse = ", "))
  # make a dict value_selector -> value_header
  label_headers <- set_names(lookup_chr(label_headers, label_selectors, default = identity),
                             label_selectors)
  label_string_vars <- vars(endpointLabel, factorLabel, n_string)
  label_string_vars_to_draw <- label_string_vars[match(labels_displayed, label_selectors)]

  # Drop sequential duplicate endpoint labels (left column of table), as is natural.
  # Factor numeric is bottom to top, but we want to replace top to bottom, so reverse again
  # Do not remove a duplicate if a category line is drawn above it.
  remove_sequential_duplicates_unless_breakafter <- function(x, ordered_index, breakAfter)
    ifelse(sequential_duplicates(x, ordering=rev(order(ordered_index)))
           & !breakAfter,
           "", x)

  # Prepare for labels: we use ordered_index as unique y-value
  hrDf %>%
    mutate(n_string  = str_c(n)) %>%
    # we only need these
    select(ordered_index, !!!label_string_vars, breakAfter) %>%
    mutate(endpointLabel = remove_sequential_duplicates_unless_breakafter(endpointLabel, ordered_index, breakAfter),
           n_string = remove_sequential_duplicates_unless_breakafter(n_string, ordered_index, breakAfter)) %>%
    select(-breakAfter) %>%
    # Add title row. bind_rows does not use tidy eval, need to ".$"
    bind_rows( tibble(ordered_index=max(.$ordered_index)+1,
                      endpointLabel = label_headers[["endpoint"]],
                      factorLabel = label_headers[["factor"]],
                      n_string = label_headers[["n"]]) ) %>%
    select(ordered_index, !!!label_string_vars_to_draw) %>%
    # endpointLabel and factorLabel will share x values -> gather in key-value arrangement
    gather(x, labels, !!!label_string_vars_to_draw) ->
    labelsDf

  value_selectors <- c("HR", "CI", "p", "n", "subgroup_n")
  default_value_headers <- c("HR"="HR", "CI"="CI", "p"="p", "n"="n", "subgroup_n"="n")
  value_headers <- list_modify(as_list(default_value_headers), !!!as_list(value_headers))
  if (any(!values_displayed %in% value_selectors))
    stop("Unknown fields in values_displayed: ",
         str_c(values_displayed[!values_displayed %in% value_selectors], collapse = ", "))
  # make a dict value_selector -> value_header
  value_headers <- set_names(lookup_chr(value_headers, value_selectors, default = identity),
                             value_selectors)
  value_string_vars <- vars(HR_string, CI_string, p_string, n_string, subgroup_n_string)
  value_string_vars_to_draw <- value_string_vars[match(values_displayed, value_selectors)]
  # Prepare for values, in analogy to labels
  hrDf %>%
    mutate(CI_string = paste0("(", sprintf(HRsprintfFormat, Lower_CI),
                              "\u2013", sprintf(HRsprintfFormat, Upper_CI), ")"),
           HR_string = sprintf(HRsprintfFormat, HR),
           p_string  = survivalFormatPValue(p, with_prefix = FALSE,
                                            p.lessthan.cutoff = p_lessthan_cutoff,
                                            psprintfFormat = psprintfFormat,
                                            pad_for_less_than=TRUE),
           n_string  = str_c(n),
           subgroup_n_string = str_c(subgroup_n)
    ) %>%
    # we only need these
    select(ordered_index, !!!value_string_vars) %>%
    # Add header row. bind_rows does not use tidy eval, need to ".$"
    bind_rows( tibble(ordered_index=max(.$ordered_index)+1,
                      HR_string = lookup_chr(value_headers, "HR"),
                      CI_string = lookup_chr(value_headers, "CI"),
                      p_string = lookup_chr(value_headers, "p"),
                      n_string = lookup_chr(value_headers, "n"),
                      subgroup_n_string = lookup_chr(value_headers, "subgroup_n")) ) %>%
    select(ordered_index, !!!value_string_vars_to_draw) %>%
    gather(x, labels, !!!value_string_vars_to_draw) ->
  valuesDf

  # Now (after getting the labels) take care for the "-Inf" for log(0) if a Cox computation did not converge
  # (only if na_rm = FALSE)
  if (log_scale)
  {
    hrDf %<>% mutate(HR=replace(HR, near(HR, 0), NA),
                     Upper_CI=replace(Upper_CI, is.infinite(Upper_CI), NA))
  }

  text_element <- calc_element("text", ggtheme)
  relative_text_size <- text_element$size / 12 # some values were developed for font size 12pt
  plot_margin <- calc_element("plot.margin", ggtheme)
  # geom_text ignores theme(). Use partial binding to create geom_text function with appropriate font
  geom_text_forest <- partial(geom_text,
                              hjust="left", vjust="center",
                              # convert font size to point size
                              size = text_element$size / ggplot2::.pt,
                              family = text_element$family,
                              fontface = text_element$face)

  margin_between_parts <- 8

  themePlot <- ggtheme +
    theme(
      axis.line = element_line(colour = "black"),
      #panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title = element_blank(),
      #plot.margin = unit(c(1,1,1,1), "lines")
      plot.margin = margin(0, margin_between_parts, 0, margin_between_parts, "pt")
    )

  themeTable <- ggtheme +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = "none",
          panel.border = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          plot.margin = unit(rep(0,4), "pt"))

  labels_x_pos <- space_needed(labelsDf$x, labelsDf$labels)
  values_x_pos <- space_needed(valuesDf$x, valuesDf$labels)
  # in fact, this is the y index of the table header
  max_ordered_index <- max(hrDf$ordered_index, valuesDf$ordered_index, labelsDf$orderedIndex)
  header_hlines_table <- c(max_ordered_index - line_increment, max_ordered_index + line_increment)
  header_hlines_plot <- header_hlines_table[[1]]
  hline_alpha <- 0.5
  header_hline_alpha <- 0.8
  hlines <- hlines[!hlines %in% header_hlines_table]
  y_lims <- c(0, max(max_ordered_index, header_hlines_table, header_hlines_plot, hlines))

  # if HR_x_limits is NA, or a vector, just pass as-is
  if (is.null(HR_x_limits))
  {
    if (log_scale)
    {
      log_HR_bound <- 0.01
      max_x_limits <- c(log_HR_bound, exp(-log(log_HR_bound)))
    }
    else
    {
      max_x_limits <- c(0, 100)
    }
    HR_x_limits <- c(max(max_x_limits[[1]], min(hrDf$HR, hrDf$Lower_CI, na.rm = TRUE)),
                     min(max_x_limits[[2]], max(hrDf$HR, hrDf$Upper_CI, na.rm = TRUE)))
  }



  hrDf %>%
    ggplot(aes(x=HR,y=ordered_index)) +
    themePlot +
    geom_point(size=5*relative_text_size, shape=18) +
    geom_errorbarh(aes(xmax = Upper_CI, xmin = Lower_CI), height = 0.15*relative_text_size) +
    geom_vline(xintercept = 1, linetype = "longdash") +
    geom_hline(yintercept=header_hlines_plot, alpha=header_hline_alpha) +
    scale_x_continuous("Hazard Ratio",
                       trans=ifelse(log_scale, "log", "identity"),
                       breaks = HR_x_breaks,
                       limits = HR_x_limits,
                       # dont delete CI bars if CI exceeds limits
                       oob=scales::rescale_none,
                       expand = c(0,0)) +
    scale_y_continuous("", limits = y_lims, expand = c(0,0),
                       breaks = hlines) ->
    hrPlot

  labelsDf %>%
    ggplot(aes(x = lookup_num(labels_x_pos, x), y = ordered_index, label = labels)) +
    geom_text_forest() +
    themeTable +
    geom_hline(yintercept=hlines, alpha=hline_alpha) +
    geom_hline(yintercept=header_hlines_table, alpha=header_hline_alpha) +
    scale_x_continuous("",
                       limits = range(labels_x_pos),
                       breaks = mean(values_x_pos), labels = "1", # pseudo label just for height
                       expand = c(0,0)) +
    scale_y_continuous("",
                       limits = y_lims,
                       expand = c(0,0),
                       labels = NULL) ->
    labelPlot

  valuesDf %>%
    ggplot(aes(x = lookup_num(values_x_pos, x), y = ordered_index, label = labels)) +
    geom_text_forest() +
    themeTable +
    geom_hline(yintercept=hlines, alpha=hline_alpha) +
    geom_hline(yintercept=header_hlines_table, alpha=header_hline_alpha) +
    scale_x_continuous("",
                       limits = range(values_x_pos),
                       breaks = mean(values_x_pos), labels = "1", # pseudo label just for height
                       expand = c(0,0)) +
    scale_y_continuous("",
                       limits = y_lims,
                       expand = c(0,0),
                       labels = NULL) ->
    valuePlot

  plot <- cowplot::plot_grid(labelPlot, hrPlot, valuePlot, ncol=3, rel_widths = relative_widths, align = "h")

  if (valid(title))
  {
    title_element <- calc_element("plot.title", ggtheme)
    draw_label_default_args <- list(label = title,
                                    size = title_element$size,
                                    fontfamily = title_element$family,
                                    fontface = title_element$face,
                                    colour = title_element$colour)
    draw_label_args <- list_modify(draw_label_default_args, !!!title_label_args)
    title_plot <- ggdraw() + do.call(draw_label, draw_label_args)
    plot <- cowplot::plot_grid(title_plot, plot, ncol=1,
                               rel_heights=c(min(2/max_ordered_index, 0.1), 1)) # rel_heights values control title margins
  }

  # subplots have zero margin; add global margin from theme
  plot <- plot + theme(plot.margin = plot_margin)

  plot %<>% structure(forestplot_entries=max_ordered_index,
                      papersize = c(width = base_papersize[[1]] * relative_text_size,
                                    height = base_papersize[[2]]*max_ordered_index/20*relative_text_size)) # 20 is a heuristic constant

  plot
}

#' Create a grid of forest plots
#'
#' Makes use of the stored layout information in a \code{\link{forest_plot}} plot to
#' create grids of plots.
#'
#' @param ... Pass individual plots returned by forest_plot, or lists of such plots (bare lists will be spliced).
#' @param nrow,ncol Specify the grid (one is sufficient, uses auto layout if both are null)
#' @param byrow If the plots are given in by-row, or by-column (byrow=FALSE) order
#' @param plot_grid_args Additional arguments to the \code{\link[cowplot]{plot_grid}} function which is used to create the grid.
#'
#' @return Return value of \code{\link[cowplot]{plot_grid}}
#' @export
forest_plot_grid <- function(...,
                             nrow = NULL,
                             ncol = NULL,
                             byrow = TRUE,
                             plot_grid_args = list())
{
  plots <- dots_splice(...)
  if (is_empty(plots))
  {
    warning("No forest plots given to forest_plot_grid. Returning NULL.")
    return()
  }

  g(nrow, ncol) %=% grid_layout(length(plots), nrow, ncol)

  if (!byrow)
  {
    plots <- .convert_rowness(plots, nrow, ncol, TRUE)
  }

  widths <- map_dbl(plots, ~attr(., "papersize")[[1]])
  heights <- map_dbl(plots, ~attr(., "papersize")[[2]])

  row_widths <- rep(0, nrow)
  col_heights <- rep(0, ncol)
  for (col in seq_len(ncol))
  {
    height_col <- 0
    for (row in seq_len(nrow))
    {
      idx <- (row-1)*ncol + col
      if (idx > length(plots))
        next
      row_widths[[row]] <- row_widths[[row]] + widths[[idx]]
      col_heights[[col]] <- col_heights[[col]] + heights[[col]]
    }
  }

  width <- max(row_widths)
  height <- max(col_heights)

  plot_grid_args <- list_modify(plot_grid_args,
                                plotlist = plots,
                                nrow = nrow,
                                ncol = ncol)
  do.call(cowplot::plot_grid, plot_grid_args) %>%
    structure(papersize = c(width = width, height = height))
}
