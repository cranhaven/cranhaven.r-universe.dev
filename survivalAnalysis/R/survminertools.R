#' Kaplan Meier plots from survival results.
#'
#' Uses \code{\link[survminer]{ggsurvplot}} from the survminer package to create publication-ready plots.
#'
#' @param \dots One or many SurvivalAnalysisResult objects as returned by \code{\link{analyse_survival}} and
#'     arguments that will be passed to ggsurvplot.
#'     Bare lists will be spliced.
#'     If using lists, the same argument may be contained in multiple lists;
#'     in this case, the last occurrence is used, i.e. you can first pass a list
#'     with default arguments, and then override some of them.
#'     If you want to combine two curves in one plot (\code{\link[survminer]{ggsurvplot_combine}}), wrap
#'     them in \code{\link{in_one_kaplan_meier_plot}} when passing as argument here.
#'     (otherwise you will get a list with separate plots for each)
#'     In addition to all arguments supported by \code{\link[survminer]{ggsurvplot}}, these arguments and shortcuts can be used additionally:
#'     \itemize{
#'     \item break.time.by: breakByYear, breakByHalfYear, breakByQuarterYear, breakByMonth (numeric value only in ggsurvplot)
#'     \item xscale: scaleByYear, scaleByMonth (numeric value only in ggsurvplot)
#'     \item hazard.ratio (logical): display hazard ratios in addition to p value, complementing pval=T
#'     \item xlab: \{.OS,.PFS,.TTF,.DFS\}.\{years,months,days\}
#'     \item table.layout: clean, displays risk table only with color code and number, no grid, axes or labels.
#'           (do not forget risk.table=TRUE to see something)
#'     \item papersize: numeric vector of length 2, c(width, height), unit inches. kaplan_meier_plot
#'           will store a "papersize" attribute with this value which is read by \code{\link[tidytidbits]{save_pdf}}
#'     \item ggplot.add: ggplot2 object to add to the ggplot plot part of the created KM plot.
#'           One common use case is manual specification of the line type, which is currently not possible with ggsurvplot.
#'           The passed object can be result of "+" operations will be added via "+" as usual with ggplot() objects.
#'     }
#' @param mapped_plot_args Optionally, if given n objects to plot, a named list of vectors of size n.
#'     The name is an argument names passed to ggsurvplot. The elements of the vector will be mapped 1:1 to each object.
#'     This allows to perform batch plotting where only few arguments differ (e.g. titles A, B, C...) between the plots.
#' @param p_lessthan_cutoff The lower limit below which p value will be displayed as "less than".
#'     If p_lessthan_cutoff == 0.001, the a p value of 0.002 will be displayed as is, while 0.0005 will become "p < 0.001".
#'
#' @return If given one result to plot, one ggsurvplot object; if given more than one result, a list of ggsurvplot objects.
#' @export
#'
#' @examples
#' library(magrittr)
#' library(dplyr)
#' survival::aml %>%
#'   analyse_survival(vars(time, status), x) %>%
#'   kaplan_meier_plot(break.time.by="breakByMonth",
#'                     xlab=".OS.months",
#'                     risk.table=TRUE,
#'                     ggtheme=ggplot2::theme_bw(10))
kaplan_meier_plot <- function(...,
                              mapped_plot_args = list(),
                              p_lessthan_cutoff = 0.001)
{
  args <- dots_splice(...)
  survival_results <- list()
  survminer_args <- list()
  for (i in seq_along(args))
  {
    arg <- args[[i]]
    name <- names(args)[[i]]
    if (inherits(arg, "SurvivalAnalysisMultivariateResult"))
      stop("kaplan_meier_plot: Got a multivariate result: Kaplan Meier plots are only possible for univariate results")
    else if (inherits(arg, "SurvivalAnalysisUnivariateResult"))
      survival_results <- append_object(arg, survival_results)
    else if (name == "" && is.null(arg))
      warning("kaplan_meier_plot: Encountered unnamed null argument at position ", i, ", ignoring")
    else if (name == "")
      stop("kaplan_meier_plot: Encountered unnamed argument which is not a univariate survival result at position ", i)
    else
      survminer_args <- append_object(arg, survminer_args, name = name)
  }

  # Remove duplicate args by allowing to "override" default args.
  # As we want to preserve the last, not the first dup, we need to detect in reverse order
  survminer_args %<>% names() %>% rev() %>% duplicated() %>% rev() %>% {survminer_args[!.]}

  if (is_empty(survival_results))
  {
    warning("kaplan_meier_plot: Got no survival results to plot, returning empty list")
    list()
  }

  do_plot <- function(result, idx)
  {
    if (has_length(mapped_plot_args))
    {
      survminer_args <- list_modify(survminer_args, !!!map(mapped_plot_args, idx))
    }
    survminer_args <- list_modify(survminer_args, !!!result[["plot_args"]])
    .survivalDoSurvminerPlot(result, survminer_args, p_lessthan_cutoff)
  }

  if (has_length(survival_results, 1))
  {
    do_plot(survival_results[[1]], 1)
  }
  else
  {
    map(seq_along(survival_results), function(idx) do_plot(survival_results[[idx]], idx))
  }
}

#' Display multiple survival curves within the same Kaplan Meier plot
#'
#' Utility method to tell \code{\link{kaplan_meier_plot}} to combine the given
#' survival results within the same plot
#'
#' @param \dots Named SurvivalAnalysisResult objects as returned by \code{\link{analyse_survival}}.
#' Please note that the results need to come from the same data source and should
#' only differ by the "by" parameter. The first given result acts as source for common attributes.
#' Also takes one single named bare list.
#' @return Internal object for use by \code{\link{kaplan_meier_plot}} or
#' \code{\link{kaplan_meier_grid}} only
#' @export
#'
in_one_kaplan_meier_plot <- function(...)
{
  args <- dots_list(...,
                       .named = TRUE)

  if (length(args) == 1 && is_bare_list(args[[1]]))
  {
    args <- args[[1]]
  }

  if (length(args) == 1)
  {
    warning("in_one_kaplan_meier_plot: Got only one result. Calling this method then has no effect.")
    return(args[[1]]) #no-op
  }

  main_result <- args[[1]]
  for (i in seq(args))
  {
    arg <- args[[i]]
    if (inherits(arg, "SurvivalAnalysisMultivariateResult"))
      stop("in_one_kaplan_meier_plot: Got a multivariate result: Kaplan Meier plots are only possible for univariate results")
    else if (inherits(arg, "SurvivalAnalysisUnivariateResult"))
      main_result[["combined_fits"]] <- append_object(arg$fit,
                                                      main_result[["combined_fits"]],
                                                      name = names(args)[[i]])
    else
      stop("in_one_kaplan_meier_plot: Encountered argument which is not a univariate survival result at position ", i)
  }

  # we detect later on the presence of the combined_fits attribute
  main_result
}

.convert_rowness <- function(x, nrow, ncol, order_is_byrow, cut_trailing_na = FALSE)
{
  byrow_order <- matrix(seq_len(nrow *  ncol), nrow = nrow, ncol = ncol, byrow=order_is_byrow) %>% as.vector()
  # matrix may be larger than length of x. Then there are NULLs/NAs in x.
  in_x <- byrow_order %in% seq_along(x)
  x <- x[byrow_order]
  if (cut_trailing_na)
  {
    # There may be missing entries at the end or intermediate.
    # Cut the trailing missing entries
    last_true <- max(which(in_x))
    x <- x[seq_len(last_true)]
  }
  x
}

.layout_matrix <- function(x, nrow, ncol, order_is_byrow)
{
  m <- matrix(seq_len(nrow *  ncol), nrow = nrow, ncol = ncol, byrow=order_is_byrow)
  m[!(m %in% seq_along(x))] <- NA
  m
}

#' A grid of kaplan meier plots
#'
#' @inheritParams kaplan_meier_plot
#' @param nrow,ncol Determines the layout by giving nrow and/or ncol, if missing, uses an auto layout.
#' @param layout_matrix Optionally specify a layout matrix, which is passed to \code{\link[gridExtra]{marrangeGrob}}
#' @param byrow If no layout_matrix is specified and there are multiple rows: How should the plots by layout?
#'     The order of the plots can be by-row (default) or by-col (set byrow=FALSE).
#' @param mapped_plot_args Optionally, if given n objects to plot, a named list of vectors of size n.
#'     The name is an argument names passed to ggsurvplot. The elements of the vector will be mapped 1:1 to each object.
#'     This allows to perform batch plotting where only few arguments differ (e.g. titles A, B, C...) between the plots.
#'     Please note that only object that need plotting (survival_analysis results) are considered, not those that
#'     are already plotted (kaplan_meier_plot results)
#' @param paperwidth,paperheight,size_per_plot You can specify the size per plot, or the full paper width and height.
#'     size_per_plot may be a number (width == height) or two-dimensional, width and height.
#'     The resulting paper size will be stored as a papersize attribute that is e.g.
#'     read by \code{\link[tidytidbits]{save_pdf}}
#' @param title,surv.plot.height,risk.table.height,ncensor.plot.height Passed to \code{\link[survminer]{arrange_ggsurvplots}}
#'
#' @return An object of class arrangelist, which can be printed or saved to pdf with ggsave().
#' @export
kaplan_meier_grid <- function(...,
                              nrow = NULL,
                              ncol = NULL,
                              layout_matrix = NULL,
                              byrow = TRUE,
                              mapped_plot_args = list(),
                              paperwidth = NULL,
                              paperheight = NULL,
                              size_per_plot = dinAWidth(5),
                              title = NA,
                              surv.plot.height = NULL,
                              risk.table.height = NULL,
                              ncensor.plot.height = NULL,
                              p_lessthan_cutoff = 0.001)
{
  args <- dots_splice(...)
  to_plot <- c()
  survminer_args <- list()
  plots <- list()
  for (i in seq_along(args))
  {
    arg <- args[[i]]
    name <- names(args)[[i]]

    if (inherits(arg, "SurvivalAnalysisMultivariateResult"))
      stop("kaplan_meier_plot: Got a multivariate result: Kaplan Meier plots are only possible for univariate results")
    else if (inherits(arg, "SurvivalAnalysisUnivariateResult"))
    {
      plots <- c(plots, i)
      to_plot <- c(to_plot, length(plots))
    }
    else if (inherits(arg, "ggsurvplot"))
      plots <- append_object(arg, plots)
    else if (name == "" && is.null(arg))
      warning("kaplan_meier_plot: Encountered unnamed null argument at position ", i, ", ignoring")
    else if (name == "")
      stop("kaplan_meier_grid: Encountered unnamed argument which is not a univariate survival result or a plot: at position ", i)
    else
      survminer_args <- append_object(arg, survminer_args, name = name)
  }

  if (has_length(to_plot))
  {
    # to_plot stores the indexes of plots where plots need to be plotted.
    # At these places, plots stores the corresponding index in args, where the survival result can be found.
    arg_idc_to_plot <- plots[to_plot] %>% as_vector
    survival_results <- args[arg_idc_to_plot]
    plots[to_plot] <- kaplan_meier_plot(survival_results, survminer_args,
                                        mapped_plot_args = mapped_plot_args,
                                        p_lessthan_cutoff = p_lessthan_cutoff)
  }

  if (is_empty(plots))
  {
    warning("kaplan_meier_grid: Nothing to plot!")
    return()
  }

  if (is_null(layout_matrix))
  {
    g(nrow, ncol) %=% grid_layout(length(plots), nrow, ncol)
  }
  else
  {
    nrow = nrow(layout_matrix)
    ncol = ncol(layout_matrix)
  }

  if (!is_null(size_per_plot) || (!is_null(paperwidth) && !is_null(paperheight)))
  {
    if (is_null(paperwidth))
    {
      if (length(size_per_plot) == 2)
      {
        paperwidth <- ncol * size_per_plot[[1]]
      }
      else
      {
        paperwidth <- ncol * size_per_plot
      }
    }
    if (is_null(paperheight))
    {
      if (length(size_per_plot) == 2)
      {
        paperheight <- nrow * size_per_plot[[2]]
      }
      else
      {
        paperheight <- nrow * size_per_plot
      }
    }
  }
  else
  {
    paperwidth <- NULL
    paperheight <- NULL
  }

  gtables <- map(plots, ggsurvplot_to_gtable)

  if (is_null(layout_matrix))
  {
    layout_matrix <- .layout_matrix(gtables, nrow, ncol, byrow)
  }

  arrangelist <- gridExtra::marrangeGrob(gtables,
                                         ncol = ncol, nrow = nrow,
                                         layout_matrix = layout_matrix,
                                         top = title)

  # if (byrow)
  # {
  #   # arrange_ggsurvplot expects plot by-column, as default by matrix().
  #   # Our default is byrow. Use matrix() to reorder by-row -> by-column
  #   plots <- .convert_rowness(plots, nrow, ncol, TRUE)
  #   # this may introduce intermediate NULL entries, which we cannot easily resolve
  #   missing <- map_lgl(plots, is_null)
  #   if (any(missing))
  #   {
  #     warning("Cannot layout as requested by-row due to limitations of arrange_ggsurvplots. Specify by-column as desired.")
  #     plots <- plots[!missing]
  #   }
  # }
  #
  # arrangelist <-
  #   arrange_ggsurvplots(plots,
  #                       print = FALSE,
  #                       ncol = ncol, nrow = nrow,
  #                       title = title,
  #                       surv.plot.height = surv.plot.height,
  #                       risk.table.height = risk.table.height,
  #                       ncensor.plot.height = ncensor.plot.height)

  if (!is_null(paperwidth) && !is_null(paperheight))
  {
    arrangelist <- structure(arrangelist, papersize = c(width=paperwidth, height=paperheight))
  }
  invisible(arrangelist)
}


#' Build a gtable representation from a ggsurvplot object
#'
#' @param ggsurv_obj The ggsurvplot object
#' @param surv.plot.height,risk.table.height,ncensor.plot.height Layout parameters, see \code{\link[survminer]{arrange_ggsurvplots}}
#'
#' @return A gtable object
#' @export
ggsurvplot_to_gtable <- function(ggsurv_obj,
                                 surv.plot.height = NULL,
                                 risk.table.height = NULL,
                                 ncensor.plot.height = NULL)
{
  stopifnot(inherits(ggsurv_obj, "ggsurvplot"))
  # abuse arrange_ggsurvplots to retrieve the gtable representation
  # (inaccessible internal function .build_ggsurvplot)
  arrangelist <- arrange_ggsurvplots(list(ggsurv_obj),
                                     nrow = 1,
                                     ncol = 1,
                                     print = FALSE,
                                     surv.plot.height = surv.plot.height,
                                     risk.table.height = risk.table.height,
                                     ncensor.plot.height = ncensor.plot.height)
  # should have length 1 exactly
  arrangelist[[1]]
}

# Returns a c(row, columns) to layout n items in 2,3 or 4 columns
# For 1-4 items, this is the number of columns.
# For >4 and <8 items, uses 2 or 3 columns.
# For >=8 items, used 3 or 4 columns.
gridAutoLayout <- function(n)
{
  cols <- ifelse(n>4,
                 ifelse(n %% 3,
                        ifelse(n >=8,
                               ifelse(n %% 3,
                                      4,
                                      3),
                               2),
                        3),
                 n)
  rows <- ceiling(n/cols)
  return(c(rows, cols))
}

gridLayoutUnderspecified <- function(n, rows = NULL, cols = NULL)
{
  if (is.null(rows) && is.null(cols))
    stop("Both rows and cols are NULL: layout is not specified at all.")
  if (is.null(rows))
    rows <- ceiling(n/cols)
  else
    cols <- ceiling(n/rows)

  c(rows, cols)
}

#' Grid layouting
#'
#' Creates a grid layout nrow x ncol
#' for n items.
#'
#' @param n Number of items in grid
#' @param rows,cols Pass one of rows or cols, or none, in which case auto layout is used.
#'
#' @return A numeric vector of length 2: rows, cols
#' @export
#'
#' @examples
#' grid_layout(24, cols=4)
#' grid_layout(24)
#' grid_layout(24, rows=2)
grid_layout <- function(n, rows = NULL, cols = NULL)
{
  if (is.null(rows) && is.null(cols))
    return(gridAutoLayout(n))
  else if (is.null(rows) || is.null(cols))
    return(gridLayoutUnderspecified(n, rows, cols))
  c(rows, cols)
}

.survivalScaleConstants <- function()
{
  scaleByMonths <- 30.4375 # exact number: 30.436875
  scaleByYears <- 365.25 # exact number : 365.2425
  breakByYears <- 365.25
  breakByHalfYear <- 6*scaleByMonths
  breakByQuarterYear <- 3*scaleByMonths
  return(localVariables())
}

.survivalSuggestedSurvminerArgs <- function()
{
  sourceVariables(.survivalScaleConstants())
  return(list(
    ylab = "Cumulative Survival",
    xscale = scaleByMonths,
    break.time.by = "breakByMonth",
    pval = TRUE, pval.size = 3,
    legend = c(0.8, 0.8), legend.title = "",
    axes.offset = TRUE, censor.shape = "|", censor.size = 3))
}


.survivalDoSurvminerPlot <- function(result,
                                     survminerArgs,
                                     p.lessthan.cutoff)
{
  sourceVariables(.survivalScaleConstants())
  # a list of arguments that the user can override
  suggestedArgs <- .survivalSuggestedSurvminerArgs()

  text_element <- NULL
  if ("ggtheme" %in% names(survminerArgs))
  {
    text_element <- calc_element("text", survminerArgs$ggtheme)
    suggestedArgs[["font.family"]] <- text_element$family
  }

  if ("table.layout" %in% names(survminerArgs))
  {
    if (survminerArgs[["table.layout"]] == "clean")
    {
      table_font_size <- 8
      table_font_family <- "Arial"
      if (!is_null(text_element))
      {
        table_font_size <- round(text_element$size * 0.8)
        table_font_family <- text_element$family
      }
      table_theme <- theme_survminer() +
        theme_cleantable(base_size = table_font_size,
                         base_family = table_font_family) +
        theme(
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          plot.title = element_blank(),
          plot.margin = margin(0,0,2,0)
        )
      table_args <- list(risk.table.title="",
                         tables.y.text = FALSE,
                         risk.table.fontsize=table_font_size/ggplot2::.pt,
                         tables.theme = table_theme,
                         tables.height = 0.1)
      # we add this to the suggested args such that still all settings can be fine-tuned by the user
      suggestedArgs <- list_modify(suggestedArgs, !!!table_args)
    }
    else
    {
      warning("Unknown value for argument table.layout: ", survminerArgs[["table.layout"]])
    }
  }
  survminerArgs <- list_modify(suggestedArgs, !!!survminerArgs)

  factorId <- pluck(result, "data_attributes", "factor_id")

  if ("break.time.by" %in% names(survminerArgs) && is.character(survminerArgs[["break.time.by"]]) )
  {
    if (startsWith(survminerArgs[["break.time.by"]], "breakByYear"))
    {
      survminerArgs[["break.time.by"]] <- breakByYears
    }
    else if (startsWith(survminerArgs[["break.time.by"]], "breakByHalfYear"))
    {
      survminerArgs[["break.time.by"]] <- breakByHalfYear
    }
    else if (startsWith(survminerArgs[["break.time.by"]], "breakByQuarterYear"))
    {
      survminerArgs[["break.time.by"]] <- breakByQuarterYear
    }
    else if (startsWith(survminerArgs[["break.time.by"]], "breakByMonth"))
    {
      survminerArgs[["break.time.by"]] <- scaleByMonths
    }
    else
      stop("Unsupported text argument to break.time.by: ", survminerArgs[["break.time.by"]])
  }
  if ("xscale" %in% names(survminerArgs) && is.character(survminerArgs[["xscale"]]) )
  {
    if (startsWith(survminerArgs[["xscale"]], "scaleByYear"))
    {
      survminerArgs[["xscale"]] <- "d_y"
    }
    else if (startsWith(survminerArgs[["xscale"]], "scaleByMonth"))
    {
      survminerArgs[["xscale"]] <- "d_m"
    }
    # else leave as is
  }


  if (factorId == "1")
  {
    survminerArgs[["pval"]] <- F
  }
  else
  {
    if (!"legend.labs" %in% names(survminerArgs))
    {
      survminerArgs[["legend.labs"]] <- result$factorLabels
    }
    if ("pval" %in% names(survminerArgs) && is.logical(survminerArgs[["pval"]]) && survminerArgs[["pval"]])
    {
      # pass value instead of letting survminer calculate the p-value, which fails sometimes
      survminerArgs[["pval"]] <- survivalFormatPValue(pValueOfSurvDiff(result$diff), with_prefix = TRUE,
                                                      p.lessthan.cutoff = p.lessthan.cutoff)
    }
    if ("hazard.ratio" %in% names(survminerArgs))
    {
      if (is.logical(survminerArgs[["hazard.ratio"]]))
      {
        if (survminerArgs[["hazard.ratio"]] && .has_strata(result))
        {
          df <- .per_stratum_hrs(result,
                                 format_numbers = TRUE,
                                 p_precision = 3,
                                 hr_precision =  2,
                                 p_less_than_cutoff = 0.001)
          if (.n_strata(result) == 2)
          {
            df[1,] %$%
              str_c("HR = ", HR, " (CI ", Lower.CI, " \U2013 ", Upper.CI, ")") %>%
              {str_c(survminerArgs[["pval"]], ., sep="\n")} ->
            survminerArgs[["pval"]]
          }
          else
          {
            # omit every second row, which contains the inverted HRs
            df[seq(1, nrow(df), 2),] %$%
              str_c(label, ": HR = ", HR, " (CI ", Lower.CI, " \U2013 ", Upper.CI, ")", collapse = "\n") %>%
              {str_c(survminerArgs[["pval"]], ., sep="\n")} ->
              survminerArgs[["pval"]]
          }
        }
      }
      else
      {
        survminerArgs[["pval"]] <- str_c(survminerArgs[["pval"]], survminerArgs[["hazard.ratio"]], sep = " ")
      }
    }
  }

  if (!"ggtheme" %in% names(survminerArgs))
  {
    survminerArgs[["ggtheme"]] <- theme_bw(base_size = 10, base_family="Arial")
  }
  if ("xlab" %in% names(survminerArgs))
  {
    if (survminerArgs[["xlab"]] == ".OS.days")
    {
      survminerArgs[["xlab"]] <- "Overall Survival (days)"
    }
    else if (survminerArgs[["xlab"]] == ".TTF.days")
    {
      survminerArgs[["xlab"]] <- "Time to Treatment Failure (days)"
    }
    else if (survminerArgs[["xlab"]] == ".PFS.days")
    {
      survminerArgs[["xlab"]] <- "Progression Free Survival (days)"
    }
    else if (survminerArgs[["xlab"]] == ".DFS.days")
    {
      survminerArgs[["xlab"]] <- "Disease Free Survival (days)"
    }
    if (survminerArgs[["xlab"]] == ".OS.months")
    {
      survminerArgs[["xlab"]] <- "Overall Survival (months)"
    }
    else if (survminerArgs[["xlab"]] == ".TTF.months")
    {
      survminerArgs[["xlab"]] <- "Time to Treatment Failure (months)"
    }
    else if (survminerArgs[["xlab"]] == ".PFS.months")
    {
      survminerArgs[["xlab"]] <- "Progression Free Survival (months)"
    }
    else if (survminerArgs[["xlab"]] == ".DFS.months")
    {
      survminerArgs[["xlab"]] <- "Disease Free Survival (months)"
    }
    else if (survminerArgs[["xlab"]] == ".OS.years")
    {
      survminerArgs[["xlab"]] <- "Overall Survival (years)"
    }
    else if (survminerArgs[["xlab"]] == ".TTF.years")
    {
      survminerArgs[["xlab"]] <- "Time to Treatment Failure (years)"
    }
    else if (survminerArgs[["xlab"]] == ".PFS.years")
    {
      survminerArgs[["xlab"]] <- "Progression Free Survival (years)"
    }
    else if (survminerArgs[["xlab"]] == ".DFS.years")
    {
      survminerArgs[["xlab"]] <- "Disease Free Survival (years)"
    }
  }

  tickslab.x <- survminerArgs[["tickslab.x"]]
  tickslab.y <- survminerArgs[["tickslab.y"]]
  survminerArgs[["tickslab.x"]] <- NULL
  survminerArgs[["tickslab.y"]] <- NULL
  papersize <- survminerArgs[["papersize"]]
  survminerArgs[["papersize"]] <- NULL
  ggplot_addition <- survminerArgs[["ggplot.add"]]
  survminerArgs[["ggplot.add"]] <- NULL

  # low-key support for the _combine feature via the in_one_kaplan_meier_plot helper
  if ("combined_fits" %in% names(result))
  {
    survplotArgs <- c(list(fit = result$combined_fits, data = result$data), survminerArgs)
    plot <- do.call(ggsurvplot_combine, survplotArgs)
  }
  else
  {
    survplotArgs <- c(list(fit = result$fit, data = result$data), survminerArgs)
    plot <- do.call(ggsurvplot, survplotArgs)
  }

  if (!is.null(tickslab.x) && tickslab.x == FALSE)
  {
    plot$plot <- plot$plot + theme(axis.text.x = element_blank())
  }
  if (!is.null(tickslab.y) && tickslab.y == FALSE)
  {
    plot$plot <- plot$plot + theme(axis.text.y = element_blank())
  }
  if (!is.null(ggplot_addition))
  {
    plot$plot <- plot$plot + ggplot_addition
  }

  if (!is.null(papersize))
  {
    if (!is.numeric(papersize) || length(papersize) != 2)
    {
      warning("papersize must be a numeric vector of length 2. Ignoring.")
    }
    else
    {
      plot %<>%
        structure(papersize = papersize)
    }
  }

  plot
}
