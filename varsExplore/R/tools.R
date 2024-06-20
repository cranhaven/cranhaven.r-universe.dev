#' datatable2 - Datatable with child rows
#'
#' Created by Reigo Hendrikson: <http://www.reigo.eu/2018/04/extending-dt-child-row-example/>
#'
#' @param x     A dataframe
#' @param vars  String vector. The variables you want to put in the details table, rather than
#'              keep in the main table.
#' @param font.size String. Default is "10pt".
#' @param dom   String. What DT::datatable elements to show. Default is 'fti'.
#' @param opts  Additional options for DT::datatable.
#' @param ...   Additional parameters for DT::datatable.
#'
#' @export
#' @return A DT::datatable.
#'
datatable2 <- function(x, vars = NULL, opts = NULL, font.size = "10pt", dom = 'fti', ...) {

  names_x <- names(x)
  if (is.null(vars)) stop("'vars' must be specified!")
  pos <- match(vars, names_x)
  if (any(purrr::map_chr(x[, pos], typeof) == "list"))
    stop("list columns are not supported in datatable2()")

  pos <- pos[pos <= ncol(x)] + 1
  rownames(x) <- NULL
  if (nrow(x) > 0) x <- cbind(' ' = '&oplus;', x)

  # options
  opts <- c(
    opts,
    list(
      initComplete = DT::JS(
        "function(settings, json) {",
        paste0("$(this.api().table().header()).css({'font-size': '", font.size, "'});"),
        "}"),
      class = "compact",
      dom = dom,
      pageLength = nrow(x),
      columnDefs = list(
        list(visible = FALSE, targets = c(0, pos)),
        list(orderable = FALSE, className = 'details-control', targets = 1),
        list(className = 'dt-left', targets = 1:3),
        list(className = 'dt-right', targets = 4:ncol(x))
      )
    ))

  DT::datatable(
    x,
    ...,
    escape = -2,
    options = opts,
    callback = DT::JS(.callback2(x = x, pos = c(0, pos)))
  )
}

.callback2 <- function(x, pos = NULL) {

  part1 <- "table.column(1).nodes().to$().css({cursor: 'pointer'});"

  part2 <- .child_row_table2(x, pos = pos)

  part3 <-
    "
   table.on('click', 'td.details-control', function() {
    var td = $(this), row = table.row(td.closest('tr'));
    if (row.child.isShown()) {
      row.child.hide();
      td.html('&oplus;');
    } else {
      row.child(format(row.data())).show();
      td.html('&ominus;');
    }
  });"

  paste(part1, part2, part3)
}

.child_row_table2 <- function(x, pos = NULL) {

  names_x <- paste0(names(x), ":")
  text <- "
  var format = function(d) {
    text = '<div><table >' +
  "

  for (i in seq_along(pos)) {
    text <- paste(text, glue::glue(
      "'<tr>' +
          '<td>' + '{names_x[pos[i]]}' + '</td>' +
          '<td>' + d[{pos[i]}] + '</td>' +
        '</tr>' + " ))
  }

  paste0(text,
         "'</table></div>'
      return text;};"
  )
}

#' Numeric x
#'
#' @param x A vector
#'
#' @return A vector of numbers with NAs removed, if `x` is numeric.
#'         If `x` is non-numeric or 100% NAs, returns a single NA.
#'
#' @importFrom stats "na.omit"
#'
num_x <- function(x){

  if(is.numeric(x)) {
    y <- na.omit(x)
    if(length(y) == 0) { y <- NA }
  } else {
    y <- NA
  }

  return(y)

}

#' Searchable variable explorer with labelled variables
#'
#' Creates a summary dataframe that can be used in RStudio similar to the variable
#' explorer in Stata, but which also includes the summary statistics. If `viewer`
#' is TRUE (default) the result is shown in RStudio's Viewer pane as a searchable
#' datatable.
#'
#' This is useful particularly if you have a large dataset with a very large number
#' of labelled variables with hard to remember names. Can also be used to generate
#' a table of summary statistics.
#'
#' @param df A data frame.
#' @param viewer Logical. Whether to show results as a searchable datatable
#'        in RStudio's Viewer pane. Default is TRUE.
#' @param digits Numeric. How many digits to show for the statistics in the Viewer Pane.
#'        Default is 2.
#' @param font.size String. Font size in the Viewer Pane. Default is "10pt".
#' @param value.labels.width Numeric. How many characters to include in the "Value
#'        labels" and "Values" columns. Default is 500.
#' @param silent Logical. If FALSE, function will return the summary dataframe. Default is TRUE.
#' @param minimal If TRUE only the number of observations and missing values are shown.
#'        Default is FALSE.
#'
#' @return If `silent = FALSE` the function returns the summary stats dataframe, each
#'        variable a row. This can be used for making summary tables, or viewed with
#'        the `View()` function.
#' @export
#'
#' @importFrom magrittr "%>%"
#' @examples
#'
#' qog <- rio::import("http://www.qogdata.pol.gu.se/dataarchive/qog_bas_cs_jan18.dta")
#' vars_explore(qog, silent = FALSE, viewer = FALSE)
#' \dontrun{
#' vars_explore(qog)
#' vars_explore(qog, minimal = TRUE)
#' vars_explore(qog, silent = FALSE, viewer = FALSE) %>% View()
#' }
#' qog_summary <- vars_explore(qog, silent = FALSE, viewer = FALSE)
#'
vars_explore <- function(df,
                         viewer = TRUE,
                         digits = 2,
                         font.size = "10pt",
                         value.labels.width = 500,
                         silent = TRUE,
                         minimal = FALSE) {

  stats <- "mean, median, sd, min, max" %>%
    stringr::str_replace("mean", "Mean") %>%
    stringr::str_replace("median", "Median") %>%
    stringr::str_replace("sd", "Std.Dev.") %>%
    stringr::str_replace("min", "Min") %>%
    stringr::str_replace("max", "Max") %>%
    stringr::str_remove_all(" ") %>%
    stringr::str_split(",") %>%
    unlist()

  # build basic summary
  summary_df <- data.frame(
    Variable    = names(df),
    Description = purrr::map_chr(df, ~ifelse(!is.null(attr(.x, "label")), attr(.x, "label"), "")),
    Obs.        = purrr::map_dbl(df, ~sum(!is.na(.x))),
    Missing     = purrr::map_dbl(df, ~sum( is.na(.x))))

  if (!minimal){
    summary_df <- summary_df %>%
      dplyr::mutate(
        Type        = purrr::map_chr(df, ~class(.x)),
        Mean        = purrr::map_dbl(df, ~mean  (num_x(.x))),
        Median      = purrr::map_dbl(df, ~median(num_x(.x))),
        Std.Dev.    = purrr::map_dbl(df, ~sd    (num_x(.x))),
        Min         = purrr::map_chr(df, ~min   (num_x(.x))),
        Max         = purrr::map_chr(df, ~max   (num_x(.x)))
      )

    # round numeric values
    summary_df <- summary_df %>% dplyr::mutate_if(is.numeric, ~round(.x, digits))

    # get value labels
    value_labels <- df %>%
      purrr::map(~names(attr(.x, "labels"))) %>%                    # creates list of value labels
      purrr::map(~glue::glue_collapse(.x, sep = "; ")) %>%          # glues all labels of a variable
      purrr::map_df(~ifelse(length(.x) == 0, NA, .x)) %>%           # replaces empty labels with NA
      tidyr::gather(key = "Variable", value = "Value labels") %>%   # transpose to long format
      dplyr::mutate(`Value labels` = stringr::str_trunc(`Value labels`, value.labels.width))

    # add value labels
    summary_df <- summary_df %>%
      dplyr::mutate(Values = purrr::map_chr(df,
                    ~stringr::str_trunc(paste(unique(.x), collapse = ", "), value.labels.width))) %>%
      dplyr::full_join(value_labels, by = "Variable") %>%
      dplyr::select(Variable, Description, Type, Obs., Missing, stats, Values, `Value labels`) %>%

      # fix possible encoding problems (e.g. special characters in country names)
      # DT::datatable gives errors for non-UTF8 characters
      dplyr::mutate(
        Variable       = iconv(Variable),
        Description    = iconv(Description),
        Values         = iconv(Values),
        `Value labels` = iconv(`Value labels`),
      )

  }

  # if viewer = TRUE show as searchable datatable in the viewer pane
  if(viewer) {

    tempFileName <- tempfile("summary_df_", fileext = ".html")

    if (minimal) {

      summary_df %>%
        DT::datatable(
          rownames = FALSE,
          #editable = TRUE,
          #extensions = 'Scroller',
          options = list(
            initComplete = DT::JS(
              "function(settings, json) {",
              paste0("$(this.api().table().header()).css({'font-size': '", font.size, "'});"),
              "}"),
            class = "compact",
            dom = 'fti',
            pageLength = nrow(summary_df),
            columnDefs = list(
              list(className = 'dt-left', targets = 1:3)
            )

            # # for Scroller extension
            # deferRender = TRUE,
            # scrollY = 200,
            # scroller = TRUE
          ),
        ) %>%
        DT::formatStyle(columns = colnames(summary_df), fontSize = font.size) %>%
        DT::saveWidget(tempFileName)

    } else {

    datatable2(
        summary_df,
        vars = c("Type", "Mean", "Median", "Std.Dev.", "Min", "Max", "Values", "Value labels"),
      ) %>%
      DT::formatStyle(columns = colnames(summary_df), fontSize = font.size) %>%
      DT::saveWidget(tempFileName)

    }

    rstudioapi::viewer(tempFileName)
  }

  # if silent = FALSE, return the summary dataframe
  if (silent) { message("See the Viewer Pane"); return(NULL) } else { return(summary_df) }
}
