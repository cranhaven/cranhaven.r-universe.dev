## Generate Javascript for datatable headers.
##
## This function generates Javascript code for datatable headers. The script adjusts header tooltips and names.
## The abbreviations have a minimum length of 7 characters.
##
## @param data An optional data.frame. Default is 'data' in the global environment.
## @return A character vector containing the JavaScript code.
header_callback <- function(data = data) {
  ## De r code heeft geen toegang tot het data-object uit de Javascript functie.
  ## Voeg dit daarom toe als optionele variabele
  ## Zie comment bij: https://vustudentanalytics.atlassian.net/browse/VUSASOFT-3541


  c(
    "function(thead, data, start, end, display){",
    "  var ncols = data[0].length;",
    sprintf(
      "  var shortnames = [%s]",
      paste0(paste0(
        "'", abbreviate(names(data), minlength = 7), "'"
      ), collapse = ",")
    ),
    sprintf(
      "  var tooltips = [%s];",
      paste0(paste0(
        "'", names(data), "'"
      ), collapse = ",")
    ),
    "  for(var i=0; i<ncols; i++){",
    "    $('th:eq('+i+')',thead).attr('title', tooltips[i]).text(shortnames[i]);",
    "  }",
    "}"
  )
}

## Generate Javascript for datatable cell values.
##
## This function generates Javascript code for datatable cell values. The script truncates cell values to 11 characters and adds a tooltip with the full value.
##
## @param data A data.frame.
## @return A character vector containing the JavaScript code.
value_callback <- function(data) {
  ## Zie comment bij: https://vustudentanalytics.atlassian.net/browse/VUSASOFT-3541

  c(
    "function(data, type, row, meta) {",
    "return type === 'display' && data.length > 11 ?",
    "'<span title=\"' + data + '\">' + data.substr(0, 9) + '...</span>' : data;",
    "}"
  )
}


## Add header-related Javascript to datatable options.
##
## This function takes a list of datatable options and adds the header-related Javascript from 'header_callback' to these options.
##
## @param options_DT A list of datatable options.
## @param data A data.frame.
## @return The updated list of datatable options.
add_with_limit_header_JS <- function(options_DT, data) {
  headerJS <- list(headerCallback = htmlwidgets::JS(header_callback(data)))

  ## Add header code
  options_DT <- c(options_DT, headerJS)

  return(options_DT)
}


## Add value-related Javascript to datatable options.
##
## This function takes a list of datatable options and adds the value-related Javascript from 'value_callback' to these options.
##
## @param options_DT A list of datatable options_DT.
## @param data A data.frame.
## @return The updated list of datatable options_DT.
add_width_limit_values_JS <- function(options_DT, data) {
  valueJS <- list(
    targets = "_all",
    render = htmlwidgets::JS(value_callback(data))
  )

  ## Extract current columnDefs internal lists (if set)
  ## Add valueJS to it and set new ColumnDefs
  ## INFO Code is a bit complex, but this method ensures it works also when columnDefs aren't set
  new_columns_options_DT <- append(options_DT["columnDefs"] %>% unname() %>% rlang::flatten(), list(valueJS))
  new_columns_options_DT <- stats::setNames(list(new_columns_options_DT), "columnDefs")

  options_DT["columnDefs"] <- new_columns_options_DT

  return(options_DT)
}


## Get basic datatable options.
##
## This function returns a list of basic datatable options.
##
## @return A list of datatable options.
basic_options <- function() {
  list(
    language = list(url = "//cdn.datatables.net/plug-ins/1.10.11/i18n/Dutch.json"),
    pagingType = "full",
    deferRender = TRUE,
    # dom = 'lfrti<"table-button"B>p',
    # dom = "<'table-button'B><lf><t><'row'<'col-sm-4'i><'col-sm-8'p>>",
    dom = "<'row'<'col-sm-6'l><'col-sm-6'f>><'row'<'col-sm-12'tr>><'row'<'col-sm-4'i><'col-sm-8'p>><'table-button paginate_button'B>",
    buttons = c("excel", "pdf", "print"),
    # info = TRUE,
    # processing = TRUE,
    columnDefs = list(
      list(
        targets = "_all",
        className = "dt-center"
      )
    )
  )
}


## Get advanced datatable options.
##
## This function returns a list of advanced datatable options.
##
## @return A list of datatable options.
advanced_options <- function() {
  list(
    pagingType = "full",
    colReorder = TRUE,
    rowReorder = TRUE,
    deferRender = TRUE,
    lengthChange = TRUE,
    dom = 'lfrti<"table-button"B>p',
    scrollX = "362px",
    buttons = c("colvis", "copy", "csv"),
    info = TRUE,
    processing = TRUE,
    columnDefs = list(
      list(
        targets = "_all",
        className = "dt-center"
      )
    )
  )
}



## Create a basic datatable.
##
## This function creates a basic datatable with various configurable options and features.
##
## @param data The data.frame to be displayed.
## @param rownames A logical value indicating whether to display row names.
## @param extensions A character vector specifying the DataTables extensions to be used.
## @param options_DT A list of DataTables options.
## @param limit_width A character string indicating how to limit column width.
## @param ... Additional arguments passed to DT::datatable().
## @return A datatable object.
make_basic_table <- function(data,
                             rownames = FALSE,
                             extensions = c("Buttons"),
                             options_DT = basic_options(),
                             limit_width = "values",
                             ...) {
  if (limit_width == "both") {
    ## set JS
    options_DT <- add_width_limit_values_JS(options_DT, data)
    options_DT <- add_with_limit_header_JS(options_DT)
  } else if (limit_width == "values") {
    options_DT <- add_width_limit_values_JS(options_DT, data)
  } else if (limit_width == "headers") {
    options_DT <- add_with_limit_header_JS(options_DT)
  }



  ## Add some basic logic
  if ("pageLength" %in% names(options_DT)) {
    # do nothing
  } else {
    if (nrow(data) <= 15) {
      options_DT <- c(options_DT, paging = FALSE)
      options_DT <- c(options_DT, info = FALSE)
    }
  }

  if ("searching" %in% names(options_DT)) {
    # do nothing
  } else {
    if (nrow(data) <= 15) {
      options_DT <- c(options_DT, searching = FALSE)
    }
  }

  if (nrow(data) > 15 & !("lengthChange" %in% options_DT)) {
    options_DT <- c(options_DT, list(lengthMenu = list(
      c(10, -1),
      c("10", "All")
    )))
  } else {
    options_DT <- c(options_DT, lengthChange = FALSE)
  }

  DT::datatable(data,
    rownames = rownames,
    extensions = extensions,
    options = options_DT,
    ## Escape is always true for security reasons, see documentation
    escape = TRUE,
    style = "bootstrap",
    ...
  )
}


## Create an advanced datatable.
##
## This function creates an advanced datatable with various configurable options and features.
##
## @param data The data.frame to be displayed.
## @param rownames A logical value indicating whether to display row names.
## @param filter A character string indicating where to display the table filter.
## @param extensions A character vector specifying the DataTables extensions to be used.
## @param options_DT A list of DataTables options.
## @param limit_width A logical value indicating whether to limit column width.
## @param ... Additional arguments passed to DT::datatable().
## @return A datatable object.
make_advanced_table <- function(
    data,
    rownames = FALSE,
    filter = "top",
    extensions = c("Buttons", "ColReorder", "RowReorder"),
    options_DT = advanced_options(),
    limit_width = "values",
    ...) {
  if (limit_width == "both") {
    ## set JS
    options_DT <- add_width_limit_values_JS(options_DT, data)
    options_DT <- add_with_limit_header_JS(options_DT)
  } else if (limit_width == "values") {
    options_DT <- add_width_limit_values_JS(options_DT, data)
  } else if (limit_width == "headers") {
    options_DT <- add_with_limit_header_JS(options_DT)
  }

  ## Voeg All als optie lengte tabel toe
  if ("lengthMenu" %in% options_DT) {
    ## do nothing
  } else {
    options_DT <- c(options_DT, list(lengthMenu = list(
      c(10, 25, 50, 100, -1),
      c("10", "25", "50", "100", "All")
    )))
  }

  if ("pageLength" %in% names(options_DT)) {
    # do nothing
  } else {
    if (nrow(data) <= 25) {
      options_DT <- c(options_DT, paging = FALSE)
    }
  }

  DT::datatable(data,
    rownames = rownames,
    extensions = extensions,
    filter = filter,
    options = options_DT,
    ## Escape is always true for security reasons, see documentation
    escape = TRUE,
    ...
  )
}


## Create a basic datatable for HTML rendering.
##
## This function creates a basic datatable with options optimized for HTML rendering.
##
## @param data The data.frame to be displayed.
## @param ... Additional arguments passed to 'make_basic_table()'.
## @return A datatable object.
make_basic_table_html <- function(data, ...) {
  make_basic_table(
    data,
    width = "100%",
    height = "auto",
    options_DT = c(basic_options(), scrollX = TRUE),
    limit_width = NULL,
    ...
  )
}


## Create an advanced datatable for HTML rendering.
##
## This function creates an advanced datatable with options optimized for HTML rendering.
##
## @param data The data.frame to be displayed.
## @param ... Additional arguments passed to 'make_advanced_table()'.
## @return A datatable object.
make_advanced_table_html <- function(data, ...) {
  make_advanced_table(
    data,
    width = "100%",
    height = "auto",
    options_DT = c(basic_options(), scrollX = TRUE),
    limit_width = NULL,
    ...
  )
}


#' Prepare a data table for displaying
#'
#' This function prepares a data table for displaying by providing user-friendly names, removing unneeded variables, and formatting percentages.
#'
#' @param y A string specifying the column name to be used as the y-axis variable.
#' @param df A data frame containing the raw data.
#' @param df_summarized A data frame containing the summarized data.
#' @param id A string specifying the ID associated with the data.
#' @param y_right An optional string specifying the column name to be used as the second y-axis variable. Default is NULL.
#' @param facet_var A symbol specifying the column to be used for faceting. Default is 'VIS_Groep'.
#' @param facet_name_var A symbol specifying the column to be used for faceting names. Default is 'VIS_Groep_naam'.
#' @param table_type Choose from basic for a simple datatable and advanced for more buttons etc.
#' @param rownames A logical value indicating whether to display row names.
#' @param extensions A character vector specifying the DataTables extensions to be used.
#' @param options_DT A list of DataTables options.
#' @param limit_width A character string indicating how to limit column width.
#' @param ... Further arguments passed on to the 'make_basic_table' function.
#' @return A DT::datatable object ready for displaying.
#' @export
#' @examples
#'df <- data.frame(VIS_Groep = c("Group1", "Group1", "Group2", "Group2"),
#'                 VIS_Groep_naam = c("Name1", "Name1", "Name2", "Name2"),
#'                 y = c(TRUE, TRUE, FALSE, FALSE), z = c(TRUE, FALSE, TRUE, FALSE))
#'df_summarized <- df %>%
#'  dplyr::group_by(VIS_Groep, VIS_Groep_naam) %>%
#'  dplyr::summarise(
#'    y = mean(y),
#'    z = mean(z)
#'  ) %>%
#'  dplyr::ungroup()
#'  id <- "id"
#'output <- prep_table("y", df, df_summarized, id = id)
#'
prep_table <- function(y,
                       df,
                       df_summarized,
                       id,
                       y_right = NULL,
                       facet_var = rlang::sym("VIS_Groep"),
                       facet_name_var = rlang::sym("VIS_Groep_naam"),
                       table_type = c("basic", "advanced"),
                       rownames = FALSE,
                       extensions = c("Buttons"),
                       options_DT = basic_options(),
                       limit_width = "values",
                       ...) {
  ## table type
  table_type <- dplyr::first(table_type)

  ## Remove unneeded variables
  dfTabel <- df_summarized %>%
    dplyr::select(
      -!!facet_name_var,
      -!!facet_var
    )

  ## Set user friendly names
  names(dfTabel) <- purrr::map_chr(names(dfTabel), ~ display_name(.x, id))

  ## Get boolean vars in order to add formatting %
  if (is.logical(df[[y]])) {
    sBoolean_vars <- y
  } else {
    sBoolean_vars <- c()
  }

  if (!is.null(y_right) && is.logical(df[[y_right]])) {
    sBoolean_vars <- c(sBoolean_vars, y_right)
  }

  sBoolean_vars <- sBoolean_vars %>%
    purrr::map_chr(~ display_name(.x, id))

  if (table_type == "basic") {
    ## Make datatable object
    dfTabel <- dfTabel %>%
      make_basic_table(
        rownames = rownames,
        extensions = extensions,
        options_DT = options_DT,
        limit_width = limit_width,
        caption = dplyr::first(df_summarized[[facet_name_var]]),
        ...
      ) %>%
      DT::formatPercentage(sBoolean_vars, 2)
  } else {
    dfTabel <- dfTabel %>%
      make_advanced_table(
        rownames = rownames,
        extensions = extensions,
        options_DT = options_DT,
        limit_width = limit_width,
        caption = dplyr::first(df_summarized[[facet_name_var]]),
        ...
      ) %>%
      DT::formatPercentage(sBoolean_vars, 2)
  }

  return(dfTabel)
}
