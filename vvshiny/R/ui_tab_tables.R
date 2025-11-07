## @title tabPanelTables function
## @description Function to define the structure of tables based on the 'id' parameter.
## @param id A string representing the id.
## @param table_one A string representing the first table. Default is "tabel".
## @param table_two A string representing the second table. Default is "tabel_twee".
## @return A tab panel with one or two tables based on the 'id' parameter.
tabPanelTables <- function(id, table_one = "tabel", table_two = "tabel_twee") {
  if (stringr::str_detect(id, "bench")) {
    ## Call function to create a tab panel with two tables
    tabTableTwo(id, table_one, table_two)
  } else if (stringr::str_detect(id, "comp")) {
    ## Call function to create a tab panel with one table
    tabTableOne(id, table_one)
  }
}


#' @title tabTableOne function
#' @description Function to create a tab panel with one table.
#' @param id A string representing the id.
#' @param table_one A string representing the table.
#' @return A tab panel with one table.
#' @export
#' @examples
#' dummy_data <- data.frame(
#'   A = 1:5,
#'   B = letters[1:5]
#' )
#' dummy_dt <- DT::datatable(dummy_data)
#' tabTableOne("dummy_id", dummy_dt)
tabTableOne <- function(id, table_one) {
  shiny::tabPanel(
    "Tabel",
    shiny::fluidRow(
      shiny::column(
        width = 12,
        align = "center",
        shinycssloaders::withSpinner(DT::DTOutput(shiny::NS(id, table_one)))
      )
    )
  )
}


#' @title tabTableTwo function
#' @description Function to create a tab panel with two tables.
#' @param id A string representing the id.
#' @param table_one A string representing the first table.
#' @param table_two A string representing the second table.
#' @return A tab panel with two tables.
#' @export
#' @examples
#' dummy_data1 <- data.frame(
#'   A = 1:5,
#'   B = letters[1:5]
#' )
#' dummy_dt1 <- DT::datatable(dummy_data1)
#' dummy_data2 <- data.frame(
#' X = 6:10,
#' Y = letters[6:10]
#' )
#' dummy_dt2 <- DT::datatable(dummy_data2)
#' tabTableTwo("dummy_id", dummy_dt1, dummy_dt2)
tabTableTwo <- function(id, table_one, table_two) {
  shiny::tabPanel(
    "Tabel",
    shiny::fluidRow(
      shiny::column(
        width = 6,
        align = "center",
        shinycssloaders::withSpinner(DT::DTOutput(shiny::NS(id, table_one)))
      ),
      shiny::column(
        width = 6,
        align = "center",
        shinycssloaders::withSpinner(DT::DTOutput(shiny::NS(id, table_two)))
      )
    )
  )
}


## @title tabellenPopover function
## @description Function for creating popovers on the tabs of the tables.
## @param ... Arguments passed to other methods.
## @param tabblad A string representing the tab.
## @return A bsPopover from the spsComps package with specified content and style.
tabellenPopover <- function(..., tabblad) {

  tabblad_info <- dplyr::case_when(
    tabblad == "Table" ~ "Text table",
    tabblad == "Composition percentages" ~ "Text composition %",
    TRUE ~ "Test"
  )

  tabblad_tekst <- paste0("<br>", tabblad_info, "</br>")

  # Guard clause
  if (!requireNamespace("spsComps", quietly = TRUE)) {
    rlang::abort("The package spsComps should be installed for this funtion to work")
  }

  spsComps::bsPopover(
    tag = ...,
    title = tabblad,
    content = tabblad_tekst,
    placement = "left",
    bgcolor = "#3C8DBC",
    titlecolor = "white",
    contentcolor = "#3C8DBC",
    html = TRUE
  )
}
