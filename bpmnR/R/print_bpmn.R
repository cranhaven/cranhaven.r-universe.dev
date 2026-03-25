# ============================== MAIN FUNCTION =================================
#' Print xml part of bpmn
#'
#' @param x A bpmn object from create_bpmn function
#' @param ... Any additional arguments
#'
#'
#' @author Alessio Nigro
#'
#' @import htmltools
#' @importFrom DT datatable
#' @importFrom huxtable as_hux
#' @importFrom huxtable everywhere
#' @importFrom huxtable print_screen
#' @importFrom huxtable set_all_borders
#' @importFrom huxtable set_all_border_colors
#' @importFrom huxtable set_all_padding
#' @importFrom huxtable set_caption
#' @importFrom huxtable set_caption_pos
#' @importFrom huxtable set_header_rows
#' @importFrom huxtable set_outer_padding
#' @importFrom huxtable style_headers
#'
#'
#' @return No return value, only print model.
#' @rdname print
#' @export
print_bpmn <- function(x, ...) {
  # Defines options used to omit whitespace that would normally be written around HTML tags
  noWS <-
    c("before",
      "after",
      "outside",
      "after-begin",
      "before-end",
      "inside")

  # Defines table number for more structured output
  table_number <- 1

  # Prints BPMN object in the console and in an htmlwidget pane
  for (bpmn_element in names(x)) {
    if (bpmn_element != "xml") {
      # Prints nicely formatted tables of BPMN elements in the console
      as_hux(x[[bpmn_element]]) %>%
        set_all_padding(4) %>%
        set_outer_padding(0) %>%
        set_all_borders(TRUE) %>%
        set_all_border_colors(1, everywhere, "darkcyan") %>%
        set_header_rows(1, TRUE) %>%
        style_headers(bold = TRUE,
                      italic = TRUE,
                      text_color = "dodgerblue3") %>%
        set_caption(paste0("Table ", table_number, ": ", "BPMN ", bpmn_element)) %>%
        set_caption_pos("topcenter") %>%
        print_screen()

      # Prints HTML tables of BPMN elements in an htmlwidget pane
      # (which provides filtering, pagination, sorting, and other features in the tables)
      print(
        datatable(
          x[[bpmn_element]],
          options = list(dom = "lfrtip", lengthMenu = list(
            list(10, 25, 50, -1), list(10, 25, 50, "All")
          )),
          caption = tags$caption(
            style = "caption-side: bottom; text-align: center; padding-top: 10px",
            paste0("Table ", table_number, ": "),
            em("BPMN ",
               strong(bpmn_element, .noWS = noWS),
               ".",
               .noWS = noWS)
          ),
          filter = "top",
          editable = "cell"
        )
      )
      cat("\n")

      # Increases table number by one
      table_number <- table_number + 1
    }
  }
}
