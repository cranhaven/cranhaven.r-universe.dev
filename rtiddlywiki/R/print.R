

#' Print tw_html object
#'
#' @param x a `tw_html` object.
#' @param ... additional arguments passed to or from other methods.
#'
#' @returns no return
#' @export
print.tw_html <- function(x, ...) {
    class_x <- class(x)

    head <- list()
    if ("tw_tabs" %in% class_x) {
        css_path <- system.file("css", "tabs.css", package = "rtiddlywiki")
        if (!file.exists(css_path)) {
            stop("CSS file for tabs not found in package.")
        }
        css_content <- paste(readLines(css_path), collapse = "\n")
        style_tag <- htmltools::tags$style(htmltools::HTML(css_content))
        head <- list(style_tag)
    }

    page <- htmltools::tags$html(
        htmltools::tags$head(
            head
        ),
        htmltools::tags$body(
            x
        )
    )

    print(htmltools::browsable(page), ...)

    invisible(x)
}
