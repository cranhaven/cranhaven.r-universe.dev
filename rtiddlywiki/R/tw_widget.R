
#' Create a tiddlywiki widget from htmlwidget
#'
#' @param widget an object of htmlwidget
#' @param is_cat whether to show results on screen
#'
#' @return a new tiddlywiki widget
#' @export
#'
#' @examples
#' library(leaflet)
#' \dontrun{
#' content <- paste(sep = "<br/>",
#'                  "<b><a href='http://www.samurainoodle.com'>Samurai Noodle</a></b>",
#'                  "606 5th Ave. S",
#'                  "Seattle, WA 98138"
#' )
#'
#' widget <- leaflet() %>% addTiles() %>%
#'     addPopups(-122.327298, 47.597131, content,
#'                   options = popupOptions(closeButton = FALSE)
#'     )
#' tw_widget(widget)
#' }
tw_widget <- function(widget, is_cat = FALSE) {

    # If rending a markdown file
    # Return the original widget if it is not tiddler_document
    if (!is.null(knitr::opts_knit$get('rmarkdown.pandoc.to'))) {
        fmt <- rmarkdown::default_output_format(knitr::current_input())
        if (fmt$name != "rtiddlywiki::tiddler_document") {
            return(widget)
        }
    }

    w_class <- class(widget)
    if (!("htmlwidget" %in% w_class)) {
        stop("Not a htmlwidget object")
    }

    if (length(w_class) != 2) {
        stop("Expect with two classes")
    }

    w_type <- w_class[w_class != "htmlwidget"]

    w_class[w_class == "htmlwidget"] <- "html-widget"

    # Save to html file
    temp_file <- tempfile(fileext = "html")
    htmlwidgets::saveWidget(widget = widget, file = temp_file, selfcontained = FALSE)
    temp_doc <- rvest::read_html(temp_file)
    xpath <- paste(sprintf("contains(@class, '%s')", w_class), collapse = " and ")
    xpath <- sprintf("//div[%s]", xpath)
    h_node <- rvest::html_element(temp_doc, xpath = xpath)
    h_id <- rvest::html_attr(h_node, "id")
    xpath <- sprintf("//script[@data-for='%s' and @type='application/json']", h_id)
    script_node <- rvest::html_element(temp_doc, xpath = xpath)
    json <- rvest::html_text(script_node)
    new_widget <- sprintf('<$htmlwidgets type="%s" uuid="%s" data="""%s"""/>',
                         w_type, h_id, json)
    file.remove(temp_file)

    # A temp solution to replace double quotation into single quotation
    new_widget <- gsub("\\\\\"", "'", new_widget)

    if (is_cat) {
        cat(new_widget)
    }

    # Export raw html if rending rmarkdown
    if (!is.null(knitr::opts_knit$get('rmarkdown.pandoc.to'))) {
        fmt <- rmarkdown::default_output_format(knitr::current_input())

        if (fmt$name == "rtiddlywiki::tiddler_document") {
            new_widget <- knitr::raw_html(new_widget)
            return(new_widget)
        }
    }

    new_widget
}
