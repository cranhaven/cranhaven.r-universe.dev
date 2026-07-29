# Generate tabs in the tiddlywiki

.unique_name <- function(prefix = "id") {
    entropy <- paste(Sys.time(), stats::runif(1), sample(1e6, 1), sep = "-")
    hash <- digest::digest(entropy, algo = "sha1")  # 40 hex characters
    paste0(prefix, "-", hash)
}

#' Generate HTML Tabs with Dynamic Content
#'
#' This function creates a tabbed interface where each tab has dynamically generated content.
#'
#' @param names A character vector of tab labels.
#' @param fun A function that generates the content for each tab. It must take an index (`i`) as the first argument.
#' @param groupname A unique string to group the radio inputs (default is generated automatically).
#' @param checked The index of the tab that should be pre-selected (default is `1`).
#' @param ... Additional arguments passed to `fun`.
#'
#' @return An `htmltools::tagList` containing the tabbed interface.
#' @examples
#' \dontrun{
#' tab_labels <- c("Tab1", "Tab2", "Tab3")
#'
#' tab_content_fun <- function(i, extra_text = "") {
#'     htmltools::tagList(
#'         htmltools::tags$p(paste("Content for tab:", tab_labels[i], extra_text)),
#'         htmltools::tags$img(src = paste0("plot_", i, ".png"), width = "100%")
#'     )
#' }
#'
#' tabs(tab_labels, tab_content_fun, checked = 2, extra_text = "Additional details")
#' }
#' @export
tabs <- function(names, fun, groupname = .unique_name(), checked = 1, ...) {
    if (!is.character(names) || length(names) == 0) {
        stop("`names` must be a non-empty character vector.")
    }

    if (!is.function(fun)) {
        stop("`fun` must be a function that generates HTML content.")
    }

    if (!is.character(groupname) || length(groupname) != 1) {
        stop("`groupname` must be a single string.")
    }

    if (!is.numeric(checked) || length(checked) != 1 || checked < 1 || checked > length(names)) {
        stop("`checked` must be a single numeric value between 1 and the number of tabs.")
    }

    buttons <- c()
    contents <- c()

    for (i in seq_along(names)) {
        id <- paste0(groupname, "-", names[i])
        is_checked <- if (i == checked) "checked" else NULL

        buttons_i <- htmltools::tagList(
            htmltools::tags$input(type = "radio", id = id, name = groupname, checked = is_checked),
            htmltools::tags$label(`for` = id, names[i])
        )
        buttons <- c(buttons, buttons_i)

        content_i <- fun(i, ...)
        content_i <- htmltools::tagList(
            htmltools::tags$div(content_i)
        )
        contents <- c(contents, content_i)
    }

    # Combine everything
    res <- htmltools::div(
        class = "tabs",
        htmltools::div(class = "tab-buttons tc-tab-buttons", buttons),
        htmltools::div(class = "tab-content", contents)
    )
    return(structure(
        res,
        class = c("tw_html", "tw_tabs", class(res))
    ))
}



#' Create HTML Tabs for Multiple Objects
#'
#' This function generates a tabbed HTML interface for displaying multiple R objects
#' such as `ggplot2` plots and data frames. Each tab can include custom arguments
#' for rendering (e.g., image width or table formatting).
#'
#' @param ... Named arguments where each name defines a tab label. Each argument
#'   can be either:
#'   \itemize{
#'     \item A single object (`ggplot`, `data.frame`, etc.)
#'     \item A list with an `object` element and additional named arguments passed to the relevant rendering function.
#'   }
#'
#' @return A `htmltools::tag` object representing the full tab interface. Can be
#' printed in R Markdown documents or displayed interactively in RStudio Viewer.
#'
#' @details
#' Supported object types:
#' \itemize{
#'   \item `ggplot` objects — rendered as base64-encoded images using `save_base64()`
#'   \item `data.frame` or `tibble` — rendered using `kable_html()`
#' }
#'
#' Additional arguments passed inside the list are used by the relevant rendering function.
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#'
#' # Simple plot and table
#' p <- ggplot(cars) + geom_point(aes(speed, dist))
#' df <- head(cars)
#'
#' # Basic usage
#' create_tabs(
#'   plot = p,
#'   table = df
#' )
#'
#' # With custom rendering arguments
#' create_tabs(
#'   plot = list(object = p, width = 4),
#'   table = list(object = df, digits = 2)
#' )
#' }
#'
#' @export
create_tabs <- function(...) {
    tabs_input <- list(...)
    tab_names <- names(tabs_input)

    if (is.null(tab_names) || any(tab_names == "")) {
        stop("All tabs must be named.")
    }
    i <- 1

    tabs_fun <- function(i, tabs_input) {
        tab_i <- tabs_input[[i]]

        # If tab_i is not a list, treat it as a simple object
        if (!("list" %in% class(tab_i))) {
            tab_i <- list(object = tab_i)
        }

        object <- tab_i$object
        args <- tab_i[setdiff(names(tab_i), "object")]

        class_i <- class(object)

        if ("ggplot" %in% class_i) {
            # Default width if not provided
            img <- do.call(save_base64, c(list(object), args))
            return(htmltools::tags$img(src = img, width = "100%"))
        } else if ("data.frame" %in% class_i || "tbl_df" %in% class_i) {
            tbl <- do.call(kable_html, c(list(object), args))
            return(tbl)
        } else {
            stop("Unsupported object type: ", paste(class_i, collapse = ", "))
        }
    }

    htmls <- tabs(tab_names, tabs_fun, tabs_input = tabs_input)
    return(htmls)
}

