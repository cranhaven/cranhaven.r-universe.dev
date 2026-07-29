#' Read a TiddlyWiki Table into a Data Frame
#'
#' This function parses a table written in TiddlyWiki format and converts it into an R data frame.
#' It can optionally treat the first row as a header.
#'
#' @param table A character string representing the TiddlyWiki table.
#' @param header A logical value indicating whether the first row should be treated as column headers. Default is TRUE.
#'
#' @return A data frame containing the parsed table data.
#' @examples
#' table <- "|!Cell1 |!Cell2 |\n|Cell3 |Cell4 |"
#' df <- read_table(table, header = TRUE)
#' print(df)
#'
#' @export
read_table <- function(table, header = TRUE) {
    stopifnot(is.vector(table))
    stopifnot(nchar(table) > 0)
    # Split table into lines

    lines <- strsplit(table, "\n")[[1]]

    # Remove empty lines
    lines <- lines[nzchar(lines)]
    # remove lines which don't start with |
    lines <- lines[grepl("^\\|", lines)]
    pattern <- " *\\|!? *"
    if (header) {
        headers <- (unlist(strsplit(lines[1], pattern)))
        headers <- headers[nchar(headers) > 0]
        # Extract data rows
        data_lines <- lines[-1]
    } else {
        headers <- NULL
        data_lines <- lines
    }

    # Process data
    data_list <- lapply(data_lines, function(line) {
        cells <- trimws(unlist(strsplit(line, pattern)))
        cells[cells != ""]
    })

    # Convert to data frame
    df <- as.data.frame(do.call(rbind, data_list), stringsAsFactors = FALSE)

    # Assign column names if header is TRUE
    if (header) {
        colnames(df) <- headers
    }

    return(df)
}


#' Convert Data Frame to HTML Table using kable
#'
#' @param df A data frame to be converted to an HTML table.
#' @param ... Other arguments to be passed to `knitr::kable`.
#'
#' @returns A htmltools object containing the HTML representation of the table.
#' @export
#'
#' @examples
#' kable_html(cars[1:10,])
kable_html <- function(df, ...) {
    stopifnot(is.data.frame(df))
    df |>
        knitr::kable(format = "html", ...) |>
        as.character() |>
        htmltools::HTML()
}

