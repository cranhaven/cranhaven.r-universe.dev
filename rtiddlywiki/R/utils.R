#' Generate tiddler in json format
#'
#' @param tiddler A list for new tiddler
#'
#' @return New tiddler in json format
#' @export
tiddler_json2 <- function(tiddler) {
    stopifnot(length(tiddler$type) == 1)
    stopifnot(tiddler$type %in% c(
        "text/vnd.tiddlywiki",
        "text/x-tiddlywiki",
        "text/x-markdown",
        "text/html",
        "text/plain",
        "application/json"
    ))
    if (!is.null(tiddler$tags)) {
        if (!is.vector(tiddler$tags)) {
            stop("tags should be a vector.")
        }
        if (length(tiddler$tags) < 1) {
            stop("tags should have at least one value.")
        }
        tiddler$tags <- paste(paste0("[[", tiddler$tags, "]]"), collapse = " ")
    }
    if (is.null(tiddler$title) || length(tiddler$title) != 1 || !is.character(tiddler$title)) {
        stop("title should be string with one item.")
    }
    if (is.null(tiddler$text)) {
        stop("text should have values")
    }


    if (!is.null(tiddler$fields)) {
        stopifnot(is.list(tiddler$fields))
        f_names <- names(tiddler$fields)
        if (is.null(f_names) | sum(nchar(f_names) == 0) > 0) {
            stop("fields should be a named vector")
        }

        # Treat vector as list
        for (i in seq(along = tiddler$fields)) {
            field_i <- tiddler$fields[[i]]
            if (!is.atomic(field_i)) {
                stop("An atomic is required.")
            }
            if (length(field_i) > 1) {
                field_i <- ifelse(!grepl(" ", field_i), field_i, paste0("[[", field_i, "]]"))
                field_i <- paste(field_i, collapse = " ")
            }
            tiddler$fields[[f_names[i]]] <- jsonlite::unbox(as.character(field_i))
        }
    }
    for (i in seq(along = tiddler)) {
        if (names(tiddler)[i] != "fields") {
            tiddler[[i]] <- jsonlite::unbox(tiddler[[i]])
        }
    }
    return(tiddler)
    jsonlite::toJSON(tiddler,
        auto_unbox = FALSE,
        null = "null", pretty = TRUE
    )
}



#' Generate tiddler in json format
#'
#' @param title tiddler title
#' @param text tiddler text
#' @param type tiddler type
#' @param tags a vector for tiddler tags
#' @param fields a named vector for tiddler fields.
#' @param format export format as json or list
#'
#' @return New tiddler in json format
#' @export
tiddler_json <- function(title, text,
                         type = c(
                             "text/vnd.tiddlywiki",
                             "text/x-tiddlywiki",
                             "text/x-markdown",
                             "text/html",
                             "text/plain",
                             "application/json"
                         ),
                         tags = NULL, fields = NULL,
                         format = c("json", "list")) {
    type <- match.arg(type, several.ok = FALSE)
    format <- match.arg(format, several.ok = FALSE)
    if (!is.null(tags)) {
        if (!is.vector(tags)) {
            stop("tags should be a vector.")
        }
        if (length(tags) < 1) {
            stop("tags should have at least one value.")
        }
        tags <- paste(paste0("[[", tags, "]]"), collapse = " ")
    }
    if (!is.null(fields)) {
        stopifnot(is.list(fields))
        f_names <- names(fields)
        if (is.null(f_names) | sum(nchar(f_names) == 0) > 0) {
            stop("fields should be a named vector")
        }
    }
    if (is.null(title) || length(title) != 1 || !is.character(title)) {
        stop("title should be string with one item.")
    }
    if (is.null(text)) {
        stop("text should have values")
    }
    text <- paste(text, collapse = "\r\n")
    body <- list(
        title = jsonlite::unbox(title),
        text = jsonlite::unbox(text),
        type = jsonlite::unbox(type),
        # created = jsonlite::unbox(created),
        # created = jsonlite::unbox(created),
        tags = jsonlite::unbox(tags)
    )
    if (!is.null(fields)) {
        f_names <- names(fields)
        if (is.null(f_names) | sum(nchar(f_names) == 0) > 0) {
            stop("fields should be a named list")
        }
        # Treat vector as list
        for (i in seq(along = fields)) {
            field_i <- fields[[i]]
            if (length(field_i) > 1) {
                field_i <- ifelse(!grepl(" ", field_i), field_i, paste0("[[", field_i, "]]"))
                field_i <- paste(field_i, collapse = " ")
            }
            body$fields[[f_names[i]]] <- jsonlite::unbox(as.character(field_i))
        }
    }

    if (format == "list") {
        return(body)
    }

    jsonlite::toJSON(body,
        auto_unbox = FALSE,
        null = "null", pretty = TRUE
    )
}


#' Split tiddlywiki field into values
#'
#' @param s a string
#'
#' @return an vector of values
#' @export
split_field <- function(s) {
    if (is.null(s)) {
        return(NULL)
    }
    stopifnot(length(s) == 1)
    if (nchar(s) == 0) {
        return(NULL)
    }

    s <- stringi::stri_trim_both(s)
    v <- c()
    s_i <- s
    while (TRUE) {
        pos_s <- 1
        spliter <- " "
        if (grepl("^\\[\\[", s_i)) {
            spliter <- "\\]\\]( |$)"
            pos_s <- 3
        }
        pos_n <- stringi::stri_locate_first_regex(s_i, spliter)

        pos_n <- as.numeric(pos_n)
        if (sum(is.na(pos_n)) > 0) {
            pos_n <- nchar(s_i)
        } else {
            pos_n <- pos_n[1] - 1
        }
        # print(pos_n)
        v_i <- substr(s_i, pos_s, pos_n)
        #        print(v_i)
        v <- c(v, v_i)
        pos_s <- pos_n + pos_s + 1
        if (pos_s > nchar(s_i)) {
            break
        }
        s_i <- substr(s_i, pos_s, nchar(s_i))
        # print(s_i)
        if (nchar(s_i) == 0) {
            break
        }
    }
    v
}



#' Convert data.frame into table of TiddlyWiki
#'
#' @param df data.frame object
#' @param collapse an optional character string to separate the results.
#'
#' @returns character string for table in TiddlyWiki
#' @export
#'
#' @examples
#' cars |>
#'     dplyr::slice(1:10) |>
#'     tw_table()
tw_table <- function(df, collapse = "\n") {
    stopifnot(is.data.frame(df))
    k_table <- knitr::kable(df, escape = TRUE)
    k_table <- k_table[-2]
    k_table <- gsub("&#124;", "|", k_table)
    paste(k_table, collapse = collapse)
}
