#' Get all tiddlers
#'
#' @param filter filter identifying tiddlers to be returned (optional, defaults to "[all[tiddlers]!is[system]sort[title]]")
#' @param exclude comma delimited list of fields to excluded from the returned tiddlers (optional, defaults to "text")
#' @param recipe string defining which recipe to read from (optional, defaults to "default")
#' @return all tiddlers information in JSON format
#' @export
#' @examples
#' \dontrun{
#' #' Get all tiddlers
#' get_tiddlers()
#' }
get_tiddlers <- function(filter = NULL,
                         exclude = NULL,
                         recipe = TW_OPTIONS("recipe")) {
    query <- list()
    if (!is.null(filter)) {
        query$filter = filter
    }

    if (!is.null(exclude)) {
        query$exclude = exclude
    }


    response <- request("GET", paste0('/recipes/', recipe, '/tiddlers.json'), query = query)
    response

}


#' Get a tiddler
#'
#' @param title  title of the tiddler to retrieve
#' @param recipe string defining which recipe to read from (optional, defaults to "default")
#' @return tiddler information in JSON format
#' @export
#' @examples
#' \dontrun{
#' get_tiddler("GettingStarted")
#' }
get_tiddler <- function(title, recipe = TW_OPTIONS("recipe")) {
    response <- request("GET", paste0('/recipes/', recipe, '/tiddlers/',
                                          title))
    if (length(response) == 0) {
        return(NULL)
    }
    # tags
    response$tags <- split_field(response$tags)
    response
}


#' Put a tiddler
#'
#' @param title tiddler title
#' @param text tiddler text
#' @param type tiddler type
#' @param tags tiddler tags which is merged with existing tags
#' @param fields a named vector for tiddler fields which is merged with existing tags
#' @param recipe string defining which recipe to write to (optional, defaults to "default")
#' @return No return value
#' @export
#' @examples
#' \dontrun{
#' title <- "New tiddler"
#' text <- c("!! Section",
#'           "This is a new tiddler")
#' type <- "text/vnd.tiddlywiki"
#' tags <- c("Tag1", "Tag 2")
#' fields <- c("F1" = "V1", "F2" = "V2")
#' put_tiddler(title = title,
#'             text = text,
#'             type = type,
#'             tags = tags,
#'             fields = fields)
#' }
put_tiddler <- function(title, text,
                        type = c("text/vnd.tiddlywiki",
                                 "text/x-tiddlywiki",
                                 "text/x-markdown",
                                 "text/html",
                                 "text/plain",
                                 "application/json"),
                        tags = NULL,
                        fields = NULL,
                        recipe = TW_OPTIONS("recipe")) {
    stopifnot(length(title) == 1)
    stopifnot(is.character(title))
    stopifnot(length(recipe) == 1)
    stopifnot(is.character(recipe))
    if (!is.null(tags)) {
        stopifnot(is.vector(tags))
        stopifnot(is.character(tags))
    }

    type <- match.arg(type)
    # Check existing tiddler
    old_tiddler <- get_tiddler(title)
    if (!is.null(old_tiddler)) {
        if (missing(text)) {
            if (is.null(old_tiddler$text)) {
                text <- ""
            } else {
                text <- old_tiddler$text
            }
        }
        if (missing(type)) {
            type <- old_tiddler$type
        }
        if (is.null(tags)) {
            tags <- old_tiddler$tags
        }
        if (!is.null(old_tiddler$fields)) {
            fields <- utils::modifyList(old_tiddler$fields, as.list(fields))
        }
    } else {
        old_tiddler <- list(type = type)
    }
    new_tiddler <- list(title = title, text = text, type = type, tags = tags,
                        fields = fields)
    new_tiddler <- utils::modifyList(old_tiddler, new_tiddler)
    .put_tiddler(new_tiddler, recipe)
    return(invisible())
}



#' Delete a tiddler
#'
#' @param title  title of the tiddler to retrieve
#' @param bag string defining which recipe to write to (optional, defaults to "default")
#' @return no return values
#' @export
#' @examples
#' \dontrun{
#' delete_tiddler("GettingStarted")
#' }
delete_tiddler <- function(title, bag = TW_OPTIONS("bag")) {
    response <- request("DELETE",
                        path = paste0('/bags/', bag, '/tiddlers/', title))
    return(invisible())
}

#' Remove fields from tiddlers
#'
#' @param title tiddler title
#' @param fields fields to remove
#' @param recipe string defining which recipe to write to (optional, defaults to "default")
#'
#' @return no return value
#' @export
remove_fields <- function(title, fields, recipe = TW_OPTIONS("recipe")) {
    stopifnot(length(title) == 1)
    stopifnot(is.character(title))
    stopifnot(is.character(fields))
    stopifnot(length(fields) > 0)

    old_tiddler <- get_tiddler(title)
    if (is.null(old_tiddler)) {
        stop("Cannot find tiddler ", title)
    }
    i <- 1
    for (i in seq(along = fields)) {
        if (is.null(old_tiddler$fields[[fields[i]]])) {
            stop("Cannot find field ", fields[i])
        }
        old_tiddler$fields[[fields[i]]] <- NULL
    }
    .put_tiddler(old_tiddler, recipe)
    return(invisible())
}

.put_tiddler <- function(new_tiddler, recipe = TW_OPTIONS("recipe")) {
    #new_tiddler$modified <- format(as.POSIXct(Sys.time(), tz = "UTC"), "%Y%m%d%H%M%S000")
    #if (is.null(new_tiddler$created)) {
    #    new_tiddler$created <- new_tiddler$modified
    #}
    if (is.null(new_tiddler$text)) {
        new_tiddler$text <- ""
    } else if (length(new_tiddler$text) > 1) {
        new_tiddler$text <- paste(new_tiddler$text, collapse = "\n")
    }
    body <- tiddler_json2(new_tiddler)
    response <- request("PUT",
                        path = paste0('/recipes/', recipe, '/tiddlers/', new_tiddler$title),
                        body = body)
    response
}


#' Get server status
#'
#' @return a list of service status
#' @export
get_status <- function() {
    status <- request("GET", "status")
    status
}
