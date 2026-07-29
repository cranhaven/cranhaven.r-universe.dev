
.get_title <- function(text) {
    pattern <- "^title: *(.+) *$"

    pos <- grepl(pattern = pattern, text)
    if (sum(pos) != 0) {
        title <- gsub(pattern, "\\1", text[pos])
        title <- gsub("^\"", "", title)
        title <- gsub("\"$", "", title)
    } else {
        title <- NULL
    }
    title
}

.is_valid_url <- function(url) {
    if (is.null(url) || url == "") return(FALSE)
    tryCatch({
        parsed <- httr2::url_parse(url)
        return(!is.null(parsed$scheme) && !is.null(parsed$hostname))
    }, error = function(e) {
        return(FALSE)
    })
}

.pretty_link <- function(text) {
    pattern <- "\\\\\\[\\\\\\[([^]]+)\\\\\\]\\\\\\]"

    # process each element of 'text'
    sapply(text, function(line) {
        # find matches
        m <- gregexpr(pattern, line, perl = TRUE)
        parts <- regmatches(line, m)[[1]]

        # no matches → return unchanged
        if (length(parts) == 0 || parts[1] == -1) return(line)

        # process each match
        replaced <- vapply(parts, function(p) {
            inside <- sub(pattern, "\\1", p)

            # escape unescaped | 
            inside <- gsub("(?<!\\\\)\\|", "\\\\|", inside, perl = TRUE)

            paste0("[[", inside, "]]")
        }, character(1))

        # replace in line
        regmatches(line, m) <- list(replaced)
        line
    }, USE.NAMES = FALSE)
}




.markdown_link <- function(text) {
    #    pattern <- "\\\\\\[\\\\\\[([a-zA-Z0-9 |]+)\\\\\\]\\\\\\]"
    pattern <- "(\\[[a-zA-Z0-9 ]+\\]\\(#)([a-zA-Z0-9 ]+)(\\))"
    pos <- grep(pattern, text)
    f <- function(x) {
        link <- gsub(pattern, "\\2", x)
        link <- utils::URLencode(link)
        gsub(pattern, paste0("\\1", link, "\\3"), x)
    }
    text <- stringr::str_replace_all(text, pattern, function(m) sapply(m, f))
    text
}


#' Format for converting from R Markdown to another tiddler markdown
#'
#' @param host the host of tiddlywiki web server
#' @param remote whether put into remote TiddlyWiki Node.js Server
#' @param preview whether to send `open_tiddler` command to ws server (tw-livebridge) to preview in browser
#' @param tags tiddler tags
#' @param fields a named vector for tiddler fields
#' @param use_bookdown logical. Use bookdown to generate markdown file.
#' @param overwrite whether to overwrite the existing tiddler.
#' @param variant variant for md_document
#' @param pandoc_args pandoc_args for md_document
#' @param ... Other argument pass to md_document
#' @return R Markdown output format to pass to render()
#' @export
#'
#' @examples
#' \dontrun{
#' library(rmarkdown)
#' render("input.Rmd")
#' }
tiddler_document <- function(host = NULL,
                             remote = FALSE,
                             preview = FALSE,
                             tags = NULL,
                             fields = NULL,
                             use_bookdown = FALSE,
                             overwrite = FALSE,
                             variant = "gfm",
                             pandoc_args = "--wrap=none",
                             ...) {
    stopifnot(length(remote) == 1, is.logical(remote))
    stopifnot(length(preview) == 1, is.logical(preview))
    stopifnot(length(use_bookdown) == 1, is.logical(use_bookdown))
    stopifnot(length(overwrite) == 1, is.logical(overwrite))
    stopifnot(length(variant) == 1, is.character(variant))
    stopifnot(length(pandoc_args) == 1, is.character(pandoc_args))
    if (!is.null(host)) {
        stopifnot(length(host) == 1)
        stopifnot(.is_valid_url(host))
    }

    message("Previewing: ", preview)
    # Get md document
    if (use_bookdown) {
        output <- bookdown::markdown_document2(variant = variant, pandoc_args = pandoc_args, ...)
    } else {
        output <- rmarkdown::md_document(variant = variant, pandoc_args = pandoc_args, ...)
    }
    # Define post processor function
    post_processor <- function(metadata, input_file, output_file, clean, verbose) {
        # save(list = ls(), file = "tmp.Rdata")
        # stop("AAAAAAAAAAAA")
        # print(input_file)
        # print(output_file)
        # file.copy(input_file, paste0(input_file, ".bck"))
        # file.copy(output_file, paste0(output_file, ".bck"))
        title <- readLines(input_file)
        title <- .get_title(title)
        if (is.null(title)) {
            title <- tools::file_path_sans_ext(basename(output_file))
        }
        text <- readLines(output_file)
        # for macro in tiddlywiki
        text <- gsub("&lt;<", "<<", text)
        text <- gsub("&lt;&lt;", "<<", text)
        text <- gsub(">&gt;", ">>", text)
        text <- gsub("&gt;&gt;", ">>", text)
        text <- gsub("\u201C", '"', text)
        text <- gsub("\u201D", '"', text)
        text <- gsub("\\\\<", '<', text)
        text <- gsub("\\\\>", '>', text)
        text <- gsub("\\\\\\$", "\\$", text)
        # Update image file path
        pattern <- c("^(\\!\\[\\]\\()(.+\\.\\w{3})(\\)).*$",
                     "^(<img +src=\")(.+\\.\\w{3})(\".*/>)$")
        i <- 1
        for (i in seq(along = pattern)) {
            # save(list = ls(), file = "a.Rdata")
            # stop()
            # vars <- load("examples/a.Rdata")
            pos_i <- grep(pattern[i], text)
            if (length(pos_i) == 0) {
                next
            }
            j <- 1
            for (j in seq_along(pos_i)) {
                img_files_raw <- gsub(pattern[i], "\\2", text[pos_i[j]])
                #img_files_raw <- file.path("examples", img_files_raw)

                pos_img <- file.exists(img_files_raw)
                if (sum(!pos_img) > 0) {
                    stop("Image files do not exist: ",
                         paste(img_files_raw[!pos_img], collapse = ", "))
                }
                img_uri <- knitr::image_uri(img_files_raw)
                text[pos_i[j]] <- gsub(pattern[i], paste0("\\1", img_uri, "\\3"), text[pos_i[j]])
            }
        }
        # save(list = ls(), file = "a.Rdata")
        # stop()
        # vars <- load("examples/a.Rdata")
        text <- .pretty_link(text)
        text <- paste(text, collapse = "\n")
        body <- tiddler_json(title = title,
                              text = text,
                              type = "text/x-markdown",
                              tags = tags,
                              fields = fields)

        output_file <- file.path(dirname(output_file),
                                 paste0(tools::file_path_sans_ext(basename(output_file)), ".json"))
        writeLines(body, output_file)
        # Push into server if host is specified
        if (remote) {
            if (!is.null(host)) {
                tw_options(host = host)
            }
            if (!.is_valid_url(tw_options()$host)) {
                stop("Host is not a valid URL: ", tw_options()$host)
            }
            res <- get_tiddler(title = title)
            if (length(res) > 0 && !overwrite) {
                stop("Existed tiddler with title: ", title)
            } else if (length(res) > 0 && overwrite) {
                warning("Existed tiddler with title: ", title)
            }
            put_tiddler(title = title,
                        text = text,
                        type = "text/x-markdown",
                        tags = tags,
                        fields = fields)
            if (preview) {
                .send_open_tiddler(title)
            }
        }
        output_file
    }
    output$post_processor <- post_processor
    return(output)
}

.send_open_tiddler <- function(title) {
    host <- TW_OPTIONS("host")
    http_x_auth_key <- TW_OPTIONS("http_x_auth_key")
    stopifnot(length(title) == 1, is.character(title))
    host_parts <- httr2::url_parse(host)
    default_port <- ifelse(host_parts$scheme == "https", "443", "80")
    default_protocol <- ifelse(host_parts$scheme == "https", "wss", "ws")

    ws_url <- sprintf("%s://%s:%s/ws",
                      default_protocol,
                      host_parts$hostname,
                      ifelse(is.null(host_parts$port), default_port, host_parts$port))
    message("Connecting to WebSocket URL: ", ws_url)

    ws <- websocket::WebSocket$new(
        ws_url, autoConnect = FALSE,
        headers = if (nchar(http_x_auth_key) > 0) list("X-Auth-Key" = http_x_auth_key) else list()
    )

    state <- new.env(parent = emptyenv())
    state$done <- FALSE

    ws$onOpen(function(event) {
        msg <- list(type = "open-tiddler", title = title)
        ws$send(jsonlite::toJSON(msg, auto_unbox = TRUE))
        message("Sent open-tiddler command for: ", title)
        later::later(function() ws$close(), 0.1)
        state$done <- TRUE
    })

    ws$onError(function(event) {
        warning("WebSocket error: ", event$message)
        state$done <- TRUE
    })

    ws$onClose(function(event) {
        message("WebSocket closed")
        state$done <- TRUE
    })

    ws$connect()

    start <- Sys.time()
    while (!state$done && difftime(Sys.time(), start, units = "secs") < 10) {
        later::run_now(timeout = 0.1)
    }
    if (!state$done) {
        warning("WebSocket did not respond within 10 seconds")
        try(ws$close(), silent = TRUE)
    }
}
