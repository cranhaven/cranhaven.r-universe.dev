
# S3 object for response ---------------------------------------------------------

as_lifx_api_response <- function(r) {

    check_lifx_response(r)

    # content <- httr::content(r, 'text',encoding = 'UTF-8') if(content==''){ content_parsed<-'' }else{ content_parsed <-
    # jsonlite::fromJSON(content, simplifyVector = FALSE) }

    class(r) <- c("lifx_api_response", class(r))
    r
}

#' @method print lifx_api_response
#' @export
print.lifx_api_response <- function(x, ...) {


  print_lifx_response_status <- function(x) {
    status <- (x$status_code)
    painted_status <- ifelse(status < 300, crayon::green(status), crayon::red(status))
    cat(painted_status)
    return(invisible(x))
  }

  print_lifx_response_content <- function(x) {

    res <- httr::content(x)
    if (length(res) == 0) {
      return(invisible(x))
    }

    if (!("results" %in% names(res))) {
      return(invisible(x))
    }

    res <- res$results
    lapply(res, function(res) {
      names <- names(res)
      values <- unlist(res)
      cat(paste0(paste0(crayon::italic(crayon::silver(paste(names, ": "))), values), collapse = "\n"))


      cat("\n\n")
    })

    return(invisible(x))
  }


    utils::str(x, no.list = T, give.head = F, give.attr = FALSE, indent.str = "  ")
    cat(crayon::italic(crayon::silver("'LIFX' api response - status ")))
    print_lifx_response_status(x)
    cat("\n")
    cat(crayon::italic(crayon::silver(paste("<LIFX ", x$url, ">\n"))))
    print_lifx_response_content(x)
    # utils::str(x$content)
    invisible(x)
}


