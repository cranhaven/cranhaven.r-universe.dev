#' Open static TGVE instance
#'
#' This is the main and most basic function to run an instance of TGVE without
#' back-end. Compared to \link[tgver]{tgve_server}, this function only uses
#' internal functions to setup an instance then opens the entry HTML file.
#' If a path of an instance is provided it opens it, otherwise creates an
#' instance from a `tempdir`.
#'
#' @param path character directory of a current instance to browse. Defaults to
#' `TEMP_PATH_ENV` environment variable.
#' @param browse boolean to decide whether to browse the instance or not.
#' @param remote boolean whether to run a remote instance of TGVE. If TRUE
#' the above `path` and `browse` parameters will be ignored. Defaults to `FALSE`
#' @param url if `remote` is true, then this will be used as the parameter
#' to pass to internal function `openURL`. It defaults, for convenience,
#' to `https://tgve.github.io/app/` instance.
#'
#' @return directory of the new instance if `path` is not provided.
#'
#' @examples \donttest{
#' tgve()
#' # just get the path of the HTML
#' p = tgve(browse = FALSE)
#' file.exists(p)
#' }
#'
#' @export
tgve = function(path = Sys.getenv("TEMP_path_ENV"),
                browse = TRUE,
                remote = FALSE,
                url = 'https://tgve.github.io/app/') {
  if(remote && !is.null(url)) {
    if(!is_valid_url(url)) stop("Invalid URL")
    message("attemping to browse: ", url)
    openURL(url)
  } else {
    new.path = path
    if(!dir.exists(path)) {
      new.path = tempInstance()
    }

    new.path = file.path(new.path, "index.html")

    if(!browse) {
      return(new.path)
    }

    message("attemping to browse: ", new.path)
    openURL(new.path)
  }
}
