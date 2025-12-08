TEMP_DIR_ENV = "TEMP_DIR_ENV"

#' Internal helper function to "browse" a URL.
#'
#' @param url character url, if given and valid other
#' parameters will be ignored.
#' @param host character host to pass to plumber
#' @param port integer port to pass to plumber
#' @param browser Boolean whether to specifically launch a browser
#' @param path character path to TGVE instance
#' @param protocol character protocol, this may change
openURL = function(url = NULL,
                   host = "127.0.0.1",
                   port = 8000,
                   browser = FALSE,
                   path = "",
                   protocol = "http://") {
  if(!interactive()) return()
  # TODO: there must be better ways of doing this
  u = paste0(protocol, host,":", port, "/", path)
  if(!is.null(url) && is.character(url) && length(url) == 1) {
    u = url
  }
  viewer = getOption("viewer")
  if(identical(.Platform$GUI, "RStudio") &&
     !is.null(viewer) &&
     !browser) {
    viewer(u)
  } else {
    utils::browseURL(u)
  }
}

#' Internal helper function to:
#' 1. copy the bundled zip
#' 2. unzip
#' 3. cleanup as required.
#' TODO: return a value?
#'
#' @param path character path of TGVE instance
#' @param over.write boolean whether to cleanup the instance in `path`
copy_tgve = function(path, over.write = TRUE) {
  dir_check(path)

  d = file.path(path, "tgve")
  f = file.path(path, "tgve.zip")

  if(over.write) {
    # remove existing copy or folder named tgve
    if(dir.exists(d)) unlink(d, recursive = TRUE)
    if(file.exists(f)) file.remove(f)
  }

  m = "could not copy TGVE bundled version."
  inst.copied = file.copy(
    system.file("tgve.zip", package = "tgver"),
    path)
  if(!inst.copied)
    stop(m)
  utils::unzip(f, exdir = path)
  # keep copy of original index.html
  file.copy(file.path(d, "index.html"), file.path(d, "index.original"))
  # there is now path/tgve
  unzipped = list.files(d, pattern = "*.js|*html")
  if(length(unzipped) < 1)
    stop(m)
  # remove tgve.zip
  file.remove(f)
}

dir_check = function(path) {
  if(!is.character(path) || length(path) != 1) {
    stop("setup takes one character variable.")
  }
  if(!dir.exists(path))
    stop("destination directory does not exist.")
}

#' copy the inst/tgve to a temp in an R session
tempInstance = function() {
  temp.path = tempdir()
  # copy_instance creates a tgve folder here
  copy_tgve(temp.path)

  # a = list(temp.path)
  # names(a) = TEMP_DIR_ENV
  # do.call(Sys.setenv, a)
  file.path(temp.path, "tgve")
}

#' Package version included as data
#'
#' @name version
#' @docType data
#' @author L Hama \email{l.hama@@leeds.ac.uk}
#' @keywords data
NULL

stopifnotonecharacter = function(c, m = NA) {
  mm = " must be one character value."
  if(is.character(m) && length(m) == 1L) {
    mm = m
  }
  if(!is.character(c) || length(c) != 1L)
    stop(c, m)
}

stopifnotvalidfile = function(file.uri) {
  if(!file.exists(file.uri))
    stop("file does not exist: ", file.uri)
  fi = file.info(file.uri)
  # any sensible geojson/csv file would above 100bytes
  if(is.na(fi$size) || fi$size < 10L)
    stop("given file is empty, is it a wrong file path?")
}

#' Good enough regex to sanitize URLs
#'
#' The task of checking a URL is "hard", see this by J. Hester:
#' https://cran.r-project.org/web/packages/rex/vignettes/url_parsing.html
#' To avoid having a dependency for now, let us not use "rex"  R package.
#' To try and understand the regex please see this gist which includes
#' a breakdown of the regex:
#' https://gist.github.com/dperini/729294
#'
#'
#' @param string must be valid vector of URLs
is_valid_url = function(string) {
  regex = paste0("^(?:(?:http(?:s)?|ftp)://)(?:\\S+(?::(?:\\S)*)?@)?(?:(?:",
                 "[a-z0-9\u00a1-\uffff](?:-)*)*(?:[a-z0-9\u00a1-\uffff])+)",
                 "(?:\\.(?:[a-z0-9\u00a1-\uffff](?:-)*)*(?:[a-z0-9\u00a1-",
                 "\uffff])+)*(?:\\.(?:[a-z0-9\u00a1-\uffff]){2,})(?::(?:\\d)",
                 "{2,5})?(?:/(?:\\S)*)?$")
  # quick localhost or loop ip4
  local = grepl("(^(?:http:)?\\/\\/localhost:?(?::\\d{2,5})?)|127\\.0\\.0\\.1",
                string)
  other = grepl(regex, string[-which(local)])
  if(length(-which(local)) == 0 ) {
    other = grepl(regex, string)
    return(other)
  }
  c(local[which(local)], other)
}

#' Helper function to generate URLs
#'
#' The function can generate a URL based on a base URL and as many as TGVE API
#' variables provided to the function.
#'
#' @param base character URL defaults to `http://127.0.0.1:8000`
#' @param ... any or all of the TGVE API variables to replace/add values to.
#'
#' @return character URL generated from `base` and `tgver::`
#'
#' @examples {
#' url = get_url(dark="false")
#' url == "http://127.0.0.1:8000?dark=false"
#' url
#' url = get_url()
#' url
#' }
#'
#' @export
get_url = function(base = "http://127.0.0.1:8000", ...) {
  args = list(...)
  if(length(args) == 0L) return(base)
  args.names = names(args)
  given = intersect(args.names, names(apis))
  new.url = ifelse(endsWith(base, "?"), base, paste0(base, "?"))
  for (x in names(apis)) {
    if(x %in% given) {
      if(endsWith(new.url, "?")) {
        new.url = paste0(new.url, x, "=", args[x])
      } else {
        new.url = paste0(new.url, "&", x, "=", args[x])
      }
    }
  }
  new.url
}

isSingleString = function(input) {
  is.character(input) & length(input) == 1
}
