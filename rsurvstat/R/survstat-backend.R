#' Check for supported curl version
#'
#' @returns boolean (+/- warning)
#' @keywords internal
#'
#' @unit
#' .check_curl()
.check_curl = function() {
  v1 = curl::curl_version()$version
  v = as.numeric(unlist(strsplit(v1, ".", fixed = TRUE)))
  if (
    v[1] < 7 ||
      (v[1] == 7 && v[2] < 88) ||
      (v[1] == 7 && v[2] == 88 && v[3] < 1)
  ) {
    warning(
      "Your version of cURL (",
      v1,
      ") is not supported. It must be at least 7.88.1"
    )
    return(FALSE)
  } else {
    return(TRUE)
  }
}

.get_request = function(
  command = c(
    "GetAllDimensions",
    "GetAllHierarchies",
    "GetAllHierarchyExistsMembers",
    "GetAllHierarchyMembers",
    "GetAllMeasures",
    "GetCubeInfo",
    "GetOlapData",
    "GetOlapResultData"
  ),
  cube = c("SurvStat", "EvStat", "SurvStat73"),
  language = c("English", "German"),
  ...,
  column_hierarchy = NULL,
  measure = NULL,
  filters = NULL,
  row_hierarchy = NULL
) {
  command = match.arg(command)
  cube = match.arg(cube)
  language = match.arg(language)
  templatedir = system.file("templates", package = "rsurvstat")
  templatefile = fs::path(templatedir, command) %>% fs::path_ext_set(".xml")
  template = readLines(templatefile, warn = FALSE)
  template = paste0(template, collapse = "\n")
  partialfiles = fs::dir_ls(templatedir, glob = "*.partial")
  partials = list()
  for (partialfile in partialfiles) {
    partial = readLines(partialfile, warn = FALSE)
    partial = paste0(partial, collapse = "\n")
    partialname = fs::path_file(partialfile) %>% fs::path_ext_remove()
    partials[[partialname]] = partial
  }
  data = rlang::list2(...)
  data$command = command
  data$cube = cube
  data$language = language
  if (!is.null(measure)) {
    data$measure = measure
  }
  if (!is.null(column_hierarchy)) {
    data$column_hierarchy = column_hierarchy
  }
  if (!is.null(filters)) {
    data$filters = list(filter = filters)
  }
  if (!is.null(row_hierarchy)) {
    data$row_hierarchy = row_hierarchy
  }
  request = whisker::whisker.render(template, data, partials)
  return(request)
}


# .get_template = function(disease, measure, age_group) {
#   # Encoding(disease) <- "UTF-8"
#   # disease = iconv(disease, "UTF-8", "latin1")
#
#   body = template %>%
#     stringr::str_replace(
#       stringr::fixed("{{disease}}"),
#       disease
#     ) %>%
#     stringr::str_replace(
#       stringr::fixed("{{measure}}"),
#       measure
#     ) %>%
#     stringr::str_replace(
#       stringr::fixed("{{age_group}}"),
#       age_group
#     )
#   return(body)
# }

.cache_settings = new.env(parent = environment())

#' Delete all cached `SurvStat` requests
#'
#' This function is only intended to be used interactively. The cache can be
#' controlled with `set_cache_settings()`
#'
#' @param confirm can be set to TRUE to make function non interactive.
#' @returns nothing. called for side effects
#' @export
#' @concept cache
#'
#' @examples
#' cache_clear( confirm = interactive() )
cache_clear = function(confirm = utils::askYesNo("Are you sure?")) {
  if (isTRUE(confirm)) {
    dir = .get_cache_dir()
    fs::file_delete(dir)
    fs::dir_create(dir)
    message("rsurvstat results cache cleared.")
  }
  invisible(NULL)
}

#' Set options for the `rsurvstat` cache
#'
#' By default successful requests to `SurvStat` are cached for 7 days to prevent
#' repeated querying of the service. This is stored in the usual R package cache
#' location by default (e.g. `"~/.cache/rsurvstat"` on mac / linux). Caching can
#' be switched off altogether.
#'
#' @param ... you can also submit the settings as a named list.
#' @param active boolean (optional), set to FALSE to disable caching
#' @param dir file path (optional), the location of the cache
#' @param stale numeric (optional), the number of days before a cached item is
#'   considered out of date
#'
#' @returns the old cache settings as a list
#' @export
#' @concept cache
#'
#' @examples
#' old_settings = set_cache_settings(active = FALSE)
#' set_cache_settings(old_settings)
set_cache_settings = function(
  ...,
  active = NULL,
  dir = NULL,
  stale = NULL
) {
  old = list(
    active = .cache_settings$active,
    dir = .cache_settings$dir
  )
  dots = rlang::list2(...)
  if (length(dots) > 0 && is.list(dots[[1]])) {
    new = dots[[1]]
  } else {
    new = list()
  }

  if (!is.null(new$active)) {
    .cache_settings$active = new$active
  } else if (!is.null(active)) {
    .cache_settings$active = active
  } else if (is.null(.cache_settings$active)) {
    .cache_settings$active = TRUE
  }

  if (!is.null(new$dir)) {
    .cache_settings$dir = new$dir
  } else if (!is.null(dir)) {
    .cache_settings$dir = dir
  } else if (is.null(.cache_settings$dir)) {
    .cache_settings$dir = rappdirs_user_cache_dir("rsurvstat")
  }

  if (!is.null(new$stale)) {
    .cache_settings$stale = new$stale
  } else if (!is.null(stale)) {
    .cache_settings$stale = stale
  } else if (is.null(.cache_settings$stale)) {
    .cache_settings$stale = 7
  }

  return(invisible(old))
}

.get_cache_dir = function() {
  if (is.null(.cache_settings$dir)) {
    set_cache_settings()
  }
  return(.cache_settings$dir)
}

.get_cache_active = function() {
  if (is.null(.cache_settings$active)) {
    set_cache_settings()
  }
  return(.cache_settings$active)
}

.get_cache_stale = function() {
  if (is.null(.cache_settings$stale)) {
    set_cache_settings()
  }
  return(.cache_settings$stale)
}

.get_cached = function(req_hash, quiet) {
  if (!.get_cache_active()) {
    return(NULL)
  }
  stale = .get_cache_stale()

  dir = .get_cache_dir()
  fs::dir_create(dir)
  cached = fs::dir_info(dir)
  lapply(seq_along(cached$path), function(i) {
    age = as.numeric(Sys.time() - cached$modification_time[i])
    if (age > stale * 24) unlink(cached$path[i])
  })
  file = fs::path(dir, req_hash, ext = "xml")
  if (fs::file_exists(file)) {
    if (!quiet) {
      message("Using cached SurvStat data.")
    }
    return(readLines(file))
  }
  return(NULL)
}

# Caches a response based on hash of request
.set_cache = function(req_hash, res) {
  dir = .get_cache_dir()
  file = fs::path(dir, req_hash, ext = "xml")
  writeLines(res, file)
}

#
.do_survstat_command = function(request, command = NULL, quiet = FALSE) {
  if (!.check_curl()) {
    return(NULL)
  }

  if (!quiet) {
    message("Making SurvStat request... ", appendLF = FALSE)
  }

  req_hash = rlang::hash(request)
  cached_res = .get_cached(req_hash, quiet)
  if (!is.null(cached_res)) {
    cached_res = xml2::read_xml(cached_res)
    return(cached_res)
  }

  # httr2 version

  # req = httr2::request(
  #   "https://tools.rki.de/SurvStat/SurvStatWebService.svc"
  # ) %>%
  #   httr2::req_headers(
  #     Accept = "text/xml",
  #     Accept = "multipart/*"
  #     # if (is.null(command)) {
  #     #   NULL
  #     # } else {
  #     #   Action = sprintf(
  #     #     '"http://tools.rki.de/SurvStat/SurvStatWebService/%s"',
  #     #     command
  #     #   )
  #     # }
  #   ) %>%
  #   httr2::req_body_raw(
  #     body = request,
  #     type = "application/soap+xml;charset=utf-8"
  #   )
  #
  # resp = tryCatch(
  #   req %>% httr2::req_perform(),
  #   error = function(e) {
  #     stop("SSL version: ", curl::curl_version()$version, "\n", e)
  #   }
  # )
  #
  # if (!quiet) {
  #   message("Data downloaded.")
  # }
  # res = resp %>% httr2::resp_body_string()

  # httr alternative

  # cert = system.file("certs", "rki-ca-bundle.pem", package = "rsurvstat")

  resp = tryCatch(
    httr::POST(
      "https://tools.rki.de/SurvStat/SurvStatWebService.svc",
      config = c(
        httr::accept_xml(),
        httr::content_type("application/soap+xml;charset=utf-8"),
        httr::accept("multipart/*"),
        httr::config(
          # cainfo = cert,
          ssl_verifypeer = TRUE,
          ssl_verifyhost = 2,
          sslversion = 6,
          http_version = 1
        )
        # httr::config("verbose" = 3)
      ),
      body = request
    ),
    error = function(e) {
      stop("SSL version: ", curl::curl_version()$version, "\n", e)
    }
  )

  if (!quiet) {
    message("Data downloaded.")
  }
  res = resp %>% httr::content(as = "text")

  # process result

  .set_cache(req_hash, res)
  res = xml2::read_xml(res)
  return(res)
}
