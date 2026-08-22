#' Build a URL for a Kew Tree of Life release
#'
#' @param path Optional path inside the release directory.
#' @param release Release directory. Use `"current_release"` or `"current"` for
#'   Kew's current release link.
#' @param base_url Base public URL.
#'
#' @return A character scalar URL.
#' @export
tol_release_url <- function(path = "", release = "current_release",
                            base_url = tol_base_url()) {
  release <- normalize_release(release)
  path <- normalize_remote_path(path)
  paste0(url_join(base_url, release, path), if (!nzchar(path)) "/" else "")
}

#' List files and directories in a Kew Tree of Life remote index
#'
#' @param path Directory path inside the release.
#' @param release Release directory. Defaults to Kew's current release link.
#' @param base_url Base public URL.
#'
#' @return A data frame with `name`, `path`, `url`, and `is_dir`.
#' @export
tol_index <- function(path = "", release = "current_release",
                      base_url = tol_base_url()) {
  path <- normalize_remote_path(path)
  index_url <- tol_release_url(path, release = release, base_url = base_url)
  html <- readLines(index_url, warn = FALSE)
  entries <- parse_index_entries(html)
  hrefs <- entries$href

  names <- utils::URLdecode(basename(sub("/$", "", hrefs)))
  paths <- file.path(path, hrefs)
  paths <- normalize_remote_path(paths)

  data.frame(
    name = names,
    path = paths,
    url = vapply(paths, tol_release_url, character(1), release = release,
                 base_url = base_url),
    is_dir = grepl("/$", hrefs),
    size = parse_remote_size(entries$size),
    modified = entries$modified,
    stringsAsFactors = FALSE
  )
}

parse_index_entries <- function(html) {
  pattern <- paste0(
    'href="([^"]+)">[^<]+</a>\\s+',
    '([0-9]{4}-[0-9]{2}-[0-9]{2}\\s+[0-9]{2}:[0-9]{2})\\s+',
    '([0-9.]+[KMGTP]?|-)'
  )
  matches <- regmatches(html, gregexpr(pattern, html, perl = TRUE))
  matches <- unlist(matches, use.names = FALSE)

  if (!length(matches)) {
    return(data.frame(
      href = character(),
      modified = character(),
      size = character(),
      stringsAsFactors = FALSE
    ))
  }

  entries <- lapply(matches, function(x) {
    parts <- regmatches(x, regexec(pattern, x, perl = TRUE))[[1]]
    data.frame(
      href = utils::URLdecode(parts[2]),
      modified = parts[3],
      size = parts[4],
      stringsAsFactors = FALSE
    )
  })
  entries <- do.call(rbind, entries)

  entries <- entries[!entries$href %in% c("../", "/"), , drop = FALSE]
  entries <- entries[!grepl("^https?://", entries$href), , drop = FALSE]
  entries <- entries[!grepl("^/", entries$href), , drop = FALSE]
  entries <- entries[!grepl("^\\?", entries$href), , drop = FALSE]

  rownames(entries) <- NULL
  entries
}

parse_remote_size <- function(size_text) {
  vapply(size_text, parse_one_remote_size, numeric(1))
}

parse_one_remote_size <- function(size_text) {
  if (is.na(size_text) || !nzchar(size_text) || identical(size_text, "-")) {
    return(NA_real_)
  }

  match <- regexec("^([0-9.]+)([KMGTP]?)$", size_text)
  parts <- regmatches(size_text, match)[[1]]
  if (!length(parts)) {
    return(NA_real_)
  }

  multiplier <- switch(
    parts[3],
    K = 1024,
    M = 1024^2,
    G = 1024^3,
    T = 1024^4,
    P = 1024^5,
    1
  )

  as.numeric(parts[2]) * multiplier
}
