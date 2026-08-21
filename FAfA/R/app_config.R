#' Access files in the current app
#'
#' NOTE: If you manually change your package name in the DESCRIPTION,
#' don't forget to change it here too, and in the config file.
#' For a safer name change mechanism, use the `golem::set_golem_name()` function.
#'
#' @param ... character vectors, specifying subdirectory and file(s)
#' within your package. The default, none, returns the root of the app.
#'
#' @noRd
app_sys <- function(...) {
  system.file(..., package = "FAfA")
}

#' Read App Config
#'
#' @param value Value to retrieve from the config file.
#' @param config Active configuration name.
#' @param use_parent Logical, scan parent directories when the file is missing.
#' @param file Location of the config file.
#'
#' @noRd
get_golem_config <- function(
  value,
  config = Sys.getenv(
    "GOLEM_CONFIG_ACTIVE",
    Sys.getenv("R_CONFIG_ACTIVE", "default")
  ),
  use_parent = TRUE,
  file = app_sys("golem-config.yml")
) {
  locate_config <- function(path) {
    if (file.exists(path) || !isTRUE(use_parent)) return(path)

    file_name <- basename(path)
    directory <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
    repeat {
      candidate <- file.path(directory, file_name)
      if (file.exists(candidate)) return(candidate)
      parent <- dirname(directory)
      if (identical(parent, directory)) break
      directory <- parent
    }
    path
  }

  parse_value <- function(text) {
    text <- trimws(sub("\\s+#.*$", "", text))
    if (grepl("^!expr\\s+", text)) {
      expression_text <- sub("^!expr\\s+", "", text)
      return(eval(parse(text = expression_text), envir = parent.frame(2)))
    }
    if (grepl("^(['\"]).*\\1$", text)) {
      return(substring(text, 2, nchar(text) - 1))
    }
    lowered <- tolower(text)
    if (lowered %in% c("yes", "true")) return(TRUE)
    if (lowered %in% c("no", "false")) return(FALSE)
    if (lowered %in% c("null", "~")) return(NULL)
    numeric_value <- suppressWarnings(as.numeric(text))
    if (!is.na(numeric_value)) return(numeric_value)
    text
  }

  config_file <- locate_config(file)
  if (!file.exists(config_file)) stop("Configuration file not found: ", file)

  lines <- readLines(config_file, warn = FALSE, encoding = "UTF-8")
  sections <- list()
  active_section <- NULL
  for (line in lines) {
    if (!nzchar(trimws(line)) || grepl("^\\s*#", line)) next
    if (grepl("^[^[:space:]][^:]*:\\s*$", line)) {
      active_section <- sub(":\\s*$", "", trimws(line))
      sections[[active_section]] <- sections[[active_section]] %||% list()
      next
    }
    if (is.null(active_section) || !grepl("^\\s+[^:]+:", line)) next

    key <- trimws(sub(":.*$", "", line))
    raw_value <- sub("^[^:]+:\\s*", "", trimws(line))
    sections[[active_section]][[key]] <- parse_value(raw_value)
  }

  selected <- sections$default %||% list()
  if (!identical(config, "default")) {
    selected <- utils::modifyList(selected, sections[[config]] %||% list())
  }
  if (!value %in% names(selected)) {
    stop("Configuration value not found: ", value)
  }
  selected[[value]]
}
