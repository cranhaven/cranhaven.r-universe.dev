# ---- Internal helpers --------------------------------------------------------

#' Format bytes to human-readable size
#' @param bytes Numeric size in bytes.
#' @return Character string like "1.23 GB".
#' @noRd
.hf_format_size <- function(bytes) {
  if (is.na(bytes) || bytes < 0) return("unknown")
  units <- c("B", "KB", "MB", "GB", "TB")
  if (bytes == 0) return("0 B")
  i <- min(floor(log(bytes, 1024)), length(units) - 1L)
  paste(round(bytes / 1024^i, 2), units[i + 1L])
}

#' Parse quantization tag from GGUF filename
#' @param filename Character filename.
#' @return Character quantization string or NA.
#' @noRd
.hf_parse_quant <- function(filename) {
  m <- regmatches(filename, regexpr(
    "[Qq][0-9]+(?:_[A-Za-z0-9]+)*", filename
  ))
  if (length(m) == 0L || nchar(m) == 0L) NA_character_ else toupper(m)
}

#' GET JSON from HF API
#' @param url Character URL.
#' @param token Character bearer token or NULL.
#' @param user_agent Character User-Agent header or NULL.
#' @return Parsed JSON (list).
#' @noRd
.hf_api_get <- function(url, token = NULL, user_agent = NULL) {
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)

  hdrs <- c(
    if (!is.null(token) && nzchar(token)) c("Authorization" = paste("Bearer", token)),
    if (!is.null(user_agent) && nzchar(user_agent)) c("User-Agent" = user_agent)
  )

  status <- tryCatch(
    utils::download.file(url, tmp, method = "libcurl", quiet = TRUE,
                         headers = hdrs),
    error = function(e) e
  )

  if (inherits(status, "error")) {
    stop("HF API request failed for ", url, ": ", conditionMessage(status),
         call. = FALSE)
  }

  jsonlite::fromJSON(tmp, simplifyVector = TRUE)
}

#' Download a file from HF with progress
#' @param url Character URL.
#' @param destfile Character destination path.
#' @param token Character bearer token or NULL.
#' @return Invisible NULL; file written to destfile.
#' @noRd
.hf_download_file <- function(url, destfile, token = NULL) {
  hdrs <- character()
  if (!is.null(token) && nzchar(token)) {
    hdrs <- c("Authorization" = paste("Bearer", token))
  }

  status <- utils::download.file(
    url, destfile, method = "libcurl", mode = "wb",
    headers = hdrs
  )
  if (status != 0L) {
    stop("Download failed (status ", status, ") for ", url, call. = FALSE)
  }
  invisible(NULL)
}

#' Validate GGUF magic number
#' @param path Character path to file.
#' @return TRUE if valid, otherwise throws error.
#' @noRd
.validate_gguf <- function(path) {
  con <- file(path, "rb")
  on.exit(close(con), add = TRUE)
  magic <- readBin(con, "raw", n = 4L)
  # GGUF magic: 0x47 0x47 0x55 0x46  ("GGUF")
  expected <- as.raw(c(0x47, 0x47, 0x55, 0x46))
  if (!identical(magic, expected)) {
    stop("File is not a valid GGUF file: ", path, call. = FALSE)
  }
  TRUE
}

#' Resolve HF token
#' @param token User-supplied token or NULL.
#' @return Character token or NULL.
#' @noRd
.hf_resolve_token <- function(token) {
  if (!is.null(token) && nzchar(token)) return(token)
  env_token <- Sys.getenv("HF_TOKEN", unset = "")
  if (nzchar(env_token)) return(env_token)
  NULL
}

#' Simple glob-to-regex converter (case-insensitive)
#' @param pattern Glob pattern.
#' @return Compiled regex pattern.
#' @noRd
.glob_to_regex <- function(pattern) {
  # Escape regex metacharacters except * and ?
  p <- gsub("([.+^${}()|\\[\\]\\\\])", "\\\\\\1", pattern)
  p <- gsub("\\*", ".*", p)
  p <- gsub("\\?", ".", p)
  paste0("^", p, "$")
}

# ---- Public functions --------------------------------------------------------

#' Get the cache directory for downloaded models
#'
#' Returns the path to the directory where models downloaded from
#' Hugging Face are cached. The directory is created if it does not exist.
#'
#' @return A character string containing the absolute path to the cache
#'   directory. The path follows the R user directory convention via
#'   \code{\link[tools]{R_user_dir}}.
#' @export
#' @examples
#' llama_hf_cache_dir()
llama_hf_cache_dir <- function() {
  d <- file.path(tools::R_user_dir("llamaR", "cache"), "models")
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
  d
}

#' List GGUF files in a Hugging Face repository
#'
#' Queries the Hugging Face API for GGUF model files in the specified
#' repository. Returns a data frame with file names, sizes, and detected
#' quantization levels.
#'
#' @param repo_id Character. Hugging Face repository in \code{"org/repo"} format,
#'   e.g. \code{"TheBloke/Llama-2-7B-GGUF"}.
#' @param token Character or \code{NULL}. Hugging Face API token.
#'   If \code{NULL}, uses the \code{HF_TOKEN} environment variable.
#' @param pattern Character or \code{NULL}. Optional glob pattern to filter
#'   results (e.g. \code{"*q4_k_m*"}). Case-insensitive.
#' @return A data frame with columns:
#'   \describe{
#'     \item{filename}{Character. The file name within the repository.}
#'     \item{size}{Numeric. File size in bytes.}
#'     \item{size_pretty}{Character. Human-readable file size.}
#'     \item{quant}{Character. Detected quantization level (e.g. "Q4_K_M")
#'       or \code{NA} if not detected.}
#'   }
#' @export
#' @examples
#' \donttest{
#' files <- llama_hf_list("TheBloke/Llama-2-7B-GGUF")
#' print(files)
#' }
llama_hf_list <- function(repo_id, token = NULL, pattern = NULL) {
  stopifnot(is.character(repo_id), length(repo_id) == 1L, grepl("/", repo_id))
  token <- .hf_resolve_token(token)

  url <- paste0("https://huggingface.co/api/models/", repo_id)
  info <- .hf_api_get(url, token)

  siblings <- info$siblings
  if (is.null(siblings) || !is.data.frame(siblings)) {
    stop("No files found in repository: ", repo_id, call. = FALSE)
  }

  fnames <- siblings$rfilename
  sizes <- if ("size" %in% names(siblings)) siblings$size else rep(NA_real_, length(fnames))

  # Filter to GGUF files
  is_gguf <- grepl("\\.gguf$", fnames, ignore.case = TRUE)
  fnames <- fnames[is_gguf]
  sizes <- sizes[is_gguf]

  if (length(fnames) == 0L) {
    stop("No GGUF files found in repository: ", repo_id, call. = FALSE)
  }

  # Apply optional pattern filter
  if (!is.null(pattern)) {
    rx <- .glob_to_regex(pattern)
    keep <- grepl(rx, fnames, ignore.case = TRUE)
    fnames <- fnames[keep]
    sizes <- sizes[keep]
  }

  data.frame(
    filename = fnames,
    size = sizes,
    size_pretty = vapply(sizes, .hf_format_size, character(1L)),
    quant = vapply(fnames, .hf_parse_quant, character(1L),
                   USE.NAMES = FALSE),
    stringsAsFactors = FALSE
  )
}

#' Download a GGUF model from Hugging Face
#'
#' Downloads a GGUF model file from a Hugging Face repository. Files are
#' cached locally so subsequent calls return the cached path without
#' re-downloading.
#'
#' Exactly one of \code{filename}, \code{pattern}, or \code{tag} must be
#' specified to identify which file to download.
#'
#' @param repo_id Character. Hugging Face repository in \code{"org/repo"} format.
#' @param filename Character or \code{NULL}. Exact filename to download.
#' @param pattern Character or \code{NULL}. Glob pattern for filename matching
#'   (case-insensitive). If multiple files match, an error is thrown listing
#'   the matches.
#' @param tag Character or \code{NULL}. Ollama-style tag. First tries the
#'   Ollama manifest API; on failure, falls back to pattern matching
#'   with \code{*\{tag\}*}.
#' @param token Character or \code{NULL}. Hugging Face API token.
#'   If \code{NULL}, uses the \code{HF_TOKEN} environment variable.
#' @param cache_dir Character or \code{NULL}. Custom cache directory.
#'   Defaults to \code{\link{llama_hf_cache_dir}()}.
#' @param revision Character. Git revision (branch/tag/commit). Defaults to
#'   \code{"main"}.
#' @param force Logical. If \code{TRUE}, re-download even if cached.
#'   Defaults to \code{FALSE}.
#' @return A character string containing the absolute path to the downloaded
#'   (or cached) GGUF model file.
#' @export
#' @examples
#' \dontrun{
#' path <- llama_hf_download("TheBloke/Llama-2-7B-GGUF",
#'                           pattern = "*q2_k*")
#' print(path)
#' }
llama_hf_download <- function(repo_id,
                              filename = NULL,
                              pattern = NULL,
                              tag = NULL,
                              token = NULL,
                              cache_dir = NULL,
                              revision = "main",
                              force = FALSE) {
  stopifnot(is.character(repo_id), length(repo_id) == 1L, grepl("/", repo_id))
  token <- .hf_resolve_token(token)

  # Exactly one selector must be specified
  selectors <- c(!is.null(filename), !is.null(pattern), !is.null(tag))
  if (sum(selectors) != 1L) {
    stop("Specify exactly one of 'filename', 'pattern', or 'tag'.",
         call. = FALSE)
  }

  # Resolve filename
  if (!is.null(tag)) {
    filename <- .hf_resolve_tag(repo_id, tag, token)
  } else if (!is.null(pattern)) {
    filename <- .hf_resolve_pattern(repo_id, pattern, token)
  }

  # Build cache path
  if (is.null(cache_dir)) cache_dir <- llama_hf_cache_dir()
  parts <- strsplit(repo_id, "/")[[1L]]
  dest_dir <- file.path(cache_dir, parts[1L], parts[2L], revision)
  if (!dir.exists(dest_dir)) dir.create(dest_dir, recursive = TRUE)
  dest_file <- file.path(dest_dir, filename)
  meta_file <- paste0(dest_file, ".json")

  # Return cached file if present

  if (file.exists(dest_file) && !force) {
    message("Using cached: ", dest_file)
    return(dest_file)
  }

  # Download to temp file, then rename
  download_url <- paste0("https://huggingface.co/", repo_id,
                         "/resolve/", revision, "/", filename)
  message("Downloading: ", filename, " from ", repo_id)
  tmp <- tempfile(tmpdir = dest_dir, fileext = ".part")
  on.exit(unlink(tmp), add = TRUE)

  .hf_download_file(download_url, tmp, token)
  .validate_gguf(tmp)

  file.rename(tmp, dest_file)
  on.exit(NULL)  # cancel unlink of tmp

  # Write metadata
  meta <- list(
    url = download_url,
    size = file.info(dest_file)$size,
    downloaded_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    repo_id = repo_id,
    filename = filename,
    revision = revision
  )
  writeLines(jsonlite::toJSON(meta, auto_unbox = TRUE, pretty = TRUE),
             meta_file)

  message("Saved to: ", dest_file)
  dest_file
}

#' Resolve tag to filename via Ollama manifest or pattern fallback
#' @noRd
.hf_resolve_tag <- function(repo_id, tag, token) {
  # Try Ollama manifest API first
  manifest_url <- paste0("https://huggingface.co/v2/", repo_id,
                         "/manifests/", tag)
  manifest <- tryCatch(
    .hf_api_get(manifest_url, token, user_agent = "llama-cpp"),
    error = function(e) NULL
  )

  if (!is.null(manifest) && !is.null(manifest$ggufFile) &&
      !is.null(manifest$ggufFile$rfilename)) {
    return(manifest$ggufFile$rfilename)
  }

  # Fallback: treat tag as pattern
  .hf_resolve_pattern(repo_id, paste0("*", tag, "*"), token)
}

#' Resolve glob pattern to a single filename
#' @noRd
.hf_resolve_pattern <- function(repo_id, pattern, token) {
  files <- llama_hf_list(repo_id, token = token, pattern = pattern)
  if (nrow(files) == 0L) {
    stop("No GGUF files matching '", pattern, "' in ", repo_id,
         call. = FALSE)
  }
  if (nrow(files) > 1L) {
    listing <- paste("  -", files$filename,
                     paste0("(", files$size_pretty, ")"),
                     collapse = "\n")
    stop("Multiple GGUF files match '", pattern, "' in ", repo_id,
         ":\n", listing, "\nPlease specify a more precise pattern or use 'filename'.",
         call. = FALSE)
  }
  files$filename
}

#' Load a model directly from Hugging Face
#'
#' Convenience function that downloads a GGUF model from Hugging Face (if not
#' already cached) and loads it via \code{\link{llama_load_model}}.
#'
#' @param repo_id Character. Hugging Face repository in \code{"org/repo"} format.
#' @param ... Additional arguments passed to \code{\link{llama_hf_download}}
#'   (e.g. \code{pattern}, \code{cache_dir}, \code{force}).
#' @param n_gpu_layers Integer. Number of layers to offload to GPU.
#'   Use \code{-1L} for all layers. Defaults to \code{0L} (CPU only).
#' @return An external pointer to the loaded model, as returned by
#'   \code{\link{llama_load_model}}.
#' @export
#' @examples
#' \dontrun{
#' model <- llama_load_model_hf("TheBloke/Llama-2-7B-GGUF",
#'                               pattern = "*q2_k*")
#' }
llama_load_model_hf <- function(repo_id, ..., n_gpu_layers = 0L) {
  path <- llama_hf_download(repo_id, ...)
  llama_load_model(path, n_gpu_layers = n_gpu_layers)
}

#' Show information about the model cache
#'
#' Lists all cached model files with their sizes and download metadata.
#'
#' @param cache_dir Character or \code{NULL}. Cache directory to inspect.
#'   Defaults to \code{\link{llama_hf_cache_dir}()}.
#' @return A data frame with columns:
#'   \describe{
#'     \item{repo_id}{Character. The Hugging Face repository identifier.}
#'     \item{filename}{Character. The model file name.}
#'     \item{size}{Numeric. File size in bytes.}
#'     \item{size_pretty}{Character. Human-readable file size.}
#'     \item{path}{Character. Absolute path to the cached file.}
#'     \item{downloaded_at}{Character. Timestamp of when the file was downloaded.}
#'   }
#'   Returns an empty data frame with the same columns if the cache is empty.
#' @export
#' @examples
#' llama_hf_cache_info()
llama_hf_cache_info <- function(cache_dir = NULL) {
  if (is.null(cache_dir)) cache_dir <- llama_hf_cache_dir()

  empty <- data.frame(
    repo_id = character(), filename = character(),
    size = numeric(), size_pretty = character(),
    path = character(), downloaded_at = character(),
    stringsAsFactors = FALSE
  )

  gguf_files <- list.files(cache_dir, pattern = "\\.gguf$",
                           recursive = TRUE, full.names = TRUE)
  if (length(gguf_files) == 0L) return(empty)

  rows <- lapply(gguf_files, function(f) {
    meta_file <- paste0(f, ".json")
    sz <- file.info(f)$size

    if (file.exists(meta_file)) {
      meta <- tryCatch(jsonlite::fromJSON(meta_file), error = function(e) list())
    } else {
      meta <- list()
    }

    data.frame(
      repo_id = if (!is.null(meta$repo_id)) meta$repo_id else NA_character_,
      filename = basename(f),
      size = sz,
      size_pretty = .hf_format_size(sz),
      path = f,
      downloaded_at = if (!is.null(meta$downloaded_at)) meta$downloaded_at else NA_character_,
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, rows)
}

#' Clear the model cache
#'
#' Removes cached model files. Can clear the entire cache or only files
#' from a specific repository.
#'
#' @param repo_id Character or \code{NULL}. If specified, only remove cached
#'   files from this repository. If \code{NULL}, clear the entire cache.
#' @param confirm Logical. If \code{TRUE} (default), ask for confirmation
#'   before deleting files in interactive sessions.
#' @param cache_dir Character or \code{NULL}. Cache directory to clear.
#'   Defaults to \code{\link{llama_hf_cache_dir}()}.
#' @return Invisible \code{NULL}. Called for its side effect of deleting
#'   cached files.
#' @export
#' @examples
#' \donttest{
#' llama_hf_cache_clear(confirm = FALSE)
#' }
llama_hf_cache_clear <- function(repo_id = NULL, confirm = TRUE,
                                 cache_dir = NULL) {
  if (is.null(cache_dir)) cache_dir <- llama_hf_cache_dir()

  if (!is.null(repo_id)) {
    parts <- strsplit(repo_id, "/")[[1L]]
    target_dir <- file.path(cache_dir, parts[1L], parts[2L])
    if (!dir.exists(target_dir)) {
      message("No cached files for ", repo_id)
      return(invisible(NULL))
    }
    files <- list.files(target_dir, recursive = TRUE, full.names = TRUE)
  } else {
    target_dir <- cache_dir
    files <- list.files(cache_dir, recursive = TRUE, full.names = TRUE)
  }

  if (length(files) == 0L) {
    message("Cache is empty.")
    return(invisible(NULL))
  }

  if (confirm && interactive()) {
    total_size <- sum(file.info(files)$size, na.rm = TRUE)
    msg <- paste0("Delete ", length(files), " file(s) (",
                  .hf_format_size(total_size), ")? [y/N] ")
    ans <- readline(msg)
    if (!tolower(ans) %in% c("y", "yes")) {
      message("Cancelled.")
      return(invisible(NULL))
    }
  }

  unlink(target_dir, recursive = TRUE)
  message("Cache cleared.")
  invisible(NULL)
}
