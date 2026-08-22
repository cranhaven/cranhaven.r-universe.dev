manifest_paths <- c(
  "sequence_manifest.txt",
  "deleted_sequences.txt",
  "specimen_manifest.txt",
  "revised_specimen_nomenclature.txt",
  "gene_manifest.txt"
)

bundle_paths <- list(
  manifests = manifest_paths,
  species_tree = "tree/species/treeoflife.current.tree",
  species_tree_support = "tree/species/treeoflife.all_support_values.current.tree"
)

#' Known download bundles
#'
#' @return A data frame with bundle names and included remote paths.
#' @export
tol_known_bundles <- function() {
  data.frame(
    bundle = rep(names(bundle_paths), lengths(bundle_paths)),
    path = unlist(bundle_paths, use.names = FALSE),
    stringsAsFactors = FALSE
  )
}

#' Known download bundles
#'
#' Alias for [tol_known_bundles()].
#'
#' @return A data frame with bundle names and included remote paths.
#' @export
tol_known_bundle <- function() {
  tol_known_bundles()
}

#' Download files from a Kew Tree of Life release
#'
#' @param paths Character vector of file paths inside the release directory.
#' @param dest_dir Local destination directory.
#' @param release Release directory. Defaults to Kew's current release link.
#' @param overwrite Replace existing local files.
#' @param preserve_dirs Preserve the remote directory structure below
#'   `dest_dir`.
#' @param quiet Passed to [utils::download.file()].
#' @param base_url Base public URL.
#' @param timeout Download timeout in seconds.
#' @param retries Number of attempts for each file.
#' @param retry_wait Seconds to wait between attempts.
#'
#' @return A character vector with local file paths.
#' @export
tol_download <- function(paths, dest_dir = tol_data_dir(), release = "current_release",
                         overwrite = FALSE, preserve_dirs = TRUE, quiet = FALSE,
                         base_url = tol_base_url(), timeout = 600,
                         retries = 3, retry_wait = 5) {
  if (missing(paths) || !length(paths)) {
    stop(
      "`paths` must contain at least one remote file path.\n",
      "Use `tol_download_bundle()` for predefined bundles or ",
      "`tol_download_release()` to download the complete release.",
      call. = FALSE
    )
  }

  paths <- normalize_remote_path(paths)
  local_paths <- if (isTRUE(preserve_dirs)) {
    file.path(dest_dir, normalize_release(release), paths)
  } else {
    file.path(dest_dir, basename(paths))
  }

  for (i in seq_along(paths)) {
    dir.create(dirname(local_paths[i]), recursive = TRUE, showWarnings = FALSE)

    if (file.exists(local_paths[i]) && !isTRUE(overwrite)) {
      next
    }

    download_with_retry(
      url = tol_release_url(paths[i], release = release, base_url = base_url),
      destfile = local_paths[i],
      expected_size = NA_real_,
      quiet = quiet,
      timeout = timeout,
      retries = retries,
      retry_wait = retry_wait
    )
  }

  normalizePath(local_paths, winslash = "/", mustWork = FALSE)
}

#' Download a predefined Kew Tree of Life bundle
#'
#' @param bundle Bundle name. See [tol_known_bundles()].
#' @inheritParams tol_download
#'
#' @return A character vector with local file paths.
#' @export
tol_download_bundle <- function(bundle = "manifests", dest_dir = tol_data_dir(),
                                release = "current_release", overwrite = FALSE,
                                quiet = FALSE, base_url = tol_base_url(),
                                timeout = 600, retries = 3, retry_wait = 5) {
  bundle <- match.arg(bundle, names(bundle_paths))

  tol_download(
    paths = bundle_paths[[bundle]],
    dest_dir = dest_dir,
    release = release,
    overwrite = overwrite,
    quiet = quiet,
    base_url = base_url,
    timeout = timeout,
    retries = retries,
    retry_wait = retry_wait
  )
}

#' Download and validate a complete Kew Tree of Life release
#'
#' This recursively indexes the remote release, downloads every file, validates
#' local file sizes against the remote index, and writes a CSV manifest.
#'
#' @param dest_dir Local destination directory for the release contents.
#' @param path Directory path inside the release to download. Use `""` for the
#'   whole release, or paths such as `"fasta/by_gene"`.
#' @param release Release directory. Defaults to Kew's current release link.
#' @param manifest_path Local CSV manifest path.
#' @param overwrite Replace existing local files.
#' @param validate_only Only index and validate existing local files.
#' @param quiet Passed to [utils::download.file()].
#' @param base_url Base public URL.
#' @param timeout Download timeout in seconds.
#' @param retries Number of attempts for each file.
#' @param retry_wait Seconds to wait between attempts.
#'
#' @return A data frame with release id, remote paths, local paths, sizes, and
#'   validation status.
#' @export
tol_download_release <- function(dest_dir = file.path("raw-data", normalize_release(release)),
                                 path = "",
                                 release = "current_release",
                                 manifest_path = file.path("raw-data", "kew_download_manifest.csv"),
                                 overwrite = FALSE, validate_only = FALSE,
                                 quiet = FALSE, base_url = tol_base_url(),
                                 timeout = 600, retries = 3, retry_wait = 5) {
  path <- normalize_remote_path(path)
  remote_files <- recursive_index(path = path, release = release, base_url = base_url)
  release_id <- detect_release_id(remote_files$path)

  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(manifest_path), recursive = TRUE, showWarnings = FALSE)

  local_paths <- file.path(dest_dir, gsub("/", .Platform$file.sep, remote_files$path))
  status <- character(nrow(remote_files))
  local_size <- rep(NA_real_, nrow(remote_files))

  message("Release detectada: ", release_id)
  message("Archivos remotos: ", nrow(remote_files))
  message(sprintf(
    "Tamano estimado: %.2f GB",
    sum(remote_files$size, na.rm = TRUE) / 1024^3
  ))
  message("Destino: ", normalizePath(dest_dir, winslash = "/", mustWork = FALSE))

  for (i in seq_len(nrow(remote_files))) {
    status[i] <- validate_downloaded_file(local_paths[i], remote_files$size[i])

    if (!isTRUE(validate_only) && (isTRUE(overwrite) || status[i] != "ok")) {
      dir.create(dirname(local_paths[i]), recursive = TRUE, showWarnings = FALSE)
      partial_path <- paste0(local_paths[i], ".part")
      if (file.exists(partial_path)) {
        unlink(partial_path)
      }

      message("Descargando ", remote_files$path[i])
      status[i] <- download_with_retry(
        url = remote_files$url[i],
        destfile = partial_path,
        finalfile = local_paths[i],
        expected_size = remote_files$size[i],
        quiet = quiet,
        timeout = timeout,
        retries = retries,
        retry_wait = retry_wait
      )
    }

    if (file.exists(local_paths[i])) {
      local_size[i] <- file.info(local_paths[i])$size
    }
  }

  manifest <- data.frame(
    release_id = release_id,
    relative_path = remote_files$path,
    url = remote_files$url,
    remote_modified = remote_files$modified,
    remote_size = remote_files$size,
    local_path = normalizePath(local_paths, winslash = "/", mustWork = FALSE),
    local_size = local_size,
    status = status,
    stringsAsFactors = FALSE
  )

  utils::write.csv(manifest, manifest_path, row.names = FALSE, fileEncoding = "UTF-8")

  failures <- manifest[manifest$status != "ok", , drop = FALSE]
  if (nrow(failures)) {
    warning(
      "Validacion incompleta: ", nrow(failures),
      " archivo(s) no estan en estado ok. Revise `manifest_path`.",
      call. = FALSE
    )
  }

  manifest
}

#' Download and validate one directory from a Kew Tree of Life release
#'
#' Convenience wrapper around [tol_download_release()] for subdirectories such
#' as `"fasta/by_gene"`, `"fasta/alignments"`, or `"tree/species"`.
#'
#' @param path Directory path inside the release.
#' @inheritParams tol_download_release
#'
#' @return A data frame with release id, remote paths, local paths, sizes, and
#'   validation status.
#' @export
tol_download_directory <- function(path, dest_dir = file.path("raw-data", normalize_release(release)),
                                   release = "current_release",
                                   manifest_path = file.path(
                                     "raw-data",
                                     paste0("kew_download_", gsub("[^A-Za-z0-9]+", "_", normalize_remote_path(path)), ".csv")
                                   ),
                                   overwrite = FALSE, validate_only = FALSE,
                                   quiet = FALSE, base_url = tol_base_url(),
                                   timeout = 600, retries = 5, retry_wait = 10) {
  if (missing(path) || !nzchar(path)) {
    stop("`path` must be a directory inside the release, e.g. 'fasta/by_gene'.", call. = FALSE)
  }

  tol_download_release(
    dest_dir = dest_dir,
    path = path,
    release = release,
    manifest_path = manifest_path,
    overwrite = overwrite,
    validate_only = validate_only,
    quiet = quiet,
    base_url = base_url,
    timeout = timeout,
    retries = retries,
    retry_wait = retry_wait
  )
}

recursive_index <- function(path = "", release = "current_release",
                            base_url = tol_base_url()) {
  entries <- tol_index(path = path, release = release, base_url = base_url)
  files <- entries[!entries$is_dir, , drop = FALSE]
  dirs <- entries[entries$is_dir, , drop = FALSE]

  for (dir in dirs$path) {
    files <- rbind(
      files,
      recursive_index(path = dir, release = release, base_url = base_url)
    )
  }

  files
}

detect_release_id <- function(paths) {
  release_notes <- grep(
    "kew_tree_of_life_release_notes_([0-9.]+)\\.txt$",
    paths,
    value = TRUE
  )
  if (length(release_notes)) {
    return(sub(".*release_notes_([0-9.]+)\\.txt$", "\\1", release_notes[1]))
  }

  tree_files <- grep("treeoflife\\.([0-9.]+)\\.tree$", paths, value = TRUE)
  if (length(tree_files)) {
    return(sub(".*treeoflife\\.([0-9.]+)\\.tree$", "\\1", tree_files[1]))
  }

  "unknown"
}

validate_downloaded_file <- function(path, expected_size) {
  if (!file.exists(path)) {
    return("missing")
  }

  if (!is.na(expected_size) && file.info(path)$size != expected_size) {
    return("size_mismatch")
  }

  "ok"
}

download_with_retry <- function(url, destfile, expected_size = NA_real_,
                                finalfile = destfile, quiet = FALSE,
                                timeout = 600, retries = 3, retry_wait = 5) {
  old_timeout <- getOption("timeout")
  options(timeout = max(timeout, old_timeout))
  on.exit(options(timeout = old_timeout), add = TRUE)

  for (attempt in seq_len(retries)) {
    if (file.exists(destfile)) {
      unlink(destfile)
    }

    result <- try(
      utils::download.file(
        url = url,
        destfile = destfile,
        mode = "wb",
        quiet = quiet
      ),
      silent = TRUE
    )

    if (!inherits(result, "try-error")) {
      status <- validate_downloaded_file(destfile, expected_size)
      if (identical(status, "ok")) {
        if (!identical(normalizePath(destfile, winslash = "/", mustWork = FALSE),
                       normalizePath(finalfile, winslash = "/", mustWork = FALSE))) {
          if (file.exists(finalfile)) {
            unlink(finalfile)
          }
          if (!file.rename(destfile, finalfile)) {
            stop("Could not move downloaded file to final path: ", finalfile, call. = FALSE)
          }
        }
        return("ok")
      }
    }

    if (file.exists(destfile)) {
      unlink(destfile)
    }

    if (attempt < retries) {
      message("Reintentando descarga en ", retry_wait, " segundos: ", url)
      Sys.sleep(retry_wait)
    }
  }

  "download_failed"
}
