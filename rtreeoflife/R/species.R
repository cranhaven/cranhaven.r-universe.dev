#' Read the Kew Tree of Life species index
#'
#' @param file Local path to `all_species_list.csv`. If `NULL`, the package
#'   dataset [tol_species] is returned.
#' @param use_package_data Use the built-in [tol_species] dataset when
#'   `file = NULL`.
#'
#' @return A data frame with normalized column names and derived fields.
#' @export
tol_species_index <- function(file = NULL, use_package_data = TRUE) {
  if (is.null(file) && isTRUE(use_package_data)) {
    return(tibble::as_tibble(tol_species))
  }

  if (is.null(file)) {
    file <- file.path("raw-data", "all_species_list.csv")
  }

  if (!file.exists(file)) {
    stop("Species index file does not exist: ", file, call. = FALSE)
  }

  index <- utils::read.csv(
    file,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    na.strings = c("", "NA")
  )
  names(index) <- clean_column_names(names(index))

  required <- c(
    "sequence_id", "data_source", "order", "family", "genus",
    "specific_epithet", "fasta_file_url"
  )
  missing <- setdiff(required, names(index))
  if (length(missing)) {
    stop(
      "Species index is missing required columns: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  index$sequence_id <- as.integer(index$sequence_id)
  if ("collection_date" %in% names(index)) {
    index$collection_date <- suppressWarnings(as.integer(index$collection_date))
  }
  if ("no_of_genes_recovered" %in% names(index)) {
    index$no_of_genes_recovered <- suppressWarnings(as.integer(index$no_of_genes_recovered))
  }
  if ("no_of_bp_recovered" %in% names(index)) {
    index$no_of_bp_recovered <- suppressWarnings(as.integer(index$no_of_bp_recovered))
  }

  index$scientific_name <- trimws(paste(index$genus, index$specific_epithet))
  index$fasta_file_name <- basename(index$fasta_file_url)

  tibble::as_tibble(index)
}

#' Search Kew Tree of Life species records
#'
#' @param index A species index returned by [tol_species_index()]. If omitted,
#'   the built-in [tol_species] dataset is used.
#' @param sequence_id Optional sequence identifier or vector of identifiers.
#' @param order,family,genus,specific_epithet,scientific_name Optional taxonomic
#'   filters.
#' @param query Optional free-text query.
#' @param ignore_case Ignore case in text filters.
#'
#' @return A filtered data frame.
#' @export
tol_search_species <- function(index = tol_species_index(), sequence_id = NULL,
                               order = NULL, family = NULL, genus = NULL,
                               specific_epithet = NULL, scientific_name = NULL,
                               query = NULL, ignore_case = TRUE) {
  out <- index

  if (!is.null(sequence_id)) {
    out <- out[out$sequence_id %in% as.integer(sequence_id), , drop = FALSE]
  }
  out <- filter_text_column(out, "order", order, ignore_case)
  out <- filter_text_column(out, "family", family, ignore_case)
  out <- filter_text_column(out, "genus", genus, ignore_case)
  out <- filter_text_column(out, "specific_epithet", specific_epithet, ignore_case)
  out <- filter_text_column(out, "scientific_name", scientific_name, ignore_case)

  if (!is.null(query) && nzchar(query)) {
    query_value <- if (ignore_case) tolower(query) else query
    searchable <- c(
      "scientific_name", "order", "family", "genus", "specific_epithet",
      "specimen_reference", "specimen_barcode"
    )
    searchable <- intersect(searchable, names(out))
    keep <- rep(FALSE, nrow(out))
    for (column in searchable) {
      values <- if (ignore_case) tolower(out[[column]]) else out[[column]]
      values[is.na(values)] <- ""
      keep <- keep | grepl(query_value, values, fixed = TRUE)
    }
    out <- out[keep, , drop = FALSE]
  }

  tibble::as_tibble(out)
}

#' Match requested species names against the Kew Tree of Life index
#'
#' @param species Character vector of requested scientific names.
#' @param index A species index returned by [tol_species_index()]. If omitted,
#'   the built-in [tol_species] dataset is used.
#' @param fuzzy If `TRUE`, return the closest available name when no exact match
#'   is found.
#' @param max_distance Maximum edit distance for fuzzy matching. If `NULL`, a
#'   conservative threshold is computed from each requested name.
#' @param multiple How to handle multiple records for the same matched species.
#'   `"all"` returns all records; `"best"` keeps the record with the highest
#'   `no_of_genes_recovered` and then `no_of_bp_recovered`.
#' @param ignore_case Ignore case when matching names.
#'
#' @return A tibble with requested names, match status, matched records, and
#'   FASTA URLs where available.
#' @export
tol_match_species <- function(species, index = tol_species_index(), fuzzy = FALSE,
                              max_distance = NULL,
                              multiple = c("all", "best"),
                              ignore_case = TRUE) {
  multiple <- match.arg(multiple)
  if (missing(species) || !length(species)) {
    stop("`species` must contain at least one scientific name.", call. = FALSE)
  }

  requested <- trimws(as.character(species))
  requested <- requested[nzchar(requested)]
  if (!length(requested)) {
    stop("`species` must contain at least one non-empty scientific name.", call. = FALSE)
  }

  if (!"scientific_name" %in% names(index)) {
    stop("`index` must contain a `scientific_name` column.", call. = FALSE)
  }

  rows <- lapply(seq_along(requested), function(i) {
    match_one_species(
      requested_name = requested[i],
      request_order = i,
      index = index,
      fuzzy = fuzzy,
      max_distance = max_distance,
      multiple = multiple,
      ignore_case = ignore_case
    )
  })

  dplyr::bind_rows(rows)
}

#' Resolve FASTA download targets for selected species records
#'
#' @param records Species records returned by [tol_species_index()] or
#'   [tol_search_species()].
#' @param dest_dir Local directory where FASTA files should be stored. If
#'   `NULL`, a session temporary directory is used.
#'
#' @return A data frame describing FASTA URLs and local paths.
#' @export
tol_resolve_fasta <- function(records, dest_dir = NULL) {
  if (missing(records) || !is.data.frame(records)) {
    stop("`records` must be a data frame returned by tol_species_index() or tol_search_species().", call. = FALSE)
  }
  if (!"fasta_file_url" %in% names(records)) {
    stop("`records` must contain a `fasta_file_url` column.", call. = FALSE)
  }

  dest_dir <- resolve_fasta_dest_dir(dest_dir)
  fasta_file_name <- basename(records$fasta_file_url)
  local_path <- file.path(dest_dir, fasta_file_name)
  status <- ifelse(file.exists(local_path), "ok", "missing")

  tibble::tibble(
    sequence_id = records$sequence_id,
    scientific_name = records$scientific_name,
    order = records$order,
    family = records$family,
    genus = records$genus,
    specific_epithet = records$specific_epithet,
    fasta_file_url = records$fasta_file_url,
    fasta_file_name = fasta_file_name,
    local_path = normalizePath(local_path, winslash = "/", mustWork = FALSE),
    status = status
  )
}

#' Download FASTA files for selected species records
#'
#' @param records Species records returned by [tol_species_index()] or
#'   [tol_search_species()]. If omitted, filters are applied to
#'   [tol_species_index()].
#' @param dest_dir Local directory where FASTA files should be stored. If
#'   `NULL`, files are downloaded to a session temporary directory.
#' @param manifest_path Optional CSV manifest path. Use `NULL` to skip writing.
#'   If `dest_dir = NULL`, no manifest is written unless this is explicitly set.
#' @param overwrite Replace existing local FASTA files.
#' @param validate_only Report local status without downloading.
#' @param quiet Passed to [utils::download.file()].
#' @param timeout Download timeout in seconds.
#' @param retries Number of attempts for each file.
#' @param retry_wait Seconds to wait between attempts.
#' @param ... Filters passed to [tol_search_species()] when `records` is omitted.
#'
#' @return A data frame with one row per requested FASTA.
#' @export
tol_download_fasta <- function(records = NULL,
                               dest_dir = NULL,
                               manifest_path = if (is.null(dest_dir)) NULL else file.path("raw-data", "kew_fasta_download_manifest.csv"),
                               overwrite = FALSE, validate_only = FALSE,
                               quiet = FALSE, timeout = 600, retries = 3,
                               retry_wait = 5, ...) {
  if (is.null(records)) {
    records <- tol_search_species(tol_species_index(), ...)
  }
  if (!nrow(records)) {
    stop("No species records were selected for FASTA download.", call. = FALSE)
  }

  dest_dir <- resolve_fasta_dest_dir(dest_dir)
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  if (!is.null(manifest_path)) {
    dir.create(dirname(manifest_path), recursive = TRUE, showWarnings = FALSE)
  }

  plan <- tol_resolve_fasta(records, dest_dir = dest_dir)

  for (i in seq_len(nrow(plan))) {
    plan$status[i] <- validate_downloaded_file(plan$local_path[i], NA_real_)

    if (!isTRUE(validate_only) && (isTRUE(overwrite) || plan$status[i] != "ok")) {
      partial_path <- paste0(plan$local_path[i], ".part")
      message("Descargando ", plan$scientific_name[i], ": ", plan$fasta_file_name[i])
      plan$status[i] <- download_with_retry(
        url = plan$fasta_file_url[i],
        destfile = partial_path,
        finalfile = plan$local_path[i],
        expected_size = NA_real_,
        quiet = quiet,
        timeout = timeout,
        retries = retries,
        retry_wait = retry_wait
      )
    }
  }

  if (!is.null(manifest_path)) {
    utils::write.csv(plan, manifest_path, row.names = FALSE, fileEncoding = "UTF-8")
  }

  tibble::as_tibble(plan)
}

#' Save downloaded FASTA files to a permanent directory
#'
#' @param plan A data frame returned by [tol_download_fasta()] or
#'   [tol_resolve_fasta()].
#' @param dest_dir Permanent destination directory.
#' @param overwrite Replace files that already exist in `dest_dir`.
#'
#' @return A copy of `plan` with updated `local_path` and `status`.
#' @export
tol_save_fasta <- function(plan, dest_dir = file.path("raw-data", "fasta", "by_recovery"),
                           overwrite = FALSE) {
  if (missing(plan) || !is.data.frame(plan)) {
    stop("`plan` must be a data frame returned by tol_download_fasta() or tol_resolve_fasta().", call. = FALSE)
  }
  required <- c("local_path", "fasta_file_name")
  missing <- setdiff(required, names(plan))
  if (length(missing)) {
    stop("`plan` is missing required columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }

  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  saved <- plan

  for (i in seq_len(nrow(saved))) {
    source_path <- saved$local_path[i]
    target_path <- file.path(dest_dir, saved$fasta_file_name[i])

    if (!file.exists(source_path)) {
      saved$status[i] <- "missing"
      saved$local_path[i] <- normalizePath(target_path, winslash = "/", mustWork = FALSE)
      next
    }

    if (!file.exists(target_path) || isTRUE(overwrite)) {
      file.copy(source_path, target_path, overwrite = overwrite)
    }

    saved$local_path[i] <- normalizePath(target_path, winslash = "/", mustWork = FALSE)
    saved$status[i] <- if (file.exists(target_path)) "ok" else "copy_failed"
  }

  tibble::as_tibble(saved)
}

#' Export downloaded FASTA files and a manifest
#'
#' @param plan A data frame returned by [tol_download_fasta()] or
#'   [tol_resolve_fasta()].
#' @param dest_dir Permanent destination directory for FASTA files.
#' @param manifest_path Optional CSV manifest path. If `NULL`, no manifest is
#'   written.
#' @param overwrite Replace files that already exist in `dest_dir`.
#'
#' @return A tibble with copied file paths and export status.
#' @export
tol_export_fasta <- function(plan, dest_dir = file.path("raw-data", "fasta", "by_recovery"),
                             manifest_path = file.path(dest_dir, "fasta_export_manifest.csv"),
                             overwrite = FALSE) {
  exported <- tol_save_fasta(plan, dest_dir = dest_dir, overwrite = overwrite)
  names(exported)[names(exported) == "status"] <- "export_status"

  if (!is.null(manifest_path)) {
    dir.create(dirname(manifest_path), recursive = TRUE, showWarnings = FALSE)
    utils::write.csv(exported, manifest_path, row.names = FALSE, fileEncoding = "UTF-8")
  }

  exported
}

#' Read a FASTA file
#'
#' @param file Local FASTA file.
#' @param as Output format. `"data.frame"` returns one row per sequence with
#'   `header`, `sequence`, and `width`; `"list"` returns a named character
#'   vector; `"text"` returns raw file lines.
#'
#' @return A data frame, named character vector, or character vector.
#' @export
tol_read_fasta <- function(file, as = c("data.frame", "list", "text")) {
  as <- match.arg(as)
  if (!file.exists(file)) {
    stop("FASTA file does not exist: ", file, call. = FALSE)
  }

  lines <- readLines(file, warn = FALSE)
  if (identical(as, "text")) {
    return(lines)
  }

  headers_at <- grep("^>", lines)
  if (!length(headers_at)) {
    return(tibble::tibble(header = character(), sequence = character(), width = integer()))
  }

  starts <- headers_at
  ends <- c(headers_at[-1] - 1L, length(lines))
  headers <- sub("^>", "", lines[starts])
  sequences <- character(length(starts))

  for (i in seq_along(starts)) {
    sequence_lines <- lines[(starts[i] + 1L):ends[i]]
    sequence_lines <- sequence_lines[!grepl("^\\s*$", sequence_lines)]
    sequences[i] <- paste0(sequence_lines, collapse = "")
  }

  if (identical(as, "list")) {
    stats::setNames(sequences, headers)
  } else {
    tibble::tibble(
      header = headers,
      sequence = sequences,
      width = nchar(sequences)
    )
  }
}

#' Attach parsed FASTA data to a download plan
#'
#' @param plan A data frame returned by [tol_download_fasta()],
#'   [tol_resolve_fasta()], or [tol_save_fasta()].
#' @param column Name of the list-column to create.
#' @param as FASTA representation passed to [tol_read_fasta()].
#' @param missing What to store for missing files. `"empty"` stores an empty
#'   data frame/list/text value; `"error"` stops on the first missing file.
#'
#' @return A data frame with a FASTA list-column.
#' @export
tol_attach_fasta <- function(plan, column = "fasta",
                             as = c("data.frame", "list", "text"),
                             missing = c("empty", "error")) {
  as <- match.arg(as)
  missing <- match.arg(missing)

  if (missing(plan) || !is.data.frame(plan)) {
    stop("`plan` must be a data frame returned by a FASTA helper.", call. = FALSE)
  }
  if (!"local_path" %in% names(plan)) {
    stop("`plan` must contain a `local_path` column.", call. = FALSE)
  }

  attached <- plan
  attached[[column]] <- lapply(attached$local_path, function(file) {
    if (!file.exists(file)) {
      if (identical(missing, "error")) {
        stop("FASTA file does not exist: ", file, call. = FALSE)
      }
      return(empty_fasta_value(as))
    }
    tol_read_fasta(file, as = as)
  })

  tibble::as_tibble(attached)
}

clean_column_names <- function(x) {
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  x
}

filter_text_column <- function(data, column, values, ignore_case = TRUE) {
  if (is.null(values)) {
    return(data)
  }
  if (!column %in% names(data)) {
    return(data[FALSE, , drop = FALSE])
  }

  haystack <- data[[column]]
  needle <- values
  if (ignore_case) {
    haystack <- tolower(haystack)
    needle <- tolower(needle)
  }
  haystack[is.na(haystack)] <- ""

  data[haystack %in% needle, , drop = FALSE]
}

match_one_species <- function(requested_name, request_order, index, fuzzy,
                              max_distance, multiple, ignore_case) {
  requested_key <- normalize_species_name(requested_name, ignore_case = ignore_case)
  index_key <- normalize_species_name(index$scientific_name, ignore_case = ignore_case)

  matched <- index[index_key == requested_key, , drop = FALSE]
  match_type <- "exact"
  distance <- 0L

  if (!nrow(matched) && isTRUE(fuzzy)) {
    unique_names <- unique(index$scientific_name)
    unique_keys <- normalize_species_name(unique_names, ignore_case = ignore_case)
    distances <- utils::adist(requested_key, unique_keys, ignore.case = FALSE)
    best <- which.min(distances)
    distance <- as.integer(distances[best])
    threshold <- max_distance
    if (is.null(threshold)) {
      threshold <- max(2L, floor(nchar(requested_key) * 0.12))
    }

    if (length(best) && is.finite(distance) && distance <= threshold) {
      matched_name <- unique_names[best]
      matched <- index[index$scientific_name == matched_name, , drop = FALSE]
      match_type <- "fuzzy"
    }
  }

  if (!nrow(matched)) {
    return(unmatched_species_row(requested_name, request_order, index))
  }

  matched <- select_species_records(matched, multiple = multiple)
  matched_name <- matched$scientific_name

  tibble::as_tibble(matched) |>
    dplyr::mutate(
      requested_name = requested_name,
      request_order = request_order,
      matched_name = matched_name,
      match_type = match_type,
      match_distance = distance,
      has_data = !is.na(.data$sequence_id) & !is.na(.data$fasta_file_url),
      .before = 1
    )
}

select_species_records <- function(records, multiple = "all") {
  if (identical(multiple, "all") || nrow(records) <= 1L) {
    return(records)
  }

  records |>
    dplyr::arrange(
      dplyr::desc(.data$no_of_genes_recovered),
      dplyr::desc(.data$no_of_bp_recovered)
    ) |>
    dplyr::slice_head(n = 1)
}

unmatched_species_row <- function(requested_name, request_order, index) {
  template <- index[NA_integer_, , drop = FALSE]
  template <- template[1, , drop = FALSE]

  tibble::as_tibble(template) |>
    dplyr::mutate(
      requested_name = requested_name,
      request_order = request_order,
      matched_name = NA_character_,
      match_type = "none",
      match_distance = NA_integer_,
      has_data = FALSE,
      .before = 1
    )
}

normalize_species_name <- function(x, ignore_case = TRUE) {
  x <- trimws(gsub("\\s+", " ", x))
  if (isTRUE(ignore_case)) {
    x <- tolower(x)
  }
  x
}

resolve_fasta_dest_dir <- function(dest_dir = NULL) {
  if (is.null(dest_dir)) {
    return(file.path(tempdir(), "rtreeoflife", "fasta", "by_recovery"))
  }

  dest_dir
}

empty_fasta_value <- function(as) {
  switch(
    as,
    "data.frame" = tibble::tibble(
      header = character(),
      sequence = character(),
      width = integer()
    ),
    "list" = stats::setNames(character(), character()),
    "text" = character()
  )
}
