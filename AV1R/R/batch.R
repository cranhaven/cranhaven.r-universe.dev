#' Convert all videos in a folder to AV1
#'
#' Finds all supported video files in \code{input_dir} and converts them to
#' AV1. Output files are written to \code{output_dir} with the same base name
#' and \code{.mp4} extension.
#'
#' When a folder contains only single-page TIFF images (e.g. a microscopy
#' image sequence), they are automatically combined into a single video
#' named after the input folder.
#'
#' When \code{recursive = TRUE}, subfolders (up to \code{max_depth} levels)
#' are scanned. Each subfolder containing only TIFF images is combined into
#' a single video named after the subfolder. Subfolders with video files are
#' processed file-by-file. All output is written flat into \code{output_dir},
#' with subfolder names used as prefixes to avoid collisions.
#'
#' @param input_dir  Path to folder with input files.
#' @param output_dir Path to folder for output files. Created if it does not
#'   exist. Defaults to \code{input_dir}.
#' @param options    An \code{av1r_options} list. Defaults to
#'   \code{av1r_options()}.
#' @param ext        Character vector of input extensions to process.
#'   Default: \code{c("mp4","avi","mkv","mov","flv","mpg","mpeg","webm","tif","tiff")}.
#' @param skip_existing If \code{TRUE} (default), skip files where the output
#'   already exists.
#' @param recursive  If \code{TRUE}, scan subfolders up to \code{max_depth}
#'   levels deep. Default: \code{FALSE}.
#' @param max_depth  Maximum subfolder depth when \code{recursive = TRUE}.
#'   Default: 5.
#'
#' @return Invisibly returns a data.frame with columns \code{input},
#'   \code{output}, \code{input_size} (bytes), \code{status}
#'   ("ok", "skipped", or "error"), and \code{message}.
#'
#' @examples
#' \dontrun{
#' # Requires FFmpeg installed
#' convert_folder("~/data/microscopy", file.path(tempdir(), "av1_output"))
#'
#' # Recursive: each subfolder with TIFFs becomes a separate video
#' convert_folder("~/data/plates", "~/av1_output", recursive = TRUE)
#' }
#' @export
convert_folder <- function(input_dir,
                           output_dir    = input_dir,
                           options       = av1r_options(),
                           ext           = c("mp4", "avi", "mkv", "mov", "flv", "mpg", "mpeg", "webm", "tif", "tiff"),
                           skip_existing = TRUE,
                           recursive     = FALSE,
                           max_depth     = 5L) {
  if (!dir.exists(input_dir))
    stop("Input directory not found: ", input_dir)

  if (!dir.exists(output_dir))
    dir.create(output_dir, recursive = TRUE)

  # Collect all tasks: list of lists with dir_path, prefix, is_tiff_seq, files
  tasks <- .collect_tasks(input_dir, ext,
                          recursive = recursive, max_depth = max_depth,
                          output_dir = output_dir)

  if (length(tasks) == 0) {
    message("AV1R: no supported files found in ", input_dir)
    empty_df <- data.frame(input = character(), output = character(),
                            input_size = numeric(), status = character(),
                            message = character(), stringsAsFactors = FALSE)
    return(invisible(empty_df))
  }

  n_tasks <- length(tasks)
  bk <- if (options$backend == "auto") detect_backend() else options$backend
  message(sprintf("AV1R batch: %d task(s), backend=%s", n_tasks, bk))

  all_results <- vector("list", n_tasks)

  for (ti in seq_along(tasks)) {
    task <- tasks[[ti]]

    if (task$is_tiff_seq) {
      message(sprintf("[%d/%d] %d TIFF images -> %s",
                      ti, n_tasks, length(task$files), task$out_name))
      all_results[[ti]] <- .convert_tiff_sequence(
        task$files, task$dir_path, output_dir,
        options, skip_existing, prefix = task$prefix)
    } else {
      inp     <- task$files
      base    <- sub("\\.[^.]+$", "", basename(inp))
      out_name <- if (is.null(task$prefix)) {
        paste0(base, "_av1.mp4")
      } else {
        paste0(task$prefix, "_", base, "_av1.mp4")
      }
      out     <- file.path(output_dir, out_name)
      in_size <- file.info(inp)$size

      if (skip_existing && file.exists(out)) {
        message(sprintf("[%d/%d] skip  %s (exists)", ti, n_tasks, basename(inp)))
        all_results[[ti]] <- data.frame(
          input = inp, output = out, input_size = in_size,
          status = "skipped", message = "output exists",
          stringsAsFactors = FALSE)
        next
      }

      message(sprintf("[%d/%d] %s -> %s", ti, n_tasks, basename(inp), basename(out)))
      status <- "ok"
      msg    <- ""

      withCallingHandlers(
        tryCatch(
          convert_to_av1(inp, out, options),
          error = function(e) {
            status  <<- "error"
            msg     <<- conditionMessage(e)
            message("  ERROR: ", msg)
          }
        ),
        warning = function(w) {
          msg <<- conditionMessage(w)
          invokeRestart("muffleWarning")
        }
      )

      all_results[[ti]] <- data.frame(
        input = inp, output = out, input_size = in_size,
        status = status, message = msg,
        stringsAsFactors = FALSE)
    }
  }

  root_df <- do.call(rbind, all_results)

  n_ok  <- sum(root_df$status == "ok")
  n_err <- sum(root_df$status == "error")
  n_skp <- sum(root_df$status == "skipped")
  n_bloat <- sum(grepl("larger than input", root_df$message))
  message(sprintf("\nAV1R batch done: %d ok, %d skipped, %d errors", n_ok, n_skp, n_err))
  if (n_bloat > 0) {
    message(sprintf("  %d file(s) re-encoded from VP9/AV1 -- output may be larger than input",
                     n_bloat))
  }

  invisible(root_df)
}

# Internal: list all subdirectories up to max_depth levels
# exclude: normalized path to skip (e.g. output_dir inside input_dir)
.list_subdirs <- function(root, max_depth, exclude = NULL) {
  result <- character()
  queue  <- list(list(path = root, depth = 0L))

  while (length(queue) > 0) {
    item  <- queue[[1]]
    queue <- queue[-1]

    if (item$depth >= max_depth) next

    children <- list.dirs(item$path, recursive = FALSE, full.names = TRUE)

    # Exclude output directory (and anything inside it)
    if (!is.null(exclude)) {
      children <- children[normalizePath(children, mustWork = FALSE) != exclude]
    }

    result <- c(result, children)

    for (ch in children) {
      queue <- c(queue, list(list(path = ch, depth = item$depth + 1L)))
    }
  }

  result
}

# Internal: scan directories and build flat task list
# Each task is a list with: dir_path, prefix, is_tiff_seq, files, out_name
.collect_tasks <- function(input_dir, ext, recursive, max_depth, output_dir) {
  pattern <- paste0("\\.(", paste(ext, collapse = "|"), ")$")
  tasks   <- list()

  # Helper: scan one directory and append tasks
  add_dir <- function(dir_path, prefix) {
    files <- list.files(dir_path, pattern = pattern,
                        ignore.case = TRUE, full.names = TRUE)
    if (length(files) == 0) return()

    tiff_files <- grep("\\.(tiff?|TIFF?)$", files, value = TRUE)

    if (length(tiff_files) == length(files) && length(files) > 1) {
      # All TIFFs -> one sequence task
      dir_name <- if (!is.null(prefix)) prefix else basename(normalizePath(dir_path))
      tasks[[length(tasks) + 1L]] <<- list(
        dir_path = dir_path, prefix = prefix,
        is_tiff_seq = TRUE, files = sort(files),
        out_name = paste0(dir_name, ".mp4"))
    } else {
      # Individual file tasks
      for (f in files) {
        base <- sub("\\.[^.]+$", "", basename(f))
        out_name <- if (is.null(prefix)) {
          paste0(base, "_av1.mp4")
        } else {
          paste0(prefix, "_", base, "_av1.mp4")
        }
        tasks[[length(tasks) + 1L]] <<- list(
          dir_path = dir_path, prefix = prefix,
          is_tiff_seq = FALSE, files = f,
          out_name = out_name)
      }
    }
  }

  # Root directory
  add_dir(input_dir, prefix = NULL)

  # Subdirectories
  if (recursive) {
    out_norm <- normalizePath(output_dir, mustWork = FALSE)
    subdirs  <- .list_subdirs(input_dir, max_depth, exclude = out_norm)
    for (sd in subdirs) {
      rel    <- substring(sd, nchar(normalizePath(input_dir)) + 2L)
      prefix <- gsub(.Platform$file.sep, "_", rel)
      add_dir(sd, prefix = prefix)
    }
  }

  tasks
}

# Internal: combine single-page TIFFs into one video via numbered symlinks
.convert_tiff_sequence <- function(tiff_files, input_dir, output_dir,
                                    options, skip_existing, prefix = NULL) {
  dir_name <- if (!is.null(prefix)) prefix else basename(normalizePath(input_dir))
  out <- file.path(output_dir, paste0(dir_name, ".mp4"))

  tiff_total_size <- sum(file.info(tiff_files)$size)

  if (skip_existing && file.exists(out)) {
    message(sprintf("AV1R: skip %s (exists)", basename(out)))
    return(invisible(data.frame(
      input = input_dir, output = out, input_size = tiff_total_size,
      status = "skipped", message = "output exists"
    )))
  }

  # Sort files for consistent frame order
  tiff_files <- sort(tiff_files)
  n <- length(tiff_files)

  # Create numbered symlinks in temp dir for ffmpeg image2 demuxer
  tmpdir <- tempfile("av1r_seq_")
  dir.create(tmpdir, recursive = TRUE)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)

  ext <- tolower(tools::file_ext(tiff_files[1]))
  for (i in seq_len(n)) {
    link_name <- file.path(tmpdir, sprintf("frame%06d.%s", i, ext))
    file.symlink(tiff_files[i], link_name)
  }

  seq_pattern <- file.path(tmpdir, sprintf("frame%%06d.%s", ext))

  status <- "ok"
  msg    <- ""
  tryCatch(
    convert_to_av1(seq_pattern, out, options),
    error = function(e) {
      status <<- "error"
      msg    <<- conditionMessage(e)
      message("  ERROR: ", msg)
    }
  )

  if (status == "ok") {
    size_in  <- tiff_total_size / 1024^2
    size_out <- file.info(out)$size / 1024^2
    message(sprintf("AV1R: %d frames, %.1f MB -> %.1f MB (%.1fx)",
                    n, size_in, size_out, size_in / size_out))
  }

  invisible(data.frame(
    input = input_dir, output = out, input_size = tiff_total_size,
    status = status, message = msg
  ))
}
