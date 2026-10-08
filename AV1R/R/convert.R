#' Convert video to AV1
#'
#' Converts biological microscopy video files (MP4/H.264, H.265, AVI/MJPEG)
#' or TIFF stacks to AV1 format. Automatically selects the best available
#' backend: Vulkan GPU (\code{VK_KHR_VIDEO_ENCODE_AV1}) when a compatible
#' device is found, otherwise CPU via FFmpeg (\code{libsvtav1}).
#'
#' @param input  Path to input file. Supported: .mp4, .avi, .mkv, .mov, .flv, .mpg, .mpeg, .webm,
#'   .tif/.tiff (multi-page), or printf pattern like \code{"frame\%04d.tif"}.
#' @param output Path to output file (.mp4 or .mkv).
#' @param options An \code{av1r_options} list. Defaults to \code{av1r_options()}.
#'   Use \code{backend = "cpu"} or \code{backend = "vulkan"} to force a backend.
#'
#' @return Invisibly returns 0L on success. Stops with an error on failure.
#'
#' @examples
#' # List available options
#' str(av1r_options())
#'
#' \dontrun{
#' # Requires FFmpeg installed
#' convert_to_av1("recording.mp4", file.path(tempdir(), "recording_av1.mp4"))
#' }
#' @export
convert_to_av1 <- function(input, output, options = av1r_options()) {
  is_seq <- grepl("%", input, fixed = TRUE)
  if (!is_seq && !file.exists(input)) stop("Input file not found: ", input)
  check_ffmpeg()

  # Warn if input is already an efficient codec (VP9/AV1)
  input_codec <- NA_character_
  input_size  <- NA_real_
  if (!is_seq) {
    input_codec <- .ffmpeg_video_codec(input)
    if (!is.na(input_codec) && input_codec %in% c("av1", "vp9")) {
      warning(sprintf(
        "Input '%s' is already encoded with %s. ",
        basename(input), toupper(input_codec)),
        "Re-encoding to AV1 may increase file size. ",
        "Consider skipping this file unless you need a different container or parameters.",
        call. = FALSE)
    }
    input_size <- file.info(input)$size
  }

  # Multi-page TIFF: extract pages to temp PNG sequence via magick
  # Skip if input is already an image sequence pattern (contains %)
  is_tiff <- !is_seq && grepl("\\.tiff?$", input, ignore.case = TRUE)
  tiff_tmpdir <- NULL
  if (is_tiff) {
    tiff_tmpdir <- tempfile("av1r_tiff_")
    input <- .tiff_to_png_sequence(input, tiff_tmpdir)
    on.exit(unlink(tiff_tmpdir, recursive = TRUE), add = TRUE)
  }

  # TIFF stacks are scientific data -- use tiff_crf (default 5)
  if (is_tiff || is_seq) {
    if (options$verbose && options$crf != options$tiff_crf) {
      message(sprintf(
        "AV1R: TIFF/image input. Using tiff_crf=%d instead of crf=%d.",
        options$tiff_crf, options$crf))
    }
    options$crf <- options$tiff_crf
  }

  bk <- if (options$backend == "auto") detect_backend() else options$backend

  if (bk == "vulkan") {
    # GPU path: CQP only (RADV). Probe CPU SSIM and find matching Vulkan CRF
    # so that quality is comparable across backends.
    # For TIFF/image: use calibrated lookup (probing requires seekable video)
    # For video: probe SSIM dynamically
    vulkan_crf <- if (is_tiff || is_seq) {
      .crf_to_vulkan_crf(options$crf)
    } else {
      .vulkan_probe_crf(input, options$crf)
    }
    message(sprintf("AV1R [gpu/vulkan]: Vulkan AV1 encode  [user_crf=%d -> vulkan_crf=%d]",
                    options$crf, vulkan_crf))
    info <- .ffmpeg_video_info(input)
    ew <- info$width  - (info$width  %% 2L)
    eh <- info$height - (info$height %% 2L)
    ts <- if (is_tiff || is_seq) options$tiff_scale else NULL
    sz <- .compute_output_size(ew, eh, tiff_scale = ts)
    if (sz$scaled) {
      message(sprintf("AV1R [vulkan]: scaling %dx%d -> %dx%d (proportional)",
                      ew, eh, sz$width, sz$height))
    }
    .Call("R_av1r_vulkan_encode",
          input, output,
          sz$width, sz$height, info$fps, as.integer(vulkan_crf),
          PACKAGE = "AV1R")
  } else if (bk == "vaapi") {
    message(sprintf("AV1R [gpu/vaapi]: VAAPI AV1 encode  [crf=%d]", options$crf))
    .vaapi_encode_av1(input, output, options)
  } else if (.has_ffmpeg_encoder("libsvtav1")) {
    # CPU path: SVT-AV1
    .ffmpeg_encode_av1(input, output, options, encoder = "libsvtav1")
  } else {
    stop("SVT-AV1 encoder (libsvtav1) not found in ffmpeg.\n",
         "  Install SVT-AV1 support for ffmpeg:\n",
         "    Ubuntu/Debian: sudo apt install ffmpeg libsvtav1enc-dev\n",
         "    Fedora:        sudo dnf install ffmpeg svt-av1\n",
         "    macOS:         brew install ffmpeg\n",
         "    From source:   https://gitlab.com/AOMediaCodec/SVT-AV1\n",
         "  Verify: ffmpeg -encoders | grep libsvtav1")
  }

  # Warn if output is larger than input (re-encoding efficient codecs)
  if (!is.na(input_size) && file.exists(output)) {
    output_size <- file.info(output)$size
    if (output_size > input_size) {
      warning(sprintf(
        "Output '%s' (%.1f MB) is larger than input (%.1f MB). ",
        basename(output), output_size / 1024^2, input_size / 1024^2),
        "Input may already be efficiently encoded",
        if (!is.na(input_codec)) paste0(" (", input_codec, ")") else "",
        ".",
        call. = FALSE)
    }
  }

  message("AV1R: done.")
  invisible(0L)
}

# Internal: call ffmpeg via system2()
.ffmpeg_encode_av1 <- function(input, output, options, encoder = "libsvtav1") {
  ffmpeg <- Sys.which("ffmpeg")

  is_seq <- grepl("%", input, fixed = TRUE)
  is_tiff <- grepl("\\.tiff?$", input, ignore.case = TRUE)
  is_image <- is_seq || is_tiff

  # For TIFF stacks ffmpeg reads via image2 demuxer
  # shQuote: system2() pastes args into a shell command, so paths with
  # spaces or special characters must be quoted explicitly.
  input_args <- if (is_image) {
    c("-framerate", "25", "-i", shQuote(input))
  } else {
    c("-i", shQuote(input))
  }

  # Crop input to even dimensions (grayscale->yuv420p with odd w/h causes green line)
  info <- .ffmpeg_video_info(input)
  ew <- info$width  - (info$width  %% 2L)
  eh <- info$height - (info$height %% 2L)
  needs_crop <- (ew != info$width || eh != info$height)

  # Scale: user tiff_scale (if image input) + hardware minimum
  ts <- if (is_image) options$tiff_scale else NULL
  sz <- .compute_output_size(ew, eh, tiff_scale = ts)

  scale_args <- if (sz$scaled) {
    message(sprintf("AV1R [cpu]: scaling %dx%d -> %dx%d (proportional)",
                    ew, eh, sz$width, sz$height))
    if (needs_crop) {
      c("-vf", sprintf("crop=%d:%d:0:0,scale=%d:%d", ew, eh, sz$width, sz$height))
    } else {
      c("-vf", sprintf("scale=%d:%d", sz$width, sz$height))
    }
  } else if (needs_crop) {
    c("-vf", sprintf("crop=%d:%d:0:0", ew, eh))
  } else {
    character(0)
  }

  encode_args <- c(
    "-c:v", encoder,
    "-crf", as.character(options$crf),
    "-b:v", "0",
    "-pix_fmt", "yuv420p"
  )

  if (encoder == "libsvtav1") {
    svt_params <- paste0("preset=", options$preset)
    if (options$threads > 0L) {
      svt_params <- paste0(svt_params, ":lp=", options$threads)
    }
    encode_args <- c(encode_args, "-svtav1-params", svt_params)
  }

  audio_args <- if (is_image) c("-an") else c("-c:a", "copy")

  args <- c(
    "-y",
    input_args,
    scale_args,
    encode_args,
    audio_args,
    shQuote(output)
  )

  label <- "cpu"
  message(sprintf(
    "AV1R [%s]: ffmpeg %s -> %s  [encoder=%s crf=%d%s]",
    label, basename(input), basename(output), encoder, options$crf,
    if (encoder == "libsvtav1") sprintf(" preset=%d", options$preset) else ""
  ))

  stderr_file <- tempfile("av1r_ffmpeg_stderr_")
  on.exit(unlink(stderr_file), add = TRUE)
  ret <- system2(ffmpeg, args, stderr = stderr_file)

  if (ret != 0L) {
    stderr_out <- tryCatch(
      paste(readLines(stderr_file, warn = FALSE), collapse = "\n"),
      error = function(e) "(could not read stderr)"
    )
    stop("ffmpeg failed with exit code ", ret,
         "\nCommand: ffmpeg ", paste(shQuote(args), collapse = " "),
         "\nffmpeg stderr:\n", stderr_out)
  }

  invisible(ret)
}

# Internal: VAAPI AV1 encode via ffmpeg av1_vaapi
.vaapi_encode_av1 <- function(input, output, options) {
  ffmpeg <- Sys.which("ffmpeg")
  if (nchar(ffmpeg) == 0) stop("ffmpeg not found")

  is_seq <- grepl("%", input, fixed = TRUE)
  is_tiff <- grepl("\\.tiff?$", input, ignore.case = TRUE)
  is_image <- is_seq || is_tiff
  input_args <- if (is_image) c("-framerate", "25", "-i", shQuote(input)) else c("-i", shQuote(input))

  audio_args <- if (is_image) c("-an") else c("-c:a", "copy")

  # Rate control:
  #   - Explicit bitrate: always use VBR -b:v (user override)
  #   - TIFF/image input: CQP mode -- map CRF (0-63) -> QP (0-255)
  #   - Video input: VBR at 55% of source bitrate (AV1 is ~45% more efficient)
  if (!is.null(options$bitrate)) {
    target_bps <- as.integer(options$bitrate) * 1000L
    rate_args  <- c("-b:v", as.character(target_bps))
    rate_label <- sprintf("bitrate=%dk (manual)", options$bitrate)
  } else if (is_image) {
    vaapi_qp <- .crf_to_vaapi_qp(options$crf)
    rate_args  <- c("-rc_mode", "CQP", "-global_quality", as.character(vaapi_qp))
    rate_label <- sprintf("CQP qp=%d (from crf=%d)", vaapi_qp, options$crf)
  } else {
    input_bitrate <- .ffmpeg_video_bitrate(input)
    if (!is.na(input_bitrate) && input_bitrate > 0) {
      target_bps <- as.integer(input_bitrate * 0.55)
      rate_args  <- c("-b:v", as.character(target_bps))
      rate_label <- sprintf("bitrate=%dk (auto, 55%% of input)", target_bps %/% 1000L)
    } else {
      # Fallback: estimate bitrate from file size and duration
      duration <- .ffmpeg_duration(input)
      fsize    <- file.info(input)$size
      if (!is.na(duration) && duration > 0 && !is.na(fsize) && fsize > 0) {
        est_bps    <- as.integer((fsize * 8) / duration)
        target_bps <- as.integer(est_bps * 0.55)
        rate_args  <- c("-b:v", as.character(target_bps))
        rate_label <- sprintf("bitrate=%dk (auto, 55%% of est. %dk)",
                              target_bps %/% 1000L, est_bps %/% 1000L)
      } else {
        rate_args  <- c("-b:v", "4000000")
        rate_label <- "bitrate=4000k (default)"
      }
    }
  }

  # Align to multiple of 8 (VAAPI macroblock requirement -- avoids green line)
  info <- .ffmpeg_video_info(input)
  ew <- info$width  - (info$width  %% 8L)
  eh <- info$height - (info$height %% 8L)
  needs_align <- (ew != info$width || eh != info$height)

  # Scale: user tiff_scale (if image input) + hardware minimum
  ts <- if (is_image) options$tiff_scale else NULL
  sz <- .compute_output_size(ew, eh, tiff_scale = ts)

  if (sz$scaled) {
    message(sprintf("AV1R [vaapi]: scaling %dx%d -> %dx%d (proportional)",
                    info$width, info$height, sz$width, sz$height))
    if (needs_align) {
      vf <- sprintf("crop=%d:%d:0:0,scale=%d:%d,format=nv12,hwupload",
                     ew, eh, sz$width, sz$height)
    } else {
      vf <- sprintf("scale=%d:%d,format=nv12,hwupload", sz$width, sz$height)
    }
  } else if (needs_align) {
    message(sprintf("AV1R [vaapi]: cropping %dx%d -> %dx%d (align to 8)",
                    info$width, info$height, ew, eh))
    vf <- sprintf("crop=%d:%d:0:0,format=nv12,hwupload", ew, eh)
  } else {
    vf <- "format=nv12,hwupload"
  }

  args <- c(
    "-y",
    "-vaapi_device", "/dev/dri/renderD128",
    input_args,
    "-vf", vf,
    "-c:v", "av1_vaapi",
    rate_args,
    audio_args,
    shQuote(output)
  )

  message(sprintf(
    "AV1R [vaapi]: ffmpeg %s -> %s  [%s]",
    basename(input), basename(output), rate_label
  ))

  stderr_file <- tempfile("av1r_ffmpeg_stderr_")
  on.exit(unlink(stderr_file), add = TRUE)
  ret <- system2(ffmpeg, args, stderr = stderr_file)
  if (ret != 0L) {
    stderr_out <- tryCatch(
      paste(readLines(stderr_file, warn = FALSE), collapse = "\n"),
      error = function(e) "(could not read stderr)"
    )
    stop("ffmpeg vaapi failed with exit code ", ret,
         "\nCommand: ffmpeg ", paste(shQuote(args), collapse = " "),
         "\nffmpeg stderr:\n", stderr_out)
  }
  invisible(ret)
}

# Internal: get video stream bitrate in bps (NA if unavailable)
.ffmpeg_video_bitrate <- function(input) {
  ffprobe <- Sys.which("ffprobe")
  if (nchar(ffprobe) == 0) return(NA_integer_)

  lines <- tryCatch(
    suppressWarnings(
      system2(ffprobe,
              c("-v", "quiet", "-select_streams", "v:0",
                "-show_entries", "stream=bit_rate",
                "-of", "default=noprint_wrappers=1", shQuote(input)),
              stdout = TRUE, stderr = FALSE)
    ),
    error = function(e) character(0)
  )

  val <- sub("bit_rate=", "", grep("^bit_rate=", lines, value = TRUE))
  bps <- suppressWarnings(as.integer(val))
  if (length(bps) == 0 || is.na(bps) || bps <= 0) NA_integer_ else bps
}

# Internal: get video width/height/fps via ffprobe
.ffmpeg_video_info <- function(input) {
  ffprobe <- Sys.which("ffprobe")
  if (nchar(ffprobe) == 0) ffprobe <- Sys.which("ffmpeg")
  if (nchar(ffprobe) == 0) stop("ffprobe/ffmpeg not found")

  lines <- tryCatch(
    suppressWarnings(
      system2(ffprobe,
              c("-v", "quiet", "-select_streams", "v:0",
                "-show_entries", "stream=width,height,r_frame_rate",
                "-of", "default=noprint_wrappers=1", shQuote(input)),
              stdout = TRUE, stderr = FALSE)
    ),
    error = function(e) character(0)
  )

  width  <- as.integer(sub("width=",  "", grep("^width=",  lines, value = TRUE)))
  height <- as.integer(sub("height=", "", grep("^height=", lines, value = TRUE)))
  fps_str <- sub("r_frame_rate=", "", grep("^r_frame_rate=", lines, value = TRUE))

  fps <- if (length(fps_str) > 0 && grepl("/", fps_str)) {
    parts <- strsplit(fps_str, "/")[[1]]
    as.integer(round(as.numeric(parts[1]) / as.numeric(parts[2])))
  } else {
    25L
  }

  if (length(width) == 0 || length(height) == 0)
    stop("Could not read video dimensions from: ", input)

  list(width = width, height = height, fps = fps)
}

# Internal: get video codec name via ffprobe (e.g. "h264", "vp9", "av1")
.ffmpeg_video_codec <- function(input) {
  ffprobe <- Sys.which("ffprobe")
  if (nchar(ffprobe) == 0) return(NA_character_)
  lines <- tryCatch(
    suppressWarnings(
      system2(ffprobe,
              c("-v", "quiet", "-select_streams", "v:0",
                "-show_entries", "stream=codec_name",
                "-of", "default=noprint_wrappers=1", shQuote(input)),
              stdout = TRUE, stderr = FALSE)
    ),
    error = function(e) character(0)
  )
  val <- sub("codec_name=", "", grep("^codec_name=", lines, value = TRUE))
  if (length(val) == 0 || nchar(val) == 0) NA_character_ else val
}

# Internal: apply tiff_scale then ensure hardware minimum
# Uses integer multiplier so each pixel maps to an exact NxN block.
# Final dimensions rounded down to multiple of 8 (hardware macroblock alignment).
# Returns list(width, height, scaled)
.compute_output_size <- function(width, height, tiff_scale = NULL,
                                 min_w = 320L, min_h = 128L) {
  mult <- 1L

  if (!is.null(tiff_scale)) {
    if (length(tiff_scale) == 1L) {
      mult <- max(1L, as.integer(round(tiff_scale)))
    } else {
      ratio <- min(tiff_scale[1] / width, tiff_scale[2] / height)
      mult <- max(1L, as.integer(floor(ratio)))
    }
  }

  # Ensure hardware minimum: increase multiplier if needed
  while (width * mult < min_w || height * mult < min_h) {
    mult <- mult + 1L
  }

  sw <- as.integer(width  * mult)
  sh <- as.integer(height * mult)

  # Round down to multiple of 8 (hardware macroblock alignment)
  sw <- sw - (sw %% 8L)
  sh <- sh - (sh %% 8L)

  list(width = sw, height = sh, scaled = (mult > 1L))
}

# Internal: map CRF (0-63) to VAAPI CQP quantizer (0-255)
# Calibrated empirically on microscopy TIFF stacks (RADV/Mesa av1_vaapi).
# Uses piecewise linear interpolation between measured equivalence points:
#   CRF  1 ->  QP  10     CRF  5 ->  QP  15     CRF 10 ->  QP  35
#   CRF 20 ->  QP  70     CRF 30 ->  QP 110     CRF 40 ->  QP 150
#   CRF 50 ->  QP 190     CRF 63 ->  QP 255
.crf_to_vaapi_qp <- function(crf) {
  stopifnot(is.numeric(crf), length(crf) == 1, crf >= 0, crf <= 63)
  crf <- as.integer(crf)
  knots_crf <- c( 0L,  1L,   5L,  10L,  20L,  30L,  40L,  50L,  63L)
  knots_qp  <- c( 1L, 10L,  15L,  35L,  70L, 110L, 150L, 190L, 255L)
  as.integer(round(stats::approx(knots_crf, knots_qp, xout = crf, rule = 2)$y))
}

# Internal: map CPU CRF to Vulkan CRF for TIFF/image input
# Vulkan CQP compresses more aggressively at low CRF (1-7).
# Calibrated empirically on microscopy TIFF stacks (RADV, RX 9070):
#   CPU CRF  1 -> Vulkan  2     CPU CRF  3 -> Vulkan  2
#   CPU CRF  5 -> Vulkan  3     CPU CRF  8 -> Vulkan  5
#   CPU CRF 10 -> Vulkan  7     CPU CRF 15 -> Vulkan 11
#   CPU CRF 20 -> Vulkan 15     CPU CRF 30+ ~= CPU CRF (converge)
.crf_to_vulkan_crf <- function(crf) {
  stopifnot(is.numeric(crf), length(crf) == 1, crf >= 0, crf <= 63)
  crf <- as.integer(crf)
  knots_cpu    <- c( 0L,  1L,  3L,  5L,  8L, 10L, 15L, 20L, 30L, 40L, 50L, 63L)
  knots_vulkan <- c( 1L,  2L,  2L,  3L,  5L,  7L, 11L, 15L, 30L, 40L, 50L, 63L)
  as.integer(round(stats::approx(knots_cpu, knots_vulkan, xout = crf, rule = 2)$y))
}

# Internal: compute dimensions to meet hardware minimum (no user scale)
.ensure_min_size <- function(width, height, min_w = 320L, min_h = 128L) {
  .compute_output_size(width, height, tiff_scale = NULL,
                       min_w = min_w, min_h = min_h)
}

# Internal: extract multi-page TIFF to PNG sequence via magick
# Returns ffmpeg-compatible input path (printf pattern)
.tiff_to_png_sequence <- function(tiff_path, tmpdir) {
  if (!requireNamespace("magick", quietly = TRUE))
    stop("Package 'magick' is required for multi-page TIFF input.\n",
         "  Install: install.packages(\"magick\")")

  dir.create(tmpdir, recursive = TRUE, showWarnings = FALSE)

  img <- magick::image_read(tiff_path)
  n <- length(img)
  message(sprintf("AV1R: extracting %d frames from TIFF stack...", n))

  for (i in seq_len(n)) {
    frame <- img[i]
    # Convert 16-bit grayscale to 8-bit sRGB for ffmpeg compatibility
    frame <- magick::image_convert(frame, type = "TrueColor", depth = 8)
    out_file <- file.path(tmpdir, sprintf("frame%06d.png", i))
    magick::image_write(frame, out_file, format = "png")
  }

  # Return printf pattern for ffmpeg
  file.path(tmpdir, "frame%06d.png")
}

