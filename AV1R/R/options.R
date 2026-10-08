#' AV1R encoding options
#'
#' @param crf     Constant Rate Factor: 0 (best) to 63 (worst). Default 28.
#'   Lower = better quality, larger file. Used as fallback when \code{bitrate}
#'   is \code{NULL} and input bitrate cannot be detected.
#' @param preset  Encoding speed preset: 0 (slowest/best) to 13 (fastest).
#'   Default 8 (good balance for microscopy batch jobs).
#' @param threads Number of CPU threads. 0 = auto-detect.
#' @param bitrate Target video bitrate in kbps (e.g. \code{3000} for 3 Mbps).
#'   \code{NULL} (default) = auto-detect from input (55\% of source bitrate
#'   for VAAPI, CRF for CPU).
#' @param tiff_crf CRF for TIFF/image sequence input. Default 5
#'   (near-lossless, suitable for microscopy data). Overrides \code{crf}
#'   when input is TIFF or image sequence.
#' @param tiff_scale Scale factor or target size for TIFF/image input.
#'   \code{NULL} (default) = no scaling (only auto-scale to meet hardware
#'   minimum). A single number = multiplier (e.g. \code{2} doubles both
#'   dimensions). A vector \code{c(width, height)} = target bounding box
#'   (image is scaled proportionally to fit inside, preserving aspect ratio).
#' @param verbose Logical. If \code{TRUE} (default), print informational
#'   messages about encoding decisions (e.g. TIFF CRF override).
#' @param backend \code{"auto"} (best available: vaapi > vulkan > cpu),
#'   \code{"vaapi"} (VAAPI AV1, AMD/Intel),
#'   \code{"vulkan"} (Vulkan AV1),
#'   or \code{"cpu"} (SVT-AV1 via ffmpeg).
#'
#' @return A named list of encoding parameters.
#'
#' @examples
#' # Default options (auto-detect backend and bitrate)
#' av1r_options()
#'
#' # High-quality lossless-ish for publication figures
#' av1r_options(crf = 15, preset = 4)
#'
#' # Explicit bitrate (GPU VAAPI)
#' av1r_options(bitrate = 2000, backend = "vaapi")
#'
#' # Fast batch conversion of large TIFF stacks
#' av1r_options(crf = 32, preset = 12, threads = 16)
#'
#' @export
av1r_options <- function(crf        = 28L,
                          preset     = 8L,
                          threads    = 0L,
                          bitrate    = NULL,
                          tiff_crf   = 5L,
                          tiff_scale = NULL,
                          verbose    = TRUE,
                          backend    = "auto") {
  backend <- match.arg(backend, c("auto", "vulkan", "vaapi", "cpu"))
  stopifnot(is.numeric(crf),      crf      >= 0, crf      <= 63)
  stopifnot(is.numeric(tiff_crf), tiff_crf >= 0, tiff_crf <= 63)
  stopifnot(is.numeric(preset),   preset   >= 0, preset   <= 13)
  stopifnot(is.numeric(threads),  threads  >= 0)
  stopifnot(is.logical(verbose),  length(verbose) == 1)
  if (!is.null(bitrate)) stopifnot(is.numeric(bitrate), bitrate > 0)
  if (!is.null(tiff_scale)) {
    stopifnot(is.numeric(tiff_scale), all(tiff_scale > 0))
    stopifnot(length(tiff_scale) %in% c(1L, 2L))
  }

  structure(
    list(crf        = as.integer(crf),
         preset     = as.integer(preset),
         threads    = as.integer(threads),
         bitrate    = if (is.null(bitrate)) NULL else as.integer(bitrate),
         tiff_crf   = as.integer(tiff_crf),
         tiff_scale = tiff_scale,
         verbose    = verbose,
         backend    = backend),
    class = "av1r_options"
  )
}

#' @export
print.av1r_options <- function(x, ...) {
  btr <- if (is.null(x$bitrate)) "auto" else paste0(x$bitrate, "k")
  scl <- if (is.null(x$tiff_scale)) "none" else if (length(x$tiff_scale) == 1)
    paste0(x$tiff_scale, "x") else paste0(x$tiff_scale[1], "x", x$tiff_scale[2])
  cat(sprintf(
    "AV1R options: crf=%d  tiff_crf=%d  preset=%d  threads=%s  bitrate=%s  tiff_scale=%s  backend=%s\n",
    x$crf, x$tiff_crf, x$preset,
    if (x$threads == 0L) "auto" else as.character(x$threads),
    btr, scl,
    x$backend
  ))
  invisible(x)
}
