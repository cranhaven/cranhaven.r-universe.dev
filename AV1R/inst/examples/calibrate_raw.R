#!/usr/bin/env Rscript
# AV1R: Raw CRF/bitrate calibration — bypasses calibration layer
# Calls ffmpeg directly to measure true backend response curves
#
# Usage: Rscript calibrate_raw.R

# ── Settings (edit here) ─────────────────────────────────────────────────
input    <- "/mnt/Data2/DS_projects/AV_test/test.avi"
crf_min  <- 1L
crf_max  <- 63L
duration <- 5  # seconds

if (!file.exists(input)) stop("File not found: ", input)

# ── Trim short clip ──────────────────────────────────────────────────────
clip <- file.path(tempdir(), paste0("av1r_rawcal.", tools::file_ext(input)))
cat(sprintf("Trimming first %d s from: %s\n", duration, input))
ret <- system2("ffmpeg", c(
  "-y", "-i", shQuote(input),
  "-t", as.character(duration),
  "-an", "-c:v", "copy", shQuote(clip)
), stdout = FALSE, stderr = FALSE)
if (ret != 0) stop("ffmpeg trim failed")

# ── Helper: get video bitrate from file ──────────────────────────────────
get_vbitrate <- function(path) {
  lines <- tryCatch(
    suppressWarnings(system2("ffprobe", c(
      "-v", "quiet", "-select_streams", "v:0",
      "-show_entries", "stream=bit_rate",
      "-of", "default=noprint_wrappers=1", shQuote(path)
    ), stdout = TRUE, stderr = FALSE)),
    error = function(e) character(0)
  )
  val <- sub("bit_rate=", "", grep("^bit_rate=", lines, value = TRUE))
  bps <- suppressWarnings(as.integer(val))
  if (length(bps) == 0 || is.na(bps)) NA_integer_ else bps %/% 1000L
}

# ── Check backend availability ───────────────────────────────────────────
has_svtav1 <- system2("ffmpeg", c("-hide_banner", "-encoders"),
                      stdout = TRUE, stderr = FALSE)
has_svtav1 <- any(grepl("libsvtav1", has_svtav1))

has_vaapi <- tryCatch({
  ret <- system2("ffmpeg", c(
    "-y", "-vaapi_device", "/dev/dri/renderD128",
    "-i", shQuote(clip), "-t", "0.1",
    "-vf", "format=nv12,hwupload", "-c:v", "av1_vaapi",
    "-b:v", "1000000", "-f", "null", "/dev/null"
  ), stdout = FALSE, stderr = FALSE)
  ret == 0
}, error = function(e) FALSE)

has_vulkan <- tryCatch({
  library(AV1R)
  vulkan_available()
}, error = function(e) FALSE)

backends <- c()
if (has_svtav1) backends <- c(backends, "cpu")
if (has_vaapi)  backends <- c(backends, "vaapi")
if (has_vulkan) backends <- c(backends, "vulkan")
cat("Backends:", paste(backends, collapse = ", "), "\n\n")

# ── Run calibration ─────────────────────────────────────────────────────
results <- data.frame(
  crf = integer(), backend = character(), kbps = integer(),
  stringsAsFactors = FALSE
)

total <- length(crf_min:crf_max) * length(backends)
step  <- 0L

for (crf in crf_min:crf_max) {
  for (bk in backends) {
    step <- step + 1L
    out <- file.path(tempdir(), sprintf("rawcal_%s_crf%02d.mp4", bk, crf))

    cat(sprintf("[%3d/%d] %-6s crf=%2d ... ", step, total, bk, crf))

    ok <- tryCatch({
      if (bk == "cpu") {
        ret <- system2("ffmpeg", c(
          "-y", "-i", shQuote(clip),
          "-c:v", "libsvtav1", "-crf", crf, "-b:v", "0",
          "-pix_fmt", "yuv420p", "-svtav1-params", sprintf("preset=8"),
          "-an", shQuote(out)
        ), stdout = FALSE, stderr = FALSE)
        ret == 0

      } else if (bk == "vaapi") {
        # VAAPI: use CRF-equivalent via constant_quality (if supported)
        # Fallback: map CRF to bitrate using a wide sweep
        # Try qp mode first, fall back to bitrate matching
        ret <- system2("ffmpeg", c(
          "-y", "-vaapi_device", "/dev/dri/renderD128",
          "-i", shQuote(clip),
          "-vf", "format=nv12,hwupload",
          "-c:v", "av1_vaapi",
          "-qp", crf,
          "-an", shQuote(out)
        ), stdout = FALSE, stderr = FALSE)
        if (ret != 0) {
          # qp not supported, try global_quality
          ret <- system2("ffmpeg", c(
            "-y", "-vaapi_device", "/dev/dri/renderD128",
            "-i", shQuote(clip),
            "-vf", "format=nv12,hwupload",
            "-c:v", "av1_vaapi",
            "-global_quality", crf,
            "-an", shQuote(out)
          ), stdout = FALSE, stderr = FALSE)
        }
        ret == 0

      } else if (bk == "vulkan") {
        # Run in subprocess to avoid segfault on repeated Vulkan init
        r_code <- sprintf(
          'library(AV1R); info <- AV1R:::.ffmpeg_video_info(%s); .Call("R_av1r_vulkan_encode", %s, %s, info$width, info$height, info$fps, %dL, PACKAGE="AV1R")',
          deparse(clip), deparse(clip), deparse(out), crf
        )
        ret <- system2("Rscript", c("-e", shQuote(r_code)),
                        stdout = FALSE, stderr = FALSE)
        ret == 0
      }
    }, error = function(e) {
      cat("FAILED:", conditionMessage(e), "\n")
      FALSE
    })

    if (!ok || !file.exists(out)) {
      cat("FAILED\n")
      results <- rbind(results, data.frame(crf = crf, backend = bk, kbps = NA_integer_))
      next
    }

    kbps <- get_vbitrate(out)
    cat(sprintf("%5s kbps\n", if (is.na(kbps)) "-" else as.character(kbps)))

    results <- rbind(results, data.frame(crf = crf, backend = bk, kbps = kbps))
    unlink(out)
  }
}

# ── Print table ──────────────────────────────────────────────────────────
cat("\n")
w <- 6 + 12 * length(backends)
cat(strrep("=", w), "\n")

hdr <- sprintf("  %3s", "CRF")
for (bk in backends) hdr <- paste0(hdr, sprintf("  %10s", paste0(bk, "_kbps")))
cat(hdr, "\n")
cat(strrep("-", w), "\n")

for (crf in crf_min:crf_max) {
  line <- sprintf("  %3d", crf)
  for (bk in backends) {
    row <- results[results$crf == crf & results$backend == bk, ]
    if (nrow(row) == 1 && !is.na(row$kbps)) {
      line <- paste0(line, sprintf("  %10d", row$kbps))
    } else {
      line <- paste0(line, sprintf("  %10s", "-"))
    }
  }
  cat(line, "\n")
}

cat(strrep("=", w), "\n")
cat(sprintf("\nInput: %s (%d s clip, video only)\n", basename(input), duration))

# ── Cleanup ──────────────────────────────────────────────────────────────
unlink(clip)
cat("Done.\n")
