#!/usr/bin/env Rscript
# AV1R: VAAPI bitrate response calibration
# VAAPI ignores CRF/QP — only responds to -b:v
# This script maps requested bitrate → actual output bitrate
#
# Usage: Rscript calibrate_vaapi_bitrate.R

# ── Settings (edit here) ─────────────────────────────────────────────────
input        <- "/mnt/Data2/DS_projects/AV_test/test.mp4"
duration     <- 5  # seconds
bitrates_kbps <- c(
  seq(100, 500, by = 50),
  seq(600, 1500, by = 100),
  seq(2000, 5000, by = 500),
  seq(6000, 12000, by = 1000)
)

if (!file.exists(input)) stop("File not found: ", input)

# ── Trim short clip (video only) ────────────────────────────────────────
clip <- file.path(tempdir(), paste0("av1r_vaapi_cal.", tools::file_ext(input)))
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

# ── Run calibration ─────────────────────────────────────────────────────
results <- data.frame(
  target_kbps = integer(),
  actual_kbps = integer(),
  stringsAsFactors = FALSE
)

total <- length(bitrates_kbps)

for (i in seq_along(bitrates_kbps)) {
  bps <- bitrates_kbps[i] * 1000L
  out <- file.path(tempdir(), sprintf("vaapi_br%d.mp4", bitrates_kbps[i]))

  cat(sprintf("[%2d/%d] target=%5d kbps ... ", i, total, bitrates_kbps[i]))

  ret <- system2("ffmpeg", c(
    "-y", "-vaapi_device", "/dev/dri/renderD128",
    "-i", shQuote(clip),
    "-vf", "format=nv12,hwupload",
    "-c:v", "av1_vaapi",
    "-b:v", as.character(bps),
    "-an", shQuote(out)
  ), stdout = FALSE, stderr = FALSE)

  if (ret != 0 || !file.exists(out)) {
    cat("FAILED\n")
    results <- rbind(results, data.frame(target_kbps = bitrates_kbps[i], actual_kbps = NA_integer_))
    next
  }

  actual <- get_vbitrate(out)
  cat(sprintf("actual=%5s kbps\n", if (is.na(actual)) "-" else as.character(actual)))

  results <- rbind(results, data.frame(target_kbps = bitrates_kbps[i], actual_kbps = actual))
  unlink(out)
}

# ── Print table ──────────────────────────────────────────────────────────
cat("\n")
cat(strrep("=", 40), "\n")
cat(sprintf("  %12s  %12s  %8s\n", "Target kbps", "Actual kbps", "Ratio"))
cat(strrep("-", 40), "\n")

for (i in seq_len(nrow(results))) {
  r <- results[i, ]
  ratio <- if (!is.na(r$actual_kbps) && r$target_kbps > 0)
    sprintf("%7.2f", r$actual_kbps / r$target_kbps) else sprintf("%8s", "-")
  cat(sprintf("  %12d  %12s  %s\n",
              r$target_kbps,
              if (is.na(r$actual_kbps)) "-" else as.character(r$actual_kbps),
              ratio))
}
cat(strrep("=", 40), "\n")
cat(sprintf("\nInput: %s (%d s clip, video only)\n", basename(input), duration))

# ── Cleanup ──────────────────────────────────────────────────────────────
unlink(clip)
cat("Done.\n")
