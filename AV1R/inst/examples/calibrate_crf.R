#!/usr/bin/env Rscript
# AV1R: CRF calibration across backends
# Encodes a short clip at each CRF value for all backends,
# collects video bitrate and SSIM, prints comparison table.
#
# Usage: Rscript calibrate_crf.R

library(AV1R)

# ── Settings (edit here) ─────────────────────────────────────────────────
input    <- "/mnt/Data2/DS_projects/AV_test/test.mp4"
crf_min  <- 1L
crf_max  <- 50L
duration <- 5  # seconds

if (!file.exists(input)) stop("File not found: ", input)
stopifnot(crf_min >= 0, crf_max <= 63, crf_min <= crf_max)

# ── Trim short clip ──────────────────────────────────────────────────────
clip <- file.path(tempdir(), paste0("av1r_calib_clip.", tools::file_ext(input)))
cat(sprintf("Trimming first %d s from: %s\n", duration, input))
ret <- system2("ffmpeg", c(
  "-y", "-i", shQuote(input),
  "-t", as.character(duration),
  "-c", "copy", shQuote(clip)
), stdout = FALSE, stderr = FALSE)
if (ret != 0) stop("ffmpeg trim failed")

# ── Detect backends ──────────────────────────────────────────────────────
all_backends <- c("cpu", "vaapi", "vulkan")
available <- vapply(all_backends, function(bk) {
  tryCatch(detect_backend(prefer = bk) == bk, error = function(e) FALSE)
}, logical(1))
backends <- names(available[available])
cat("Backends:", paste(backends, collapse = ", "), "\n")
if (length(backends) == 0) stop("No backends available")

# ── Helper: get video bitrate (bps) from encoded file ────────────────────
get_video_bitrate <- function(path) {
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
  if (length(bps) == 0 || is.na(bps)) NA_integer_ else bps
}

# ── Run calibration ─────────────────────────────────────────────────────
results <- data.frame(
  crf     = integer(),
  backend = character(),
  bitrate_kbps = numeric(),
  ssim    = numeric(),
  stringsAsFactors = FALSE
)

total <- length(crf_min:crf_max) * length(backends)
step  <- 0L

for (crf in crf_min:crf_max) {
  for (bk in backends) {
    step <- step + 1L
    out <- file.path(tempdir(), sprintf("av1r_calib_%s_crf%02d.mp4", bk, crf))

    cat(sprintf("[%d/%d] %s crf=%d ... ", step, total, bk, crf))

    elapsed <- tryCatch({
      t0 <- proc.time()["elapsed"]
      convert_to_av1(clip, out, options = av1r_options(crf = crf, backend = bk))
      proc.time()["elapsed"] - t0
    }, error = function(e) {
      cat("FAILED:", conditionMessage(e), "\n")
      NA_real_
    })

    if (is.na(elapsed) || !file.exists(out)) {
      results <- rbind(results, data.frame(
        crf = crf, backend = bk, bitrate_kbps = NA, ssim = NA
      ))
      next
    }

    bps  <- get_video_bitrate(out)
    kbps <- if (!is.na(bps)) bps / 1000 else NA_real_

    ssim <- tryCatch(
      measure_ssim(clip, out, duration = duration),
      error = function(e) NA_real_
    )

    cat(sprintf("%.0f kbps  SSIM=%.4f  (%.1fs)\n",
                if (is.na(kbps)) 0 else kbps,
                if (is.na(ssim)) 0 else ssim,
                elapsed))

    results <- rbind(results, data.frame(
      crf = crf, backend = bk, bitrate_kbps = kbps, ssim = ssim
    ))

    unlink(out)
  }
}

# ── Print table ──────────────────────────────────────────────────────────
cat("\n")
cat(strrep("=", 20 + 22 * length(backends)), "\n")

# Header
hdr <- sprintf("  %4s", "CRF")
for (bk in backends) {
  hdr <- paste0(hdr, sprintf("  %10s %10s", paste0(bk, "_kbps"), paste0(bk, "_ssim")))
}
cat(hdr, "\n")
cat(strrep("-", 20 + 22 * length(backends)), "\n")

for (crf in crf_min:crf_max) {
  line <- sprintf("  %4d", crf)
  for (bk in backends) {
    row <- results[results$crf == crf & results$backend == bk, ]
    if (nrow(row) == 1 && !is.na(row$bitrate_kbps)) {
      line <- paste0(line, sprintf("  %10.0f %10.4f", row$bitrate_kbps, row$ssim))
    } else {
      line <- paste0(line, sprintf("  %10s %10s", "-", "-"))
    }
  }
  cat(line, "\n")
}

cat(strrep("=", 20 + 22 * length(backends)), "\n")
cat(sprintf("\nInput: %s (%d s clip)\n", basename(input), duration))

# ── Cleanup ──────────────────────────────────────────────────────────────
unlink(clip)
cat("Done.\n")
