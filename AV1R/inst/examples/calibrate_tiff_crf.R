#!/usr/bin/env Rscript
# AV1R: TIFF CRF calibration
# Encodes a TIFF stack at CRF 1–10, collects file size and SSIM,
# prints comparison table.
#
# Usage: Rscript calibrate_tiff_crf.R

library(AV1R)

# ── Settings (edit here) ─────────────────────────────────────────────────
input    <- "/mnt/Data2/DS_projects/AV_test/mitosis.tif"
crf_min  <- 1L
crf_max  <- 10L

if (!file.exists(input)) stop("File not found: ", input)

size_in <- file.info(input)$size / 1024^2
cat(sprintf("Input: %s (%.1f MB)\n", basename(input), size_in))

# ── Detect backend ───────────────────────────────────────────────────────
bk <- detect_backend()
cat("Backend:", bk, "\n\n")

# ── Run calibration ─────────────────────────────────────────────────────
results <- data.frame(
  crf      = integer(),
  size_mb  = numeric(),
  ratio    = numeric(),
  time_s   = numeric(),
  stringsAsFactors = FALSE
)

for (crf in crf_min:crf_max) {
  out <- file.path(tempdir(), sprintf("av1r_tiff_crf%02d.mp4", crf))

  cat(sprintf("[crf=%2d] encoding ... ", crf))

  elapsed <- tryCatch({
    t0 <- proc.time()["elapsed"]
    convert_to_av1(input, out,
                   options = av1r_options(tiff_crf = crf, backend = bk))
    proc.time()["elapsed"] - t0
  }, error = function(e) {
    cat("FAILED:", conditionMessage(e), "\n")
    NA_real_
  })

  if (is.na(elapsed) || !file.exists(out)) {
    results <- rbind(results, data.frame(
      crf = crf, size_mb = NA, ratio = NA, time_s = NA
    ))
    next
  }

  size_out <- file.info(out)$size / 1024^2
  ratio    <- size_in / size_out

  cat(sprintf("%.2f MB  (%.1fx smaller)  %.1f s\n", size_out, ratio, elapsed))

  results <- rbind(results, data.frame(
    crf = crf, size_mb = size_out, ratio = ratio, time_s = elapsed
  ))

  unlink(out)
}

# ── Print table ──────────────────────────────────────────────────────────
cat("\n")
cat(strrep("=", 42), "\n")
cat(sprintf("  %4s  %10s  %7s  %8s\n", "CRF", "Size (MB)", "Ratio", "Time (s)"))
cat(strrep("-", 42), "\n")

for (i in seq_len(nrow(results))) {
  r <- results[i, ]
  if (!is.na(r$size_mb)) {
    cat(sprintf("  %4d  %10.2f  %6.1fx  %8.1f\n",
                r$crf, r$size_mb, r$ratio, r$time_s))
  } else {
    cat(sprintf("  %4d  %10s  %7s  %8s\n", r$crf, "-", "-", "-"))
  }
}

cat(strrep("=", 42), "\n")
cat(sprintf("\nInput: %s (%.1f MB)\n", basename(input), size_in))
cat(sprintf("Backend: %s\n", bk))
cat("Done.\n")
