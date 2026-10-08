#!/usr/bin/env Rscript
# AV1R: TIFF stack encoding with explicit scaling
#
# Tests all backends (VAAPI, Vulkan, CPU) with different tiff_scale values.
# Aspect ratio is always preserved (proportional scaling, no distortion).
#
# Usage: Rscript tiff_scale.R

library(AV1R)

input  <- "/mnt/Data2/DS_projects/AV_test1/mitosis.tif"
outdir <- "/mnt/Data2/DS_projects/AV_test1/av1_output"

backends <- c("vaapi", "vulkan", "cpu")
scales   <- list(
  "default" = NULL,
  "2x"      = 2,
  "4x"      = 4,
  "1080p"   = c(1920, 1080),
  "480p"    = c(640, 480)
)

results <- data.frame(backend = character(), scale = character(),
                      file = character(), size_mb = numeric(),
                      time_s = numeric(), stringsAsFactors = FALSE)

for (bk in backends) {
  cat(sprintf("\n%s\n", strrep("=", 60)))
  cat(sprintf("  Backend: %s\n", toupper(bk)))
  cat(sprintf("%s\n", strrep("=", 60)))

  for (sc_name in names(scales)) {
    sc_val  <- scales[[sc_name]]
    outfile <- file.path(outdir, sprintf("mitosis_%s_%s.mp4", bk, sc_name))

    # Remove old output
    if (file.exists(outfile)) unlink(outfile)

    cat(sprintf("\n--- %s / %s ---\n", bk, sc_name))
    opts <- av1r_options(tiff_crf = 5, tiff_scale = sc_val, backend = bk)

    t0 <- proc.time()["elapsed"]
    ok <- tryCatch({
      convert_to_av1(input, outfile, opts)
      TRUE
    }, error = function(e) {
      message("  ERROR: ", conditionMessage(e))
      FALSE
    })
    elapsed <- proc.time()["elapsed"] - t0

    if (ok && file.exists(outfile)) {
      mb <- file.info(outfile)$size / 1024^2
      results <- rbind(results, data.frame(
        backend = bk, scale = sc_name, file = basename(outfile),
        size_mb = mb, time_s = elapsed, stringsAsFactors = FALSE))
    } else {
      results <- rbind(results, data.frame(
        backend = bk, scale = sc_name, file = basename(outfile),
        size_mb = NA, time_s = elapsed, stringsAsFactors = FALSE))
    }
  }
}

# ── Summary ────────────────────────────────────────────────────────────────
cat(sprintf("\n\n%s\n", strrep("=", 78)))
cat(sprintf("  %-10s %-10s %-35s %8s %8s\n",
            "Backend", "Scale", "File", "Size MB", "Time s"))
cat(strrep("-", 78), "\n")
for (i in seq_len(nrow(results))) {
  r <- results[i, ]
  cat(sprintf("  %-10s %-10s %-35s %8s %8.1f\n",
              r$backend, r$scale, r$file,
              if (is.na(r$size_mb)) "ERROR" else sprintf("%.1f", r$size_mb),
              r$time_s))
}
cat(strrep("=", 78), "\n")
