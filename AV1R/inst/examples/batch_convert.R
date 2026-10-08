#!/usr/bin/env Rscript
# AV1R: Batch convert all videos in a folder to AV1
#
# Converts MP4, AVI, MKV, MOV, TIFF stacks to AV1.
# Output files get "_av1.mp4" suffix in the output folder.
# Existing output files are skipped.
#
# Usage: Rscript batch_convert.R

library(AV1R)

# ── Settings (edit here) ─────────────────────────────────────────────────
input_dir  <- "/mnt/Data2/DS_projects/AV_test1"
output_dir <- "/mnt/Data2/DS_projects/AV_test1/av1_output"
crf        <- 28L
preset     <- 8L     # CPU only: 0 (slow/best) - 13 (fast)
backend    <- "auto" # "auto", "vaapi", "vulkan", "cpu"
recursive  <- TRUE   # scan subfolders (TIFF folders -> single video each)
max_depth  <- 5L     # max subfolder depth when recursive = TRUE

# ── Run ──────────────────────────────────────────────────────────────────
opts <- av1r_options(crf = crf, preset = preset, backend = backend)
cat(sprintf("Input:     %s\n", input_dir))
cat(sprintf("Output:    %s\n", output_dir))
cat(sprintf("Backend:   %s (detected: %s)\n", backend, detect_backend()))
cat(sprintf("Recursive: %s (max_depth=%d)\n", recursive, max_depth))
print(opts)
cat("\n")

t0 <- proc.time()["elapsed"]
results <- convert_folder(input_dir, output_dir, options = opts,
                          recursive = recursive, max_depth = max_depth)
elapsed <- proc.time()["elapsed"] - t0

# ── Summary ──────────────────────────────────────────────────────────────
cat("\n")
cat(strrep("=", 78), "\n")
cat(sprintf("  %-40s %10s %10s %8s\n", "File", "Input MB", "Output MB", "Ratio"))
cat(strrep("-", 78), "\n")

total_in  <- 0
total_out <- 0

for (i in seq_len(nrow(results))) {
  r <- results[i, ]
  if (r$status == "ok") {
    in_mb  <- r$input_size / 1024^2
    out_mb <- file.info(r$output)$size / 1024^2
    total_in  <- total_in + in_mb
    total_out <- total_out + out_mb
    cat(sprintf("  %-40s %10.1f %10.1f %7.1fx\n",
                basename(r$input), in_mb, out_mb, in_mb / out_mb))
  } else {
    cat(sprintf("  %-40s %10s %10s %8s\n",
                basename(r$input), "-", "-", toupper(r$status)))
  }
}

cat(strrep("-", 78), "\n")
cat(sprintf("  %-40s %10.1f %10.1f %7.1fx\n",
            "TOTAL", total_in, total_out,
            if (total_out > 0) total_in / total_out else 0))
cat(strrep("=", 78), "\n")
cat(sprintf("\nTime: %.1f s\n", elapsed))
