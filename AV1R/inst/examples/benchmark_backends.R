#!/usr/bin/env Rscript
# AV1R: Benchmark all available backends
# Usage: Rscript benchmark_backends.R <input_file> [crf]

library(AV1R)

# ── Settings ──────────────────────────────────────────────────────────────
input    <- "/mnt/Data2/DS_projects/AV_test/test.avi"
crf      <- 28L
duration <- 130  # seconds

if (!file.exists(input)) stop("File not found: ", input)
output_dir <- dirname(input)

# ── Get video info via ffprobe ────────────────────────────────────────────
probe_lines <- system2("ffprobe", c(
  "-v", "quiet", "-select_streams", "v:0",
  "-show_entries", "stream=width,height,r_frame_rate",
  "-of", "default=noprint_wrappers=1", shQuote(input)
), stdout = TRUE, stderr = FALSE)

width  <- as.integer(sub("width=",  "", grep("^width=",  probe_lines, value = TRUE)))
height <- as.integer(sub("height=", "", grep("^height=", probe_lines, value = TRUE)))
fps_str <- sub("r_frame_rate=", "", grep("^r_frame_rate=", probe_lines, value = TRUE))
fps <- if (grepl("/", fps_str)) {
  parts <- strsplit(fps_str, "/")[[1]]
  as.numeric(parts[1]) / as.numeric(parts[2])
} else {
  as.numeric(fps_str)
}

# ── Trim clip ─────────────────────────────────────────────────────────────
bench_input <- file.path(tempdir(), paste0("av1r_bench_clip.", tools::file_ext(input)))
cat(sprintf("Trimming first %d s from: %s\n", duration, input))
ret <- system2("ffmpeg", c(
  "-y", "-i", shQuote(input),
  "-t", as.character(duration),
  "-c", "copy", shQuote(bench_input)
), stdout = FALSE, stderr = FALSE)
if (ret != 0) stop("ffmpeg trim failed")

n_frames  <- as.integer(round(fps * duration))
input_mb  <- file.info(bench_input)$size / 1024^2

cat(sprintf("Input:  %s\n", basename(input)))
cat(sprintf("Clip:   %d s, %dx%d @ %.1f fps, %d frames\n",
            duration, width, height, fps, n_frames))
cat(sprintf("Clip size: %.1f MB\n", input_mb))
cat(sprintf("CRF:    %d\n\n", crf))

# ── Detect backends ───────────────────────────────────────────────────────
all_backends <- c("vulkan", "vaapi", "cpu")

available <- vapply(all_backends, function(bk) {
  tryCatch({
    detected <- detect_backend(prefer = bk)
    detected == bk
  }, error = function(e) FALSE)
}, logical(1))

backends <- names(available[available])
cat("Backends:", paste(backends, collapse = ", "), "\n\n")
if (length(backends) == 0) stop("No backends available")

# ── Benchmark ─────────────────────────────────────────────────────────────
base <- sub("\\.[^.]+$", "", basename(input))
results <- data.frame(
  backend  = character(),
  time_sec = numeric(),
  size_mb  = numeric(),
  stringsAsFactors = FALSE
)

for (bk in backends) {
  out <- file.path(output_dir, paste0(base, "_av1_", bk, ".mp4"))
  cat(sprintf("── %s %s\n", toupper(bk), strrep("-", max(1, 55 - nchar(bk)))))

  elapsed <- tryCatch({
    t0 <- proc.time()["elapsed"]
    convert_to_av1(bench_input, out, options = av1r_options(crf = crf, backend = bk))
    proc.time()["elapsed"] - t0
  }, error = function(e) {
    message("  FAILED: ", conditionMessage(e))
    NA_real_
  })

  out_mb <- if (!is.na(elapsed) && file.exists(out)) {
    file.info(out)$size / 1024^2
  } else {
    NA_real_
  }

  # Measure SSIM vs original
  ssim <- if (!is.na(elapsed) && file.exists(out)) {
    measure_ssim(bench_input, out, duration = duration)
  } else {
    NA_real_
  }

  results <- rbind(results, data.frame(
    backend  = bk,
    time_sec = as.numeric(elapsed),
    size_mb  = out_mb,
    ssim     = ssim
  ))
  cat("\n")
}

# ── Results ───────────────────────────────────────────────────────────────
cat(strrep("=", 78), "\n")
cpu_time <- results$time_sec[results$backend == "cpu"]
cpu_time <- if (length(cpu_time) == 1 && !is.na(cpu_time)) cpu_time else NA_real_

cat(sprintf("  %-10s %10s %10s %12s %8s %10s %10s\n",
            "Backend", "Time (s)", "Size (MB)", "Compression", "SSIM", "FPS", "vs CPU"))
cat(strrep("-", 88), "\n")

for (i in seq_len(nrow(results))) {
  r <- results[i, ]
  comp_str <- if (!is.na(r$size_mb) && r$size_mb > 0)
    sprintf("%11.1fx", input_mb / r$size_mb) else sprintf("%12s", "-")
  ssim_str <- if (!is.na(r$ssim))
    sprintf("%8.4f", r$ssim) else sprintf("%8s", "-")
  fps_str <- if (!is.na(r$time_sec) && r$time_sec > 0)
    sprintf("%10.1f", n_frames / r$time_sec) else sprintf("%10s", "-")
  speedup_str <- if (!is.na(cpu_time) && !is.na(r$time_sec) && r$time_sec > 0)
    sprintf("%9.1fx", cpu_time / r$time_sec) else sprintf("%10s", "-")
  cat(sprintf("  %-10s %10s %10s %s %s %s %s\n",
              r$backend,
              if (is.na(r$time_sec)) "FAILED" else sprintf("%.1f", r$time_sec),
              if (is.na(r$size_mb))  "-"      else sprintf("%.2f", r$size_mb),
              comp_str,
              ssim_str,
              fps_str,
              speedup_str))
}
cat(strrep("=", 88), "\n")
cat(sprintf("\nCompression = clip size (%.1f MB) / encoded size\n", input_mb))
cat("vs CPU = CPU time / backend time (higher = faster than CPU)\n")
cat("SSIM: 1.0 = identical, >= 0.97 visually transparent, < 0.93 noticeable loss\n")
