#!/usr/bin/env Rscript
# AV1R: CQP calibration on TIFF stack (all backends)
# Encodes a TIFF stack with CPU (SVT-AV1), VAAPI CQP, and Vulkan CQP,
# prints side-by-side table to help calibrate CRF → QP mapping.
#
# Usage: Rscript calibrate_vaapi_cqp.R

library(AV1R)

# ── Settings (edit here) ─────────────────────────────────────────────────
input <- "/mnt/Data2/DS_projects/AV_test/mitosis.tif"

if (!file.exists(input)) stop("File not found: ", input)
if (!requireNamespace("magick", quietly = TRUE)) stop("magick required")

size_in <- file.info(input)$size / 1024
cat(sprintf("Input: %s (%.1f KB)\n", basename(input), size_in))

# ── Extract TIFF to PNG sequence ────────────────────────────────────────
tmpdir <- tempfile("av1r_cqp_")
dir.create(tmpdir)
on.exit(unlink(tmpdir, recursive = TRUE))

img <- magick::image_read(input)
n <- length(img)
cat(sprintf("Extracting %d frames...\n", n))
for (i in seq_len(n)) {
  frame <- magick::image_convert(img[i], type = "TrueColor", depth = 8)
  magick::image_write(frame, file.path(tmpdir, sprintf("frame%06d.png", i)),
                      format = "png")
}
seq_pattern <- file.path(tmpdir, "frame%06d.png")

info <- AV1R:::.ffmpeg_video_info(seq_pattern)
sz   <- AV1R:::.compute_output_size(info$width, info$height)
cat(sprintf("Size: %dx%d -> %dx%d\n\n", info$width, info$height,
            sz$width, sz$height))

ffmpeg <- Sys.which("ffmpeg")

# ── Helper: encode and return size in KB ─────────────────────────────────
encode_size <- function(args, out) {
  ret <- suppressWarnings(system2(ffmpeg, args, stdout = FALSE, stderr = FALSE))
  if (ret == 0 && file.exists(out)) {
    sz <- file.info(out)$size / 1024
    unlink(out)
    sz
  } else {
    NA_real_
  }
}

vf_cpu   <- if (sz$scaled) paste0("scale=", sz$width, ":", sz$height, ",format=yuv420p") else "format=yuv420p"
vf_vaapi <- if (sz$scaled) {
  sprintf("scale=%d:%d,format=nv12,hwupload", sz$width, sz$height)
} else {
  "format=nv12,hwupload"
}

# ── CPU (SVT-AV1) CRF sweep ─────────────────────────────────────────────
cpu_crfs <- sort(unique(c(1:10, 15L, 20L, 30L, 40L, 50L, 63L)))
cat("=== CPU (SVT-AV1) CRF ===\n")
cat(sprintf("%5s %10s\n", "CRF", "Size(KB)"))

cpu_results <- data.frame(crf = integer(), size_kb = numeric())
for (crf in cpu_crfs) {
  out <- file.path(tmpdir, sprintf("cpu_crf%02d.mp4", crf))
  kb <- encode_size(c(
    "-y", "-framerate", "25", "-i", shQuote(seq_pattern),
    "-vf", vf_cpu,
    "-c:v", "libsvtav1", "-crf", as.character(crf), "-b:v", "0",
    "-svtav1-params", "preset=8", "-an", shQuote(out)
  ), out)
  cat(sprintf("%5d %10.1f\n", crf, if (is.na(kb)) 0 else kb))
  cpu_results <- rbind(cpu_results, data.frame(crf = crf, size_kb = kb))
}

# ── VAAPI CQP sweep ─────────────────────────────────────────────────────
vaapi_qps <- sort(unique(c(1:40, 45L, 50L, 55L, 60L, 65L, 70L, 75L, 80L, 85L,
                           90L, 100L, 110L, 120L, 130L, 140L, 150L, 160L,
                           170L, 180L, 190L, 200L, 255L)))
cat("\n=== VAAPI CQP ===\n")
cat(sprintf("%5s %10s\n", "QP", "Size(KB)"))

vaapi_results <- data.frame(qp = integer(), size_kb = numeric())
for (qp in vaapi_qps) {
  out <- file.path(tmpdir, sprintf("vaapi_qp%03d.mp4", qp))
  kb <- encode_size(c(
    "-y", "-vaapi_device", "/dev/dri/renderD128",
    "-framerate", "25", "-i", shQuote(seq_pattern),
    "-vf", vf_vaapi,
    "-c:v", "av1_vaapi", "-rc_mode", "CQP",
    "-global_quality", as.character(qp),
    "-an", shQuote(out)
  ), out)
  cat(sprintf("%5d %10.1f\n", qp, if (is.na(kb)) 0 else kb))
  vaapi_results <- rbind(vaapi_results, data.frame(qp = qp, size_kb = kb))
}

# ── Vulkan CRF sweep ──────────────────────────────────────────────────────
has_vulkan <- vulkan_available()
vulkan_results <- data.frame(crf = integer(), size_kb = numeric())

if (has_vulkan) {
  vulkan_crfs <- sort(unique(c(1:20, 30L, 40L, 50L, 63L)))
  cat("\n=== Vulkan CRF ===\n")
  cat(sprintf("%5s %10s\n", "CRF", "Size(KB)"))

  for (crf in vulkan_crfs) {
    out <- file.path(tmpdir, sprintf("vulkan_crf%02d.mp4", crf))
    kb <- tryCatch({
      .Call("R_av1r_vulkan_encode",
            seq_pattern, out,
            sz$width, sz$height, 25L, as.integer(crf),
            PACKAGE = "AV1R")
      if (file.exists(out)) {
        s <- file.info(out)$size / 1024
        unlink(out)
        s
      } else NA_real_
    }, error = function(e) NA_real_)
    cat(sprintf("%5d %10.1f\n", crf, if (is.na(kb)) 0 else kb))
    vulkan_results <- rbind(vulkan_results, data.frame(crf = crf, size_kb = kb))
  }
} else {
  cat("\nVulkan not available, skipping.\n")
}

# ── Match: for each CPU CRF find closest VAAPI QP by file size ──────────
cat("\n=== CRF -> QP mapping (matched by file size) ===\n")
hdr <- sprintf("%5s %10s %5s %10s", "CRF", "CPU(KB)", "QP", "VAAPI(KB)")
if (has_vulkan) hdr <- paste0(hdr, sprintf(" %5s %10s", "vkCRF", "Vulkan(KB)"))
cat(hdr, "\n")
cat(strrep("-", if (has_vulkan) 55 else 35), "\n")

for (i in seq_len(nrow(cpu_results))) {
  target <- cpu_results$size_kb[i]
  if (is.na(target)) next

  # Find closest VAAPI QP
  valid_v <- vaapi_results[!is.na(vaapi_results$size_kb), ]
  idx_v   <- which.min(abs(valid_v$size_kb - target))
  line <- sprintf("%5d %10.1f %5d %10.1f",
                  cpu_results$crf[i], target,
                  valid_v$qp[idx_v], valid_v$size_kb[idx_v])

  # Find closest Vulkan CRF
  if (has_vulkan && nrow(vulkan_results) > 0) {
    valid_k <- vulkan_results[!is.na(vulkan_results$size_kb), ]
    if (nrow(valid_k) > 0) {
      idx_k <- which.min(abs(valid_k$size_kb - target))
      line <- paste0(line, sprintf(" %5d %10.1f",
                                   valid_k$crf[idx_k], valid_k$size_kb[idx_k]))
    }
  }

  cat(line, "\n")
}

cat("\nDone.\n")
