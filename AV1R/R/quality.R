# QP targets for VAAPI CQP AV1 encode
# SSIM thresholds (approx equivalent to VMAF 95-97):
#   SSIM >= 0.97  -- visually transparent
#   SSIM >= 0.95  -- very good
#   SSIM <  0.93  -- noticeable loss

.SSIM_TARGET_HI <- 0.98   # above this: QP can be increased (save space)
.SSIM_TARGET_LO <- 0.97   # below this: QP must be decreased (improve quality)
.QP_DEFAULT     <- 23L    # starting point (visually transparent for AV1)
.QP_MIN         <- 10L
.QP_MAX         <- 40L
.QP_STEP        <-  2L
.PROBE_SECONDS  <- 30L    # seconds of video used for quality probing

# ============================================================================
# Internal: encode a short clip with given QP, return SSIM score
# ============================================================================
.probe_ssim <- function(input, qp, duration = .PROBE_SECONDS) {
  ffmpeg  <- Sys.which("ffmpeg")
  tmp_out <- tempfile(fileext = ".mp4")
  on.exit(unlink(tmp_out))

  # Encode probe clip
  ret <- suppressWarnings(system2(ffmpeg, c(
    "-y",
    "-vaapi_device", "/dev/dri/renderD128",
    "-i", shQuote(input),
    "-vf", "format=nv12,hwupload",
    "-c:v", "av1_vaapi",
    "-rc_mode", "CQP",
    "-qp", as.character(qp),
    "-an", "-t", as.character(duration),
    shQuote(tmp_out)
  ), stdout = FALSE, stderr = FALSE))

  if (ret != 0L || !file.exists(tmp_out)) return(NA_real_)

  # Measure SSIM vs original
  lines <- suppressWarnings(system2(ffmpeg, c(
    "-i", shQuote(tmp_out),
    "-i", shQuote(input),
    "-lavfi", "ssim",
    "-t", as.character(duration),
    "-f", "null", "-"
  ), stdout = TRUE, stderr = TRUE))

  ssim_line <- grep("SSIM", lines, value = TRUE)
  if (length(ssim_line) == 0) return(NA_real_)

  m <- regmatches(ssim_line, regexpr("All:[0-9.]+", ssim_line))
  if (length(m) == 0) return(NA_real_)
  as.numeric(sub("All:", "", m))
}

# ============================================================================
# Internal: find optimal QP for target SSIM via binary search
# ============================================================================
.find_optimal_qp <- function(input) {
  message("AV1R: probing quality (this runs once per file)...")

  lo <- .QP_MIN
  hi <- .QP_MAX
  best_qp   <- .QP_DEFAULT
  best_ssim <- NA_real_

  # Start at QP=23, then binary search
  qp <- .QP_DEFAULT
  for (iter in seq_len(6L)) {
    ssim <- .probe_ssim(input, qp)
    if (is.na(ssim)) break

    message(sprintf("  QP=%d  SSIM=%.4f", qp, ssim))

    if (ssim >= .SSIM_TARGET_LO && ssim <= .SSIM_TARGET_HI) {
      best_qp   <- qp
      best_ssim <- ssim
      break
    }

    if (ssim > .SSIM_TARGET_HI) {
      # Quality too high (file too large) -> increase QP
      best_qp   <- qp
      best_ssim <- ssim
      lo  <- qp
      qp  <- min(qp + max(.QP_STEP, (hi - qp) %/% 2L), .QP_MAX)
    } else {
      # Quality too low -> decrease QP
      hi  <- qp
      qp  <- max(qp - max(.QP_STEP, (qp - lo) %/% 2L), .QP_MIN)
    }

    if (qp == best_qp) break
  }

  message(sprintf("AV1R: selected QP=%d (SSIM=%.4f)", best_qp,
                  if (is.na(best_ssim)) 0 else best_ssim))
  best_qp
}

# ============================================================================
# Vulkan CRF probe: match CPU SSIM via adaptive sampling
# ============================================================================

# Number of 1-second probes based on video duration
.probe_count <- function(duration_sec) {
  if (duration_sec < 300)  return(1L)   # < 5 min
  if (duration_sec < 600)  return(2L)   # 5-10 min
  if (duration_sec < 1800) return(4L)   # 10-30 min
  6L                                     # > 30 min (cap)
}

# Get video duration in seconds via ffprobe
.ffmpeg_duration <- function(input) {
  ffprobe <- Sys.which("ffprobe")
  if (nchar(ffprobe) == 0) return(NA_real_)
  lines <- tryCatch(
    suppressWarnings(system2(ffprobe, c(
      "-v", "quiet", "-show_entries", "format=duration",
      "-of", "default=noprint_wrappers=1", shQuote(input)
    ), stdout = TRUE, stderr = FALSE)),
    error = function(e) character(0)
  )
  val <- sub("duration=", "", grep("^duration=", lines, value = TRUE))
  dur <- suppressWarnings(as.numeric(val))
  if (length(dur) == 0 || is.na(dur)) NA_real_ else dur
}

# Extract N probe segments (1 sec each) from middle 80% of video
.extract_probes <- function(input, n_probes, tmpdir) {
  ffmpeg <- Sys.which("ffmpeg")
  duration <- .ffmpeg_duration(input)
  if (is.na(duration) || duration < 2) {
    # Too short -- use whole file as single probe
    return(input)
  }

  # Exclude first and last 10%
  t_start <- duration * 0.10
  t_end   <- duration * 0.90
  span    <- t_end - t_start

  if (n_probes == 1L) {
    positions <- t_start + span / 2
  } else {
    positions <- seq(t_start, t_end, length.out = n_probes)
  }

  probes <- character(n_probes)
  for (i in seq_len(n_probes)) {
    probe_file <- file.path(tmpdir, sprintf("probe_%02d.mp4", i))
    ret <- suppressWarnings(system2(ffmpeg, c(
      "-y", "-ss", sprintf("%.2f", positions[i]),
      "-i", shQuote(input),
      "-t", "1", "-c", "copy", "-an", shQuote(probe_file)
    ), stdout = FALSE, stderr = FALSE))
    if (ret == 0 && file.exists(probe_file)) {
      probes[i] <- probe_file
    }
  }
  probes[nzchar(probes)]
}

# Encode a single probe with CPU (SVT-AV1) at given CRF, return SSIM
.cpu_probe_ssim <- function(probe_input, crf) {
  ffmpeg <- Sys.which("ffmpeg")
  tmp_out <- tempfile(fileext = ".mp4")
  on.exit(unlink(tmp_out))

  ret <- suppressWarnings(system2(ffmpeg, c(
    "-y", "-i", shQuote(probe_input),
    "-c:v", "libsvtav1", "-crf", as.character(crf),
    "-pix_fmt", "yuv420p", "-svtav1-params", "preset=8",
    "-an", shQuote(tmp_out)
  ), stdout = FALSE, stderr = FALSE))

  if (ret != 0 || !file.exists(tmp_out)) return(NA_real_)
  measure_ssim(probe_input, tmp_out, duration = 1)
}

# Encode a single probe with Vulkan at given CRF, return SSIM
.vulkan_probe_ssim <- function(probe_input, crf) {
  tmp_out <- tempfile(fileext = ".mp4")
  on.exit(unlink(tmp_out))

  info <- .ffmpeg_video_info(probe_input)
  tryCatch({
    .Call("R_av1r_vulkan_encode",
          probe_input, tmp_out,
          info$width, info$height, info$fps, as.integer(crf),
          PACKAGE = "AV1R")
    if (!file.exists(tmp_out)) return(NA_real_)
    measure_ssim(probe_input, tmp_out, duration = 1)
  }, error = function(e) NA_real_)
}

# Average SSIM across all probes for a given encoder+CRF
.avg_probe_ssim <- function(probes, crf, encoder_fn) {
  ssims <- vapply(probes, function(p) encoder_fn(p, crf), numeric(1))
  ssims <- ssims[!is.na(ssims)]
  if (length(ssims) == 0) return(NA_real_)
  mean(ssims)
}

# Find Vulkan CRF that matches CPU SSIM via binary search on probes
.vulkan_probe_crf <- function(input, cpu_crf) {
  tmpdir <- tempfile("av1r_probe_")
  dir.create(tmpdir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)

  duration <- .ffmpeg_duration(input)
  if (is.na(duration)) duration <- 60
  n_probes <- .probe_count(duration)

  message(sprintf("AV1R [vulkan]: probing quality (%d sample%s, %.0f s video)...",
                  n_probes, if (n_probes > 1) "s" else "", duration))

  probes <- .extract_probes(input, n_probes, tmpdir)
  if (length(probes) == 0) {
    message("AV1R [vulkan]: probe extraction failed, using CRF as-is")
    return(cpu_crf)
  }

  # Step 1: get target SSIM from CPU at user's CRF
  target_ssim <- .avg_probe_ssim(probes, cpu_crf, .cpu_probe_ssim)
  if (is.na(target_ssim)) {
    message("AV1R [vulkan]: CPU probe failed, using CRF as-is")
    return(cpu_crf)
  }
  message(sprintf("AV1R [vulkan]: CPU target SSIM=%.4f at CRF %d", target_ssim, cpu_crf))

  # Step 2: binary search Vulkan CRF to match target SSIM
  lo <- 1L
  hi <- 63L
  best_crf  <- cpu_crf
  best_diff <- Inf

  prev_diff <- Inf
  for (iter in seq_len(6L)) {
    mid <- (lo + hi) %/% 2L
    ssim <- .avg_probe_ssim(probes, mid, .vulkan_probe_ssim)
    if (is.na(ssim)) break

    diff <- abs(ssim - target_ssim)
    message(sprintf("  Vulkan CRF=%d  SSIM=%.4f  (target=%.4f, diff=%.4f)",
                    mid, ssim, target_ssim, diff))

    if (diff < best_diff) {
      best_diff <- diff
      best_crf  <- mid
    }

    if (diff < 0.002) break  # close enough

    # Early exit: if diff not improving after 2 iterations, Vulkan can't reach target
    if (iter >= 2 && diff > 0.05 && abs(diff - prev_diff) < 0.01) {
      message("AV1R [vulkan]: SSIM not converging, using best found CRF")
      break
    }
    prev_diff <- diff

    if (ssim < target_ssim) {
      # Quality too low -> decrease CRF (better quality)
      hi <- mid
    } else {
      # Quality too high -> increase CRF (more compression)
      lo <- mid + 1L
    }

    if (lo >= hi) break
  }

  message(sprintf("AV1R [vulkan]: selected CRF=%d (best SSIM diff=%.4f)", best_crf, best_diff))
  best_crf
}

#' Measure SSIM quality score between two video files
#'
#' Compares encoded video against the original using SSIM.
#' Values close to 1.0 indicate high similarity.
#'
#' @param original Path to original video file.
#' @param encoded  Path to encoded video file.
#' @param duration Seconds to compare. \code{NULL} = full video.
#'
#' @return Numeric SSIM score (0-1), or \code{NA} on failure.
#' @export
measure_ssim <- function(original, encoded, duration = NULL) {
  ffmpeg <- Sys.which("ffmpeg")
  if (nchar(ffmpeg) == 0) stop("ffmpeg not found")

  t_args <- if (!is.null(duration)) c("-t", as.character(duration)) else character(0)

  lines <- suppressWarnings(system2(ffmpeg, c(
    "-i", shQuote(encoded),
    "-i", shQuote(original),
    "-lavfi", "ssim",
    t_args,
    "-f", "null", "-"
  ), stdout = TRUE, stderr = TRUE))

  ssim_line <- grep("SSIM", lines, value = TRUE)
  if (length(ssim_line) == 0) return(NA_real_)
  m <- regmatches(ssim_line, regexpr("All:[0-9.]+", ssim_line))
  if (length(m) == 0) return(NA_real_)
  as.numeric(sub("All:", "", m))
}
