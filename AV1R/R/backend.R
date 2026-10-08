#' Detect best available encoding backend
#'
#' Priority order: \code{"vaapi"} (VAAPI AV1 GPU via ffmpeg, best
#' speed/quality ratio) > \code{"vulkan"} (Vulkan AV1 GPU, fastest but
#' CQP only) > \code{"cpu"} (SVT-AV1 via ffmpeg).
#'
#' @param prefer \code{"auto"} (default), \code{"vaapi"}, \code{"vulkan"},
#'   or \code{"cpu"}.
#'
#' @return Character string: \code{"vaapi"}, \code{"vulkan"}, or \code{"cpu"}.
#' @export
detect_backend <- function(prefer = "auto") {
  prefer <- match.arg(prefer, c("auto", "vulkan", "vaapi", "cpu"))
  if (prefer == "cpu")    return("cpu")
  if (prefer == "vulkan") {
    vk <- tryCatch(
      .Call("R_av1r_detect_backend", "vulkan", PACKAGE = "AV1R"),
      error = function(e) "cpu"
    )
    return(if (vk == "vulkan") "vulkan" else "cpu")
  }
  if (prefer == "vaapi")  return(if (.vaapi_av1_available()) "vaapi" else "cpu")
  # prefer == "auto": VAAPI first (best speed/quality), then Vulkan, then CPU
  if (.vaapi_av1_available()) return("vaapi")
  vk <- tryCatch(
    .Call("R_av1r_detect_backend", "auto", PACKAGE = "AV1R"),
    error = function(e) "cpu"
  )
  if (vk == "vulkan") return("vulkan")
  if (.has_ffmpeg_encoder("libsvtav1")) return("cpu")
  "cpu"
}

# Internal: check if ffmpeg has a given encoder
.has_ffmpeg_encoder <- function(encoder_name) {
  ffmpeg <- Sys.which("ffmpeg")
  if (nchar(ffmpeg) == 0) return(FALSE)
  encoders <- tryCatch(
    system2(ffmpeg, c("-encoders", "-v", "quiet"), stdout = TRUE, stderr = FALSE),
    error = function(e) character(0)
  )
  any(grepl(encoder_name, encoders, fixed = TRUE))
}

# Internal: check VAAPI AV1 encode via ffmpeg
# vainfo may not list AV1 encode on RADV/Mesa even when it works via ffmpeg
.vaapi_av1_available <- function() {
  ffmpeg <- Sys.which("ffmpeg")
  if (nchar(ffmpeg) == 0) return(FALSE)
  # Check if ffmpeg has av1_vaapi encoder
  if (!.has_ffmpeg_encoder("av1_vaapi")) return(FALSE)
  # Verify /dev/dri/renderD128 exists (Linux VAAPI device)
  file.exists("/dev/dri/renderD128")
}

#' Show AV1R backend status
#'
#' Prints the active encoding backend and available GPU/CPU capabilities.
#'
#' @return Invisibly returns the active backend string.
#' @export
av1r_status <- function() {
  bk <- detect_backend()
  cat("backend:", bk, "\n")
  invisible(bk)
}

#' Check Vulkan AV1 availability
#'
#' @return \code{TRUE} if the package was compiled with Vulkan AV1 encode support.
#' @export
vulkan_available <- function() {
  tryCatch(.Call("R_av1r_vulkan_available", PACKAGE = "AV1R"), error = function(e) FALSE)
}

#' List Vulkan-capable GPU devices
#'
#' @return Character vector of device names. Devices supporting
#'   \code{VK_KHR_VIDEO_ENCODE_AV1} are marked with \code{[AV1]}.
#' @export
vulkan_devices <- function() {
  tryCatch(.Call("R_av1r_vulkan_devices", PACKAGE = "AV1R"), error = function(e) character(0))
}
