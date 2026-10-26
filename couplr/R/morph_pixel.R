# R/pixel_morph.R
# Pixel-level image morphing with optional square or recursive tiling
#
# Modes:
# - "exact"      : pixel-level LAP on normalized color (+ optional small spatial beta)
#                  Global LAP for patch_size = 1, block-level LAP for patch_size > 1
# - "color_walk" : color-driven assignment (quantize, then match by color + spatial)
#                  Scales much better; warnings only for very large images
# - "recursive"  : multi-scale 2x2 recursive tiling, falling back to square-tiling
#                  at the smallest scale, no color blending (transport only)


# =============================================================================
# Public API
# =============================================================================

#' Pixel-level image morphing (animation)
#'
#' Creates an animated morph by computing optimal pixel assignment from image A 
#' to image B, then rendering intermediate frames showing the transport.
#'
#' @param imgA Source image (file path or magick image object)
#' @param imgB Target image (file path or magick image object)
#' @param n_frames Integer number of animation frames (default: 16)
#' @param fps Frames per second for playback (default: 10)
#' @param format Output format: "gif", "webp", or "mp4"
#' @param outfile Optional output file path
#' @param show Logical, display animation in viewer (default: interactive())
#' @param mode Assignment algorithm: "color_walk" (default), "exact",
#'   "recursive", or "color_match" (spatially match pixels of identical
#'   quantized colour, filling the remainder by identity)
#' @param lap_method LAP solver method (default: "jv")
#' @param maximize Logical, maximize instead of minimize cost (default: FALSE)
#' @param quantize_bits Color quantization for "color_walk" mode (default: 5)
#' @param downscale_steps Number of 2x reductions before computing assignment (default: 0)
#' @param alpha Weight for color distance in cost function (default: 1)
#' @param beta Weight for spatial distance in cost function (default: 0)
#' @param patch_size Tile size for tiled modes (default: 1)
#' @param upscale Post-rendering upscaling factor (default: 1)
#'
#' @return Invisibly returns a list with animation object and metadata:
#'   \item{animation}{magick animation object}
#'   \item{width}{Image width in pixels}
#'   \item{height}{Image height in pixels}  
#'   \item{assignment}{Integer vector of 1-based assignment indices (R convention)}
#'   \item{n_pixels}{Total number of pixels}
#'   \item{mode}{Mode used for matching}
#'   \item{upscale}{Upscaling factor applied}
#'
#' @details
#' ## Assignment vs Rendering Semantics
#' 
#' **CRITICAL:** This function has two separate phases with different semantics:
#' 
#' **Phase 1 - Assignment Computation:**
#' 
#' The assignment is computed by minimizing:
#' \preformatted{
#'   cost(i,j) = alpha * color_distance(A[i], B[j]) + 
#'               beta * spatial_distance(pos_i, pos_j)
#' }
#' 
#' This means B's COLORS influence which pixels from A map to which positions.
#' 
#' **Phase 2 - Rendering (Transport-Only):**
#' 
#' The renderer uses ONLY A's colors:
#' - Intermediate frames: A's pixels move along paths with motion blur
#' - Final frame: A's pixels at their assigned positions (sharp, no blur)
#' - B's colors NEVER appear in the output
#' 
#' **Result:** You get A's colors rearranged to match B's geometry/layout.
#' 
#' ## What This Means
#' 
#' - B influences WHERE pixels go (via similarity in cost function)
#' - B does NOT determine WHAT COLORS appear in output
#' - Final image has A's palette arranged to mimic B's structure
#' 
#' ## Parameter Guidance
#' 
#' **For pure spatial rearrangement (ignore B's colors in assignment):**
#' \preformatted{
#'   pixel_morph_animate(A, B, alpha = 0, beta = 1)
#' }
#' 
#' **For color-similarity matching (default):**
#' \preformatted{
#'   pixel_morph_animate(A, B, alpha = 1, beta = 0)
#' }
#' 
#' **For hybrid (color + spatial):**
#' \preformatted{
#'   pixel_morph_animate(A, B, alpha = 1, beta = 0.2)
#' }
#' 
#' ## Permutation Guarantees
#' 
#' Assignment is guaranteed to be a bijection (permutation) ONLY when:
#' - \code{downscale_steps = 0} (no resolution changes)
#' - \code{mode = "exact"} with \code{patch_size = 1}
#' 
#' With downscaling or tiled modes, assignment may have:
#' - **Overlaps:** Multiple source pixels map to same destination (last write wins)
#' - **Holes:** Some destinations never filled (remain transparent)
#' 
#' A warning is issued if overlaps/holes are detected in the final frame.
#'
#' @examples
#' if (requireNamespace("magick", quietly = TRUE)) {
#'   imgA <- system.file("extdata/icons/circleA_40.png", package = "couplr")
#'   imgB <- system.file("extdata/icons/circleB_40.png", package = "couplr")
#'   if (nzchar(imgA) && nzchar(imgB)) {
#'     outfile <- tempfile(fileext = ".gif")
#'     pixel_morph_animate(imgA, imgB, outfile = outfile, n_frames = 4, show = FALSE)
#'   }
#' }
#'
#' @export
pixel_morph_animate <- function(imgA,
                                imgB,
                                n_frames = 16L,
                                fps = 10L,
                                format = c("gif", "webp", "mp4"),
                                outfile = NULL,
                                show = interactive(),
                                mode = c("color_walk", "exact", "recursive", "color_match"),
                                lap_method = "jv",
                                maximize = FALSE,
                                quantize_bits = 5L,
                                downscale_steps = 0L,
                                alpha = 1,
                                beta  = 0,
                                patch_size = 1L,
                                upscale = 1) {
  format <- match.arg(format)

  eng <- .pixel_morph_engine(imgA, imgB, n_frames, mode, lap_method, maximize,
                             quantize_bits, downscale_steps, alpha, beta,
                             patch_size, upscale)
  frames        <- eng$frames
  H             <- eng$H
  W             <- eng$W
  n_frames      <- eng$n_frames
  upscale       <- eng$upscale
  assign_0based <- eng$assign_0based
  mode          <- eng$mode
  N             <- eng$N

  magick_list <- lapply(frames, function(fr) {
    Hf  <- fr$H
    Wf  <- fr$W
    vec <- fr$data
    Cch <- length(vec) / (Hf * Wf)

    if (!Cch %in% c(3, 4)) {
      stop("Unexpected channel count from morph_pixel_level_cpp: ", Cch)
    }

    arr <- array(vec / 255, dim = c(Hf, Wf, Cch))
    magick::image_read(arr)
  })

  magick_frames <- magick::image_join(magick_list)

  # Optional upscaling of rendered frames (display only, LAP already done)
  if (upscale != 1) {
    if (abs(upscale - round(upscale)) < 1e-9) {
      target_width  <- as.integer(round(W * upscale))
      target_height <- as.integer(round(H * upscale))
      magick_frames <- magick::image_scale(
        magick_frames,
        geometry = sprintf("%dx%d!", target_width, target_height)
      )
    } else {
      pct <- upscale * 100
      magick_frames <- magick::image_scale(
        magick_frames,
        geometry = sprintf("%.2f%%", pct)
      )
    }
  }

  # Per-frame delays so last frame lingers a bit longer
  base_delay <- 100 / fps

  if (n_frames <= 1L) {
    delays <- base_delay
  } else {
    delays <- rep(base_delay, n_frames)
    linger_delay <- base_delay * (n_frames - 1L) / 3
    delays[n_frames] <- linger_delay
  }

  animation <- magick::image_animate(
    magick_frames,
    delay   = delays,
    dispose = "background",
    loop    = 0
  )

  # Save to file if requested
  if (!is.null(outfile)) {
    if (format == "mp4") {
      if (!.has_namespace("av")) {
        stop("Package 'av' is required for mp4 output.")
      }
      temp_dir   <- tempdir()
      temp_files <- file.path(
        temp_dir,
        sprintf("frame_%04d.png", seq_len(n_frames))
      )

      for (i in seq_len(n_frames)) {
        frame_i <- magick_frames[i]
        magick::image_write(frame_i, temp_files[i], format = "png")
      }

      av::av_encode_video(
        temp_files,
        output    = outfile,
        framerate = fps
      )
      unlink(temp_files)

    } else if (format == "webp") {
      magick::image_write(animation, path = outfile, format = "webp")

    } else if (format == "gif") {
      magick::image_write(animation, path = outfile, format = "gif")

    } else {
      stop("Unknown format: ", format)
    }

    message("Saved animation to: ", outfile)
  }

  # Show in viewer if requested
  if (show) {
    print(animation)
  }

  invisible(list(
    animation  = animation,
    width      = W,
    height     = H,
    assignment = assign_0based + 1L,  # Return 1-based for R convention
    n_pixels   = N,
    mode       = mode,
    upscale    = upscale
  ))
}

#' Pixel-level image morphing (final frame only)
#'
#' Computes optimal pixel assignment from A to B and returns the final 
#' transported frame (without intermediate animation frames).
#'
#' @param imgA Source image (file path or magick image object)
#' @param imgB Target image (file path or magick image object)
#' @param n_frames Internal parameter for rendering (default: 16)
#' @param mode Assignment algorithm: "color_walk" (default), "exact",
#'   "recursive", or "color_match" (spatially match pixels of identical
#'   quantized colour, filling the remainder by identity)
#' @param lap_method LAP solver method (default: "jv")
#' @param maximize Logical, maximize instead of minimize cost (default: FALSE)
#' @param quantize_bits Color quantization for "color_walk" mode (default: 5)
#' @param downscale_steps Number of 2x reductions before computing assignment (default: 0)
#' @param alpha Weight for color distance in cost function (default: 1)
#' @param beta Weight for spatial distance in cost function (default: 0)
#' @param patch_size Tile size for tiled modes (default: 1)
#' @param upscale Post-rendering upscaling factor (default: 1)
#' @param show Logical, display result in viewer (default: interactive())
#'
#' @return magick image object of the final transported frame
#'
#' @details
#' ## Transport-Only Semantics
#' 
#' This function returns a SHARP, pixel-perfect transport of A's pixels
#' to positions determined by the assignment to B.
#' 
#' **Key Points:**
#' - Assignment computed using: \code{cost = alpha * color_dist + beta * spatial_dist}
#' - B's COLORS influence assignment but DO NOT appear in output
#' - Result has A's colors arranged to match B's layout
#' - No motion blur (unlike intermediate frames in animation)
#' 
#' See \code{\link{pixel_morph_animate}} for detailed explanation of 
#' assignment vs rendering semantics.
#' 
#' ## Permutation Warnings
#' 
#' Assignment is guaranteed to be a bijection (permutation) ONLY when:
#' - \code{downscale_steps = 0} (no resolution changes)
#' - \code{mode = "exact"} with \code{patch_size = 1}
#' 
#' With downscaling or tiled modes, assignment may have:
#' - **Overlaps:** Multiple source pixels map to same destination (last write wins)
#' - **Holes:** Some destinations never filled (remain transparent)
#' 
#' If assignment is not a bijection (due to downscaling or tiling),
#' a warning will be issued. The result may contain:
#' - Overlapped pixels (multiple sources -> one destination)
#' - Transparent holes (some destinations unfilled)
#' 
#' For guaranteed pixel-perfect results, use:
#' \preformatted{
#'   pixel_morph(A, B, mode = "exact", downscale_steps = 0)
#' }
#'
#' @examples
#' if (requireNamespace("magick", quietly = TRUE)) {
#'   imgA <- system.file("extdata/icons/circleA_40.png", package = "couplr")
#'   imgB <- system.file("extdata/icons/circleB_40.png", package = "couplr")
#'   if (nzchar(imgA) && nzchar(imgB)) {
#'     result <- pixel_morph(imgA, imgB, n_frames = 4, show = FALSE)
#'   }
#' }
#'
#' @seealso \code{\link{pixel_morph_animate}} for animated version
#'
#' @export
pixel_morph <- function(imgA,
                        imgB,
                        n_frames = 16L,
                        mode = c("color_walk", "exact", "recursive", "color_match"),
                        lap_method = "jv",
                        maximize = FALSE,
                        quantize_bits = 5L,
                        downscale_steps = 0L,
                        alpha = 1,
                        beta  = 0,
                        patch_size = 1L,
                        upscale = 1,
                        show = interactive()) {
  eng <- .pixel_morph_engine(imgA, imgB, n_frames, mode, lap_method, maximize,
                             quantize_bits, downscale_steps, alpha, beta,
                             patch_size, upscale)
  frames   <- eng$frames
  H        <- eng$H
  W        <- eng$W
  n_frames <- eng$n_frames
  upscale  <- eng$upscale

  stopifnot(length(frames) == n_frames)

  last_fr <- frames[[length(frames)]]
  Hf      <- last_fr$H
  Wf      <- last_fr$W
  vec     <- last_fr$data
  Cch     <- length(vec) / (Hf * Wf)

  if (!Cch %in% c(3, 4)) {
    stop("Unexpected channel count from morph_pixel_level_cpp: ", Cch)
  }

  arr <- array(vec / 255, dim = c(Hf, Wf, Cch))
  final_img <- magick::image_read(arr)

  # Optional upscaling of final frame
  if (upscale != 1) {
    if (abs(upscale - round(upscale)) < 1e-9) {
      target_width  <- as.integer(round(W * upscale))
      target_height <- as.integer(round(H * upscale))
      final_img <- magick::image_scale(
        final_img,
        geometry = sprintf("%dx%d!", target_width, target_height)
      )
    } else {
      pct <- upscale * 100
      final_img <- magick::image_scale(
        final_img,
        geometry = sprintf("%.2f%%", pct)
      )
    }
  }

  if (show) {
    print(final_img)
  }

  invisible(final_img)
}


# =============================================================================
# Core solvers
# =============================================================================

#' Shared compute core for pixel_morph() and pixel_morph_animate()
#'
#' Validates arguments, reads and aligns the two images, computes the optimal
#' pixel assignment (optionally at a downscaled resolution), and renders the
#' transport frames via morph_pixel_level_cpp(). The two public front doors
#' differ only in how they present these frames -- a single final image versus
#' an animation -- so everything up to and including frame rendering lives here.
#'
#' Returns a list with: frames, H, W, n_frames (clamped), upscale (clamped),
#' assign_0based, mode (matched), N.
#' @noRd
.pixel_morph_engine <- function(imgA, imgB, n_frames, mode, lap_method, maximize,
                                quantize_bits, downscale_steps, alpha, beta,
                                patch_size, upscale) {
  mode <- match.arg(mode, c("color_walk", "exact", "recursive", "color_match"))

  # Robust input validation
  if (!is.numeric(upscale) || length(upscale) != 1 || is.na(upscale)) {
    stop("upscale must be a single numeric value", call. = FALSE)
  }
  upscale <- as.numeric(upscale)
  if (upscale <= 0) {
    warning("upscale must be positive, setting to 1", call. = FALSE)
    upscale <- 1
  }

  if (!is.numeric(n_frames) || length(n_frames) != 1 || is.na(n_frames)) {
    stop("n_frames must be a single numeric value", call. = FALSE)
  }
  n_frames <- as.integer(n_frames)
  if (n_frames < 2L) {
    warning("n_frames must be at least 2, setting to 2", call. = FALSE)
    n_frames <- 2L
  }

  if (!is.numeric(alpha) || length(alpha) != 1 || is.na(alpha) || alpha < 0) {
    stop("alpha must be a single non-negative numeric value", call. = FALSE)
  }
  if (!is.numeric(beta) || length(beta) != 1 || is.na(beta) || beta < 0) {
    stop("beta must be a single non-negative numeric value", call. = FALSE)
  }
  if (alpha == 0 && beta == 0) {
    stop("alpha and beta cannot both be zero", call. = FALSE)
  }

  if (!is.numeric(patch_size) || length(patch_size) != 1 || is.na(patch_size)) {
    stop("patch_size must be a single numeric value", call. = FALSE)
  }
  patch_size <- as.integer(patch_size)
  if (patch_size < 1L) {
    stop("patch_size must be at least 1", call. = FALSE)
  }

  if (!is.numeric(downscale_steps) || length(downscale_steps) != 1 || is.na(downscale_steps)) {
    stop("downscale_steps must be a single numeric value", call. = FALSE)
  }
  downscale_steps <- as.integer(downscale_steps)
  if (downscale_steps < 0L) {
    stop("downscale_steps must be non-negative", call. = FALSE)
  }

  if (!.has_namespace("magick")) stop("Package 'magick' is required.")

  # Read images and align sizes
  A <- if (is.character(imgA)) magick::image_read(imgA) else imgA
  B <- if (is.character(imgB)) magick::image_read(imgB) else imgB

  infoA <- magick::image_info(A)
  infoB <- magick::image_info(B)

  if (infoA$width != infoB$width || infoA$height != infoB$height) {
    B <- magick::image_resize(
      B,
      geometry = sprintf("%dx%d!", infoA$width, infoA$height)
    )
    infoB <- magick::image_info(B)
  }

  H <- as.integer(infoA$height)
  W <- as.integer(infoA$width)
  N <- H * W

  # Planar RGB buffers (0-255, column-major)
  arrA    <- .to_array_rgb(A)
  arrB    <- .to_array_rgb(B)
  planarA <- .to_planar_rgb(arrA)
  planarB <- .to_planar_rgb(arrB)

  # Optional downscaling for assignment computation
  ds   <- .downscale_both(planarA, planarB, H, W, steps = downscale_steps)
  Hs   <- ds$Hs
  Ws   <- ds$Ws
  A_s  <- ds$A_s
  B_s  <- ds$B_s
  Ns   <- Hs * Ws

  # Size checks based on mode (after downscaling)
  patch_size <- as.integer(patch_size)
  if (patch_size < 1L) patch_size <- 1L

  if (mode %in% c("exact", "recursive")) {

    if (patch_size <= 1L && mode == "exact") {
      MAX_EXACT <- 4096L
      if (Ns > MAX_EXACT) {
        warning(sprintf(
          paste0(
            "Image is large for 'exact' global LAP: %d x %d = %d pixels (recommended max %d).\n",
            "Computation may be slow -- consider using patch_size > 1, mode='recursive',",
            " or mode='color_walk'."
          ),
          Hs, Ws, Ns, MAX_EXACT
        ), call. = FALSE)
      }

    } else {
      tile_n <- patch_size * patch_size
      MAX_TILE_EXACT <- 400L

      if (tile_n > MAX_TILE_EXACT) {
        warning(sprintf(
          paste0(
            "Tile is large for LAP: patch_size=%d -> %d pixels per tile (recommended max %d).\n",
            "Computation may be slow -- reduce patch_size or use mode='color_walk'."
          ),
          patch_size, tile_n, MAX_TILE_EXACT
        ), call. = FALSE)
      }
    }

  } else if (mode %in% c("color_walk", "color_match")) {

    WARN_THRESHOLD <- 250000L
    if (Ns > WARN_THRESHOLD) {
      warning(sprintf(
        paste0(
          "Large image: %d x %d = %d pixels. This may take a while.\n",
          "Consider increasing downscale_steps or reducing image size."
        ),
        Hs, Ws, Ns
      ), call. = FALSE)
    }
  }

  # Compute pixel assignment at (Hs, Ws)
  assign_s <- .solve_pixel_assignment(mode, A_s, B_s, Hs, Ws, patch_size,
                                      alpha, beta, lap_method, maximize,
                                      quantize_bits)

  # 1-based -> 0-based, then upscale back to full resolution if downscaled
  assign_in <- as.integer(assign_s) - 1L
  if (downscale_steps > 0L && (Hs != H || Ws != W)) {
    assign_0based <- .upscale_assignment(assign_in, H = H, W = W, Hs = Hs, Ws = Ws)
  } else {
    assign_0based <- assign_in
  }

  # Render morph frames via C++ (assignment is 0-based here). The final frame is
  # a sharp, transport-only render; intermediate frames use bilinear splatting.
  frames <- morph_pixel_level_cpp(planarA, planarB, assign_0based, H, W, n_frames)

  list(frames = frames, H = H, W = W, n_frames = n_frames, upscale = upscale,
       assign_0based = assign_0based, mode = mode, N = N)
}

#' Dispatch a pixel-morph mode to its solver (shared by pixel_morph and
#' pixel_morph_animate)
#' @noRd
.solve_pixel_assignment <- function(mode, A_s, B_s, Hs, Ws, patch_size,
                                    alpha, beta, lap_method, maximize,
                                    quantize_bits) {
  if (mode == "exact") {
    if (patch_size > 1L) {
      .square_tiling_solver(
        A_planar = A_s, B_planar = B_s, H = Hs, W = Ws,
        max_tile_size = patch_size, alpha = alpha, beta = beta,
        method = lap_method, maximize = maximize
      )
    } else {
      .exact_cost_and_solve(A_s, B_s, Hs, Ws, alpha, beta, lap_method, maximize)
    }
  } else if (mode == "recursive") {
    .recursive_tiling_solver(
      A_planar = A_s, B_planar = B_s, H = Hs, W = Ws,
      patch_size = patch_size, alpha = alpha, beta = beta,
      method = lap_method, maximize = maximize
    )
  } else if (mode == "color_match") {
    # Identity-palette matching: pixels sharing a quantized colour are matched
    # spatially, and any left over fall back to identity.
    .solve_color_match_pipeline(A_s, B_s, Hs, Ws, quantize_bits, lap_method, maximize)
  } else {
    # color_walk mode - positional arguments matching the function signature
    .solve_color_walk_pipeline(A_s, B_s, Hs, Ws, quantize_bits, lap_method, maximize)
  }
}

#' Recursive tiling solver: multi-scale 2x2 splitting + square tiling at leaves
#' @noRd
.recursive_tiling_solver <- function(A_planar, B_planar, H, W,
                                     patch_size = 3L,
                                     alpha = 1, beta = 0,
                                     method = "jv", maximize = FALSE) {

  N <- H * W
  patch_size <- as.integer(patch_size)
  if (patch_size < 1L) patch_size <- 1L

  idx_cm <- function(x, y, H) x * H + y + 1L

  assignment <- rep(NA_integer_, N)
  N_global   <- N

  # Helper: solve a sub-block via square-tiling, then map back to global indices
  solve_small_block <- function(src_x0, src_y0, dst_x0, dst_y0, w, h) {
    if (w <= 0 || h <= 0) return()

    N_local <- w * h
    eff_tile <- min(patch_size, w, h)

    A_sub <- numeric(3L * N_local)
    B_sub <- numeric(3L * N_local)

    for (xl in 0:(w - 1L)) {
      for (yl in 0:(h - 1L)) {
        xg_src <- src_x0 + xl
        yg_src <- src_y0 + yl
        xg_dst <- dst_x0 + xl
        yg_dst <- dst_y0 + yl

        ia <- idx_cm(xg_src, yg_src, H)
        ib <- idx_cm(xg_dst, yg_dst, H)

        il <- xl * h + yl + 1L

        A_sub[il]                 <- A_planar[ia]
        A_sub[il + N_local]       <- A_planar[ia + N_global]
        A_sub[il + 2L * N_local]  <- A_planar[ia + 2L * N_global]

        B_sub[il]                 <- B_planar[ib]
        B_sub[il + N_local]       <- B_planar[ib + N_global]
        B_sub[il + 2L * N_local]  <- B_planar[ib + 2L * N_global]
      }
    }

    local_asg <- .square_tiling_solver(
      A_planar      = A_sub,
      B_planar      = B_sub,
      H             = h,
      W             = w,
      max_tile_size = eff_tile,
      alpha         = alpha,
      beta          = beta,
      method        = method,
      maximize      = maximize
    )

    for (xl in 0:(w - 1L)) {
      for (yl in 0:(h - 1L)) {
        il <- xl * h + yl + 1L
        j  <- local_asg[il]

        xl2 <- (j - 1L) %/% h
        yl2 <- (j - 1L) %%  h

        xg_src <- src_x0 + xl
        yg_src <- src_y0 + yl
        xg_dst <- dst_x0 + xl2
        yg_dst <- dst_y0 + yl2

        ia <- idx_cm(xg_src, yg_src, H)
        ib <- idx_cm(xg_dst, yg_dst, H)

        assignment[ia] <<- ib
      }
    }
  }

  # Main recursive solver for a block
  solve_block <- function(src_x0, src_y0, dst_x0, dst_y0, w, h) {
    if (w <= patch_size || h <= patch_size) {
      solve_small_block(src_x0, src_y0, dst_x0, dst_y0, w, h)
      return(invisible(NULL))
    }

    main_w <- w - (w %% 2L)
    main_h <- h - (h %% 2L)

    if (main_w < 2L || main_h < 2L) {
      solve_small_block(src_x0, src_y0, dst_x0, dst_y0, w, h)
      return(invisible(NULL))
    }

    tile_w <- main_w %/% 2L
    tile_h <- main_h %/% 2L

    tilesA <- list(
      list(x0 = src_x0,             y0 = src_y0,             w = tile_w, h = tile_h),
      list(x0 = src_x0 + tile_w,    y0 = src_y0,             w = tile_w, h = tile_h),
      list(x0 = src_x0,             y0 = src_y0 + tile_h,    w = tile_w, h = tile_h),
      list(x0 = src_x0 + tile_w,    y0 = src_y0 + tile_h,    w = tile_w, h = tile_h)
    )

    tilesB <- list(
      list(x0 = dst_x0,             y0 = dst_y0,             w = tile_w, h = tile_h),
      list(x0 = dst_x0 + tile_w,    y0 = dst_y0,             w = tile_w, h = tile_h),
      list(x0 = dst_x0,             y0 = dst_y0 + tile_h,    w = tile_w, h = tile_h),
      list(x0 = dst_x0 + tile_w,    y0 = dst_y0 + tile_h,    w = tile_w, h = tile_h)
    )

    tile_stats <- function(tiles, is_A) {
      cols <- matrix(0, 4L, 3L)
      ctrs <- matrix(0, 4L, 2L)

      for (k in 1:4) {
        tx <- tiles[[k]]$x0
        ty <- tiles[[k]]$y0
        tw <- tiles[[k]]$w
        th <- tiles[[k]]$h

        ctrs[k, 1] <- tx + (tw - 1) / 2
        ctrs[k, 2] <- ty + (th - 1) / 2

        idxs  <- integer(tw * th)
        c_idx <- 0L
        for (dx in 0:(tw - 1L)) {
          for (dy in 0:(th - 1L)) {
            x <- tx + dx
            y <- ty + dy
            c_idx <- c_idx + 1L
            idxs[c_idx] <- idx_cm(x, y, H)
          }
        }

        if (is_A) {
          cols[k, ] <- c(
            mean(A_planar[idxs])                   / 255,
            mean(A_planar[idxs + N_global])        / 255,
            mean(A_planar[idxs + 2L * N_global])   / 255
          )
        } else {
          cols[k, ] <- c(
            mean(B_planar[idxs])                   / 255,
            mean(B_planar[idxs + N_global])        / 255,
            mean(B_planar[idxs + 2L * N_global])   / 255
          )
        }
      }

      list(cols = cols, ctrs = ctrs)
    }

    A_stats <- tile_stats(tilesA, is_A = TRUE)
    B_stats <- tile_stats(tilesB, is_A = FALSE)

    colors_A <- A_stats$cols
    colors_B <- B_stats$cols
    centersA <- A_stats$ctrs
    centersB <- B_stats$ctrs

    diag_norm <- sqrt(H^2 + W^2)

    dc_mat <- matrix(0, 4L, 4L)
    ds_mat <- matrix(0, 4L, 4L)

    for (i in 1:4) {
      for (j in 1:4) {
        dc <- sqrt(sum((colors_A[i, ] - colors_B[j, ])^2))
        ds <- sqrt(sum((centersA[i, ] - centersB[j, ])^2)) / diag_norm
        dc_mat[i, j] <- dc
        ds_mat[i, j] <- ds
      }
    }

    mean_dc <- mean(dc_mat)
    mean_ds <- mean(ds_mat)
    if (mean_dc <= 0) mean_dc <- 1
    if (mean_ds <= 0) mean_ds <- 1

    dc_norm <- dc_mat / mean_dc
    ds_norm <- ds_mat / mean_ds

    C <- alpha * dc_norm + beta * ds_norm

    perm0 <- .lap_assign(C, method = method, maximize = maximize)
    perm  <- as.integer(perm0) + 1L

    for (i in 1:4) {
      src_tile <- tilesA[[i]]
      dst_tile <- tilesB[[perm[i]]]
      solve_block(
        src_x0 = src_tile$x0,
        src_y0 = src_tile$y0,
        dst_x0 = dst_tile$x0,
        dst_y0 = dst_tile$y0,
        w      = src_tile$w,
        h      = src_tile$h
      )
    }

    rest_w <- w - main_w
    rest_h <- h - main_h

    if (rest_w > 0L) {
      solve_block(
        src_x0 = src_x0 + main_w,
        src_y0 = src_y0,
        dst_x0 = dst_x0 + main_w,
        dst_y0 = dst_y0,
        w      = rest_w,
        h      = main_h
      )
    }

    if (rest_h > 0L) {
      solve_block(
        src_x0 = src_x0,
        src_y0 = src_y0 + main_h,
        dst_x0 = dst_x0,
        dst_y0 = dst_y0 + main_h,
        w      = main_w,
        h      = rest_h
      )
    }

    if (rest_w > 0L && rest_h > 0L) {
      solve_block(
        src_x0 = src_x0 + main_w,
        src_y0 = src_y0 + main_h,
        dst_x0 = dst_x0 + main_w,
        dst_y0 = dst_y0 + main_h,
        w      = rest_w,
        h      = rest_h
      )
    }

    invisible(NULL)
  }

  solve_block(0L, 0L, 0L, 0L, W, H)

  na_idx <- which(is.na(assignment))
  if (length(na_idx)) {
    assignment[na_idx] <- na_idx
  }

  assignment
}
