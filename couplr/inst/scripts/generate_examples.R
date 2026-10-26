# =============================================================================
# Generate vignette demo assets for lapr (40x40 → 80x80 pipeline)
# =============================================================================
# Output directories:
#   inst/extdata/work    – working resized PNGs (40x40) from real photos
#   inst/extdata/morphs  – real image morphs (GIF+PNG, 80x80)
#   inst/extdata/icons   – simple circle morphs (inputs + GIF+PNG, 80x80)
#
# Notes:
#  * All LAP is done at 40x40.
#  * All saved outputs are 40x40 content upscaled to 80x80 (upscale = 2).
#  * Circles are drawn with a TRANSPARENT background so they look correct in
#    both light and dark themes.
#  * Script is idempotent: re-running overwrites the same files.
# =============================================================================

message("== lapr demo asset generator (40→80 pipeline) ==")

suppressPackageStartupMessages({
  library(magick)
  library(lapr)
})

# -----------------------------------------------------------------------------
# 0. Global configuration
# -----------------------------------------------------------------------------

BASE_SIZE <- 40L
UPSCALE   <- 2L

ext_dir   <- "inst/extdata"
work_dir  <- file.path(ext_dir, "work")
morph_dir <- file.path(ext_dir, "morphs")
icon_dir  <- file.path(ext_dir, "icons")

orig_A <- file.path(ext_dir, "ImageA.jpeg")
orig_B <- file.path(ext_dir, "ImageB.jpeg")

dir.create(work_dir,  recursive = TRUE, showWarnings = FALSE)
dir.create(morph_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(icon_dir,  recursive = TRUE, showWarnings = FALSE)

# -----------------------------------------------------------------------------
# Helpers
# -----------------------------------------------------------------------------

check_originals <- function(path_A, path_B) {
  if (!file.exists(path_A) || !file.exists(path_B)) {
    stop(
      "Missing originals. Expected:\n",
      " - ", path_A, "\n",
      " - ", path_B, "\n",
      "Put ImageA.jpeg and ImageB.jpeg there, then rerun."
    )
  }
  message("✓ Found originals: ImageA.jpeg / ImageB.jpeg")
}

make_work_pngs <- function(A_img, B_img, work_dir, base_size, upscale) {
  message("Creating 40x40 working PNGs from real photos ...")

  # 40x40 working images (used for LAP)
  A_40 <- image_resize(A_img, sprintf("%dx%d!", base_size, base_size))
  B_40 <- image_resize(B_img, sprintf("%dx%d!", base_size, base_size))

  work_A_40 <- file.path(work_dir, "ImageA_40.png")
  work_B_40 <- file.path(work_dir, "ImageB_40.png")

  image_write(A_40, path = work_A_40, format = "png")
  image_write(B_40, path = work_B_40, format = "png")

  message("✓ wrote ", work_A_40)
  message("✓ wrote ", work_B_40)

  # 80x80 upscaled versions for the vignette (static inputs)
  message("Creating 80x80 upscaled PNGs for vignette visuals ...")

  A_80 <- image_resize(A_40, sprintf("%dx%d!", base_size * upscale, base_size * upscale))
  B_80 <- image_resize(B_40, sprintf("%dx%d!", base_size * upscale, base_size * upscale))

  work_A_80 <- file.path(work_dir, "ImageA_80.png")
  work_B_80 <- file.path(work_dir, "ImageB_80.png")

  image_write(A_80, path = work_A_80, format = "png")
  image_write(B_80, path = work_B_80, format = "png")

  message("✓ wrote ", work_A_80)
  message("✓ wrote ", work_B_80)

  list(
    A_40 = work_A_40,
    B_40 = work_B_40,
    A_80 = work_A_80,
    B_80 = work_B_80
  )
}

# create two simple 40x40 circle inputs:
#   circleA_40.png = blue circle (left)
#   circleB_40.png = red circle (right)
# implemented via numeric masks -> as.raster() to avoid graphics devices
# background is fully transparent
ensure_circle_inputs <- function(icon_dir, base_size, upscale) {
  message("Preparing simple circle input assets (left blue, right red, transparent bg) ...")

  circA_40 <- file.path(icon_dir, "circleA_40.png")
  circB_40 <- file.path(icon_dir, "circleB_40.png")

  if (!file.exists(circA_40) || !file.exists(circB_40)) {
    message("  - Creating 40x40 circleA_40.png / circleB_40.png")

    make_circle_img <- function(cx, cy, r, col_fg, col_bg = "transparent") {
      n <- base_size
      x <- matrix(rep(1:n, each = n), nrow = n, ncol = n)
      y <- matrix(rep(1:n, times = n), nrow = n, ncol = n)

      mask <- (x - cx)^2 + (y - cy)^2 <= r^2

      cols <- matrix(col_bg, nrow = n, ncol = n)
      cols[mask] <- col_fg

      img <- magick::image_read(as.raster(cols))
      img
    }

    # left blue circle
    A_40_img <- make_circle_img(
      cx     = base_size * 0.25,
      cy     = base_size * 0.5,
      r      = base_size * 0.2,
      col_fg = "blue"
    )

    # right red circle
    B_40_img <- make_circle_img(
      cx     = base_size * 0.75,
      cy     = base_size * 0.5,
      r      = base_size * 0.2,
      col_fg = "red"
    )

    image_write(A_40_img, path = circA_40, format = "png")
    image_write(B_40_img, path = circB_40, format = "png")

    message("    ✓ wrote ", circA_40)
    message("    ✓ wrote ", circB_40)
  } else {
    message("  - Using existing circleA_40.png / circleB_40.png")
  }

  # Upscaled 80x80 versions
  circA_80 <- file.path(icon_dir, "circleA_80.png")
  circB_80 <- file.path(icon_dir, "circleB_80.png")

  A_40_img <- image_read(circA_40)
  B_40_img <- image_read(circB_40)

  A_80_img <- image_resize(A_40_img, sprintf("%dx%d!", base_size * upscale, base_size * upscale))
  B_80_img <- image_resize(B_40_img, sprintf("%dx%d!", base_size * upscale, base_size * upscale))

  image_write(A_80_img, path = circA_80, format = "png")
  image_write(B_80_img, path = circB_80, format = "png")

  message("    ✓ wrote ", circA_80)
  message("    ✓ wrote ", circB_80)

  list(
    A_40 = circA_40,
    B_40 = circB_40,
    A_80 = circA_80,
    B_80 = circB_80
  )
}

# Generate:
#   * an animated GIF via pixel_morph_animate (80x80 via upscale)
#   * a final-frame PNG via pixel_morph (also 80x80 via upscale)
run_morph_assets <- function(
  imgA,
  imgB,
  gif_out,
  png_out,
  n_frames,
  fps,
  upscale,
  morph_args
) {
  message("  - ", basename(gif_out), " & ", basename(png_out))

  # Animated morph
  do.call(
    lapr::pixel_morph_animate,
    c(
      list(
        imgA     = imgA,
        imgB     = imgB,
        n_frames = as.integer(n_frames),
        fps      = as.integer(fps),
        format   = "gif",
        outfile  = gif_out,
        show     = FALSE,
        upscale  = upscale
      ),
      morph_args
    )
  )

  # Static final frame (same settings, final frame only)
  final_img <- do.call(
    lapr::pixel_morph,
    c(
      list(
        imgA     = imgA,
        imgB     = imgB,
        n_frames = as.integer(n_frames),
        upscale  = upscale,
        show     = FALSE
      ),
      morph_args
    )
  )

  magick::image_write(final_img, path = png_out, format = "png")
}

# -----------------------------------------------------------------------------
# 1. Load originals and create working 40x40 PNGs (real-image demos)
# -----------------------------------------------------------------------------

check_originals(orig_A, orig_B)

A_img <- image_read(orig_A)
B_img <- image_read(orig_B)

work_paths <- make_work_pngs(A_img, B_img, work_dir, BASE_SIZE, UPSCALE)

work_A_40 <- work_paths$A_40
work_B_40 <- work_paths$B_40
# work_A_80 <- work_paths$A_80  # for vignette static inputs, not used here
# work_B_80 <- work_paths$B_80

# -----------------------------------------------------------------------------
# 2. Real-image morphs (color_walk, exact, recursive)
#    → GIF (animate) + PNG (final frame), 80x80 via upscale
# -----------------------------------------------------------------------------

message("Creating real-image morphs (40→80) ...")

real_specs <- list(
  color_walk = list(
    stem = "image_color_walk",
    args = list(
      mode          = "color_walk",
      lap_method    = "jv",
      quantize_bits = 6L
    ),
    n_frames = 20L
  ),
  exact = list(
    stem = "image_exact",
    args = list(
      mode       = "exact",
      lap_method = "jv",
      alpha      = 1,
      beta       = 0,
      patch_size = 1L
    ),
    n_frames = 20L
  ),
  recursive = list(
    stem = "image_recursive",
    args = list(
      mode       = "recursive",
      lap_method = "jv",
      alpha      = 1,
      beta       = 0,
      patch_size = 2L
    ),
    n_frames = 20L
  )
)

for (name in names(real_specs)) {
  spec <- real_specs[[name]]

  gif_out <- file.path(morph_dir, paste0(spec$stem, ".gif"))
  png_out <- file.path(morph_dir, paste0(spec$stem, ".png"))

  run_morph_assets(
    imgA       = work_A_40,
    imgB       = work_B_40,
    gif_out    = gif_out,
    png_out    = png_out,
    n_frames   = spec$n_frames,
    fps        = 10L,
    upscale    = UPSCALE,
    morph_args = spec$args
  )
}

message("✓ wrote image_[color_walk|exact|recursive].gif/.png (80x80)")

# -----------------------------------------------------------------------------
# 3. Circle input assets (40x40 + 80x80, transparent background)
# -----------------------------------------------------------------------------

circles <- ensure_circle_inputs(icon_dir, BASE_SIZE, UPSCALE)
circle_A_40 <- circles$A_40
circle_B_40 <- circles$B_40
# circle_A_80 <- circles$A_80  # for vignette static inputs, not used here
# circle_B_80 <- circles$B_80

# -----------------------------------------------------------------------------
# 4. Circle morphs (color_walk, exact, recursive)
#    → GIF (animate) + PNG (final frame), 80x80 via upscale
# -----------------------------------------------------------------------------

message("Creating circle morphs (40→80) ...")

circle_specs <- list(
  color_walk = list(
    stem = "circle_color_walk",
    args = list(
      mode          = "color_walk",
      lap_method    = "jv",
      quantize_bits = 6L
    ),
    n_frames = 20L
  ),
  exact = list(
    stem = "circle_exact",
    args = list(
      mode       = "exact",
      lap_method = "jv",
      alpha      = 1,
      beta       = 0,
      patch_size = 1L
    ),
    n_frames = 20L
  ),
  recursive = list(
    stem = "circle_recursive",
    args = list(
      mode       = "recursive",
      lap_method = "jv",
      alpha      = 1,
      beta       = 0,
      patch_size = 2L
    ),
    n_frames = 20L
  )
)

for (name in names(circle_specs)) {
  spec <- circle_specs[[name]]

  gif_out <- file.path(icon_dir, paste0(spec$stem, ".gif"))
  png_out <- file.path(icon_dir, paste0(spec$stem, ".png"))

  run_morph_assets(
    imgA       = circle_A_40,
    imgB       = circle_B_40,
    gif_out    = gif_out,
    png_out    = png_out,
    n_frames   = spec$n_frames,
    fps        = 10L,
    upscale    = UPSCALE,
    morph_args = spec$args
  )
}

message("✓ wrote circle_[color_walk|exact|recursive].gif/.png (80x80)")

message("== Done. All vignette assets regenerated (40→80, pixel_morph + pixel_morph_animate). ==")
