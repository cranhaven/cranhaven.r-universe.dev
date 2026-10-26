## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  echo       = FALSE,
  message    = FALSE,
  warning    = FALSE,
  fig.align  = "center",
  out.width  = "100%"
)

library(couplr)
library(knitr)

# Helper function to locate example assets - use local paths for vignettes
ext_demo <- function(...) {
  # Images are in vignettes/images/ for pkgdown compatibility
  # Takes nested path args and returns just the filename in images/
  args <- c(...)
  filename <- args[length(args)]  # Get the last element (the filename)
  file.path("images", filename)
}

## ----all-inputs, results='asis', echo=FALSE-----------------------------------
real_A   <- ext_demo("work",  "ImageA_80.png")
real_B   <- ext_demo("work",  "ImageB_80.png")
circle_A <- ext_demo("icons", "circleA_80.png")
circle_B <- ext_demo("icons", "circleB_80.png")

files <- c(real_A, real_B, circle_A, circle_B)
alts <- c("Source image A: photograph", "Target image B: photograph",
          "Source image A: circle icon", "Target image B: circle icon")

cat('<div class="pixel-row">\n')
for (i in seq_along(files)) {
  cat(sprintf('<img src="%s" alt="%s" />\n', files[i], alts[i]))
}
cat('</div>\n')

## ----exact-vis, results='asis', echo=FALSE------------------------------------
gif_image_exact  <- ext_demo("morphs", "image_exact.gif")
gif_circle_exact <- ext_demo("icons",  "circle_exact.gif")

files <- c(gif_image_exact, gif_circle_exact)
alts <- c("Animated GIF showing exact pixel morphing between two photographs",
          "Animated GIF showing exact pixel morphing between two circle icons")

cat('<div class="pixel-row">\n')
for (i in seq_along(files)) {
  cat(sprintf('<img src="%s" alt="%s" />\n', files[i], alts[i]))
}
cat('</div>\n')

## ----color-walk-vis, results='asis', echo=FALSE-------------------------------
gif_image_cw  <- ext_demo("morphs", "image_color_walk.gif")
gif_circle_cw <- ext_demo("icons",  "circle_color_walk.gif")

files <- c(gif_image_cw, gif_circle_cw)
alts <- c("Animated GIF showing feature quantization morphing between two photographs",
          "Animated GIF showing feature quantization morphing between two circle icons")

cat('<div class="pixel-row">\n')
for (i in seq_along(files)) {
  cat(sprintf('<img src="%s" alt="%s" />\n', files[i], alts[i]))
}
cat('</div>\n')

## ----recursive-vis, results='asis', echo=FALSE--------------------------------
gif_image_rec  <- ext_demo("morphs", "image_recursive.gif")
gif_circle_rec <- ext_demo("icons",  "circle_recursive.gif")

files <- c(gif_image_rec, gif_circle_rec)
alts <- c("Animated GIF showing hierarchical morphing between two photographs",
          "Animated GIF showing hierarchical morphing between two circle icons")

cat('<div class="pixel-row">\n')
for (i in seq_along(files)) {
  cat(sprintf('<img src="%s" alt="%s" />\n', files[i], alts[i]))
}
cat('</div>\n')

