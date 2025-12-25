## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(hilbert)

## -----------------------------------------------------------------------------
data("quakes")
quakes <- quakes[quakes$long < 179.5, ]

## ---- echo = FALSE------------------------------------------------------------
knitr::kable(
  head(quakes),
  format = "html",
  align = "c",
  table.attr = "style='width:100%;'",
  caption = "*Sample of Seismic Events (w/ MB > 4.0) Near Fiji Since 1964*"
)

## ---- echo = FALSE, out.width="100%", out.extra='style="border:none;pointer-events:none;user-select:none;"'----
knitr::include_graphics("1.png")

## -----------------------------------------------------------------------------
extent <- c(xmin = min(quakes$long), ymin = min(quakes$lat),
            xmax = max(quakes$long), ymax = max(quakes$lat))

## ---- echo = FALSE------------------------------------------------------------
knitr::kable(
  t(extent),
  format = "html",
  col.names = c("X-Min", "Y-Min", "X-Max", "Y-Max"),
  align = "c",
  table.attr = "style='width:100%;'",
  caption = "*Quakes Extent*"
)

## -----------------------------------------------------------------------------
positions <- hilbert::coords_to_position(
    x      = quakes,           # Using the `data.frame` method
    coords = c("long", "lat"), # Either the column names or indices for the coordinates
    n      = 4L,               # Dimensions exponent, i.e. 2^n x 2^n grid
    extent = extent,           # The previously found extent
    attach = FALSE             # When `TRUE`, attaches to the original `data.frame`
)

## ---- echo = FALSE------------------------------------------------------------
tmp <- t(head(positions, 21))
tmp[, 21] <- c("...", "...")
rownames(tmp) <- c("**X-Position**", "**Y-Position**")
knitr::kable(tmp, col.names = NULL, row.names = TRUE)

## ---- echo = FALSE, out.width="100%", out.extra='style="border:none;pointer-events:none;user-select:none;"'----
knitr::include_graphics("2.png")

## -----------------------------------------------------------------------------
indices <- hilbert::index(positions, coords = c(1, 2), n = 4L)

## ---- echo = FALSE------------------------------------------------------------
tmp <- t(head(indices, 21))
tmp[, 21] <- c("...", "...", "...")
rownames(tmp) <- c("**X-Position**", "**Y-Position**", "**Index**")
knitr::kable(tmp, col.names = NULL, row.names = TRUE)

## ---- echo = FALSE, out.width="100%", out.extra='style="border:none;pointer-events:none;user-select:none;"'----
knitr::include_graphics("3.png")

