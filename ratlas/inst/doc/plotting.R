## ----chunk-options, include=FALSE---------------------------------------------
if (requireNamespace("pkgdown", quietly = TRUE) && pkgdown::in_pkgdown()) {
  tiny_width <- small_width <- med_width <- 7
  large_width <- 8
} else {
  tiny_width <- small_width <- med_width <- 5
  large_width <- 5.5
}

knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.asp = 0.618,
  fig.width = small_width,
  fig.align = "center",
  out.width = "90%"
)

if (capabilities("cairo") && Sys.info()[['sysname']] != "Darwin") {
  knitr::opts_chunk$set(
    dev = "png",
    dev.args = list(type = "cairo")
  )
}

## ----setup, include = FALSE---------------------------------------------------
library(ratlas)
library(dplyr)
library(ggplot2)

library(scales)
library(viridis)
library(dichromat)
library(colorspace)

library(systemfonts)
require_font("Arial", fallback = "sans")

## ----init-plot, fig.cap = "(ref:init-plot-cap)"-------------------------------
library(ggplot2)

ggplot(mtcars, aes(x = mpg, y = disp)) +
  geom_point(aes(color = factor(cyl)))

## ----theme-atlas-ex, fig.cap = "(ref:theme-atlas-ex-cap)"---------------------
library(ratlas)

ggplot(mtcars, aes(x = mpg, y = disp)) +
  geom_point(aes(color = factor(cyl))) +
  theme_atlas()

## ----showtxtfnt---------------------------------------------------------------
library(systemfonts)
require_font("Montserrat")

## ----theme-atlas-ms-ex-build, eval = FALSE------------------------------------
# ggplot(mtcars, aes(x = mpg, y = disp)) +
#   geom_point(aes(color = factor(cyl))) +
#   theme_atlas(base_family = "Montserrat")

## ----colorblindr-funcs, include = FALSE---------------------------------------
# This is a placeholder until colorblindr is on CRAN. When that happens,
# colorblindr can be added to suggests, and we can just use
# colorblindr::cvd_grid()
edit_colors <- function(plot = last_plot(), colfun = passthrough, fillfun = NULL, ...)
{
  # convert to grob if necessary
  if (!methods::is(plot, "grob")) {
    plot <- cowplot::plot_to_gtable(plot)
  }

  if (is.null(fillfun)) {
    fillfun = colfun
  }
  edit_grob_colors(plot, colfun, fillfun, ...)
}

edit_grob_colors <- function(grob, colfun, fillfun, ...)
{
  if (!is.null(grob$gp)) {
    if (!is.null(grob$gp$col)) {
      grob$gp$col <- colfun(grob$gp$col, ...)
    }
    if (!is.null(grob$gp$fill)) {
      grob$gp$fill <- fillfun(grob$gp$fill, ...)
    }
  }

  if (!is.null(grob$grobs)) {
    grob$grobs <- lapply(grob$grobs, edit_grob_colors, colfun, fillfun, ...)
  }

  if (!is.null(grob$children)) {
    grob$children <- lapply(grob$children, edit_grob_colors, colfun, fillfun, ...)
  }

  if (methods::is(grob, "rastergrob")) {
    grob <- edit_rastergrob_colors(grob, colfun, ...)
  }

  grob
}

edit_rastergrob_colors <- function(grob, colfun, ...)
{
  rasternew <- colfun(c(grob$raster), ...)
  dim(rasternew) <- dim(grob$raster)
  class(rasternew) <- class(grob$raster)
  grid::editGrob(grob, raster = rasternew)
}

cvd_grid <- function(plot = last_plot(), severity = 1)
{
  deut <- function(c) colorspace::deutan(c, severity)
  p1 <- edit_colors(plot, deut)

  prot <- function(c) colorspace::protan(c, severity)
  p2 <- edit_colors(plot, prot)

  trit <- function(c) colorspace::tritan(c, severity)
  p3 <- edit_colors(plot, trit)

  des <- function(c) colorspace::desaturate(c, severity)
  p4 <- edit_colors(plot, des)

  cowplot::plot_grid(p1, p2, p3, p4, scale = 0.9, hjust = 0, vjust = 1,
                     labels = c("Deutanomaly", "Protanomaly", "Tritanomaly", "Desaturated"),
                     label_x = 0.01, label_y = 0.99, label_size = 12, label_fontface = "bold")
}

## ----disc-cvd, echo = FALSE, fig.asp = 1.618, fig.cap = "(ref:disc-cvd-cap)", warning = FALSE----
p <- ggplot(mtcars, aes(x = mpg, y = disp)) +
  geom_point(aes(color = factor(cyl))) +
  theme_atlas()

cvd_grid(p)

## ----for_repeat, include = FALSE----------------------------------------------
n_col <- 128
img <- function(obj, nam) {
  image(1:length(obj), 1, as.matrix(1:length(obj)), col = obj, main = nam,
        ylab = "", xaxt = "n", yaxt = "n",  bty = "n")
}

## ----cont-ex, echo = FALSE, fig.asp = 0.15, fig.cap = "(ref:cont-ex-cap)"-----
oldpar <- par(mfrow=c(1, 1), mar=rep(1, 4))
img(rev(seq_gradient_pal(low = "#132B43", high = "#56B1F7",
                         space = "Lab")(seq(0, 1, length=n_col))), "")
par(oldpar)

## ----cont-cvd, echo = FALSE, fig.asp = 0.5, fig.cap = "(ref:cont-cvd-cap)"----
oldpar <- par(mfrow=c(4, 1), mar=rep(1, 4))
img(dichromat(rev(seq_gradient_pal(low = "#132B43", high = "#56B1F7",
                                   space = "Lab")(seq(0, 1, length=n_col))),
              "deutan"), "Deutanomaly")
img(dichromat(rev(seq_gradient_pal(low = "#132B43", high = "#56B1F7",
                                   space = "Lab")(seq(0, 1, length=n_col))),
              "protan"), "Protanomaly")
img(dichromat(rev(seq_gradient_pal(low = "#132B43", high = "#56B1F7",
                                   space = "Lab")(seq(0, 1, length=n_col))),
              "tritan"), "Tritanomaly")
img(desaturate(rev(seq_gradient_pal(low = "#132B43", high = "#56B1F7",
                                    space = "Lab")(seq(0, 1, length=n_col)))),
    "Desaturated")
par(oldpar)

## ----okabeito-compare, out.width = "47%", fig.asp = 1.618, fig.show = "hold", fig.cap = "Comparison plot using the default (left) and Okabe Ito (right) discrete color palettes."----
ggplot(mtcars, aes(x = mpg, y = disp)) +
  geom_point(aes(color = factor(cyl)), size = 3) +
  theme_atlas()

ggplot(mtcars, aes(x = mpg, y = disp)) +
  geom_point(aes(color = factor(cyl)), size = 3) +
  scale_color_okabeito() +
  theme_atlas()

## ----show-viridis, echo = FALSE, fig.asp = 0.6, fig.cap = "The viridis color scales."----
oldpar <- par(mfrow=c(5, 1), mar=rep(1, 4))
img(rev(viridis(n_col)), "Viridis")
img(rev(magma(n_col)), "Magma")
img(rev(plasma(n_col)), "Plasma")
img(rev(inferno(n_col)), "Inferno")
img(rev(cividis(n_col)), "Cividis")
par(oldpar)

## ----viridis-compare, out.width = "47%", fig.asp = 1.618, fig.show = "hold", fig.cap = "Comparison plot with default (left) and viridis (right) continuous color palettes."----
ggplot(faithfuld, aes(x = eruptions, y = waiting)) +
  geom_raster(aes(fill = density)) +
  theme_atlas()

ggplot(faithfuld, aes(x = eruptions, y = waiting)) +
  geom_raster(aes(fill = density)) +
  scale_fill_viridis_c() +
  theme_atlas()

## ----atlas-colors, fig.cap = "(ref:atlas-colors-cap)"-------------------------
ggplot(mtcars, aes(x = mpg, y = disp)) +
  geom_point(aes(color = factor(cyl))) +
  scale_color_atlas() +
  theme_atlas()

## ----default-theme, fig.cap = "(ref:default-theme-cap)"-----------------------
set_theme(theme_atlas())

ggplot(mtcars, aes(x = mpg, y = disp)) +
  geom_point(aes(color = factor(cyl)))

## ----eval = FALSE-------------------------------------------------------------
# p <- ggplot(mtcars, aes(x = mpg, y = disp)) +
#   geom_point()
# 
# ggsave2(plot = p, filename = "my-plot.png", path = "where/to/save")

## ----eval = FALSE-------------------------------------------------------------
# p %>%
#   ggsave2(filename = "my-plot.png", path = "where/to/save") %>%
#   ggsave2(filename = "my-plot.pdf", path = "where/to/save")

