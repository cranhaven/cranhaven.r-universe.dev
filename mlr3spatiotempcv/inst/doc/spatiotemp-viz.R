## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----fig.align='center', eval=FALSE-------------------------------------------
# library(mlr3)
# library(mlr3spatiotempcv)
# task_st = tsk("cookfarm_mlr3")
# task_st$set_col_roles("SOURCEID", roles = "space")
# task_st$set_col_roles("Date", roles = "time")
# resampling = rsmp("sptcv_cstf", folds = 5)
# 
# pl = autoplot(resampling, task_st, c(1, 2, 3, 4),
#   crs = 4326, point_size = 3, axis_label_fontsize = 10,
#   plot3D = TRUE
# )
# 
# # Warnings can be ignored
# pl_subplot = plotly::subplot(pl)
# 
# plotly::layout(pl_subplot,
#   title = "Individual Folds",
#   scene = list(
#     domain = list(x = c(0, 0.5), y = c(0.5, 1)),
#     aspectmode = "cube",
#     camera = list(eye = list(z = 2.5))
#   ),
#   scene2 = list(
#     domain = list(x = c(0.5, 1), y = c(0.5, 1)),
#     aspectmode = "cube",
#     camera = list(eye = list(z = 2.5))
#   ),
#   scene3 = list(
#     domain = list(x = c(0, 0.5), y = c(0, 0.5)),
#     aspectmode = "cube",
#     camera = list(eye = list(z = 2.5))
#   ),
#   scene4 = list(
#     domain = list(x = c(0.5, 1), y = c(0, 0.5)),
#     aspectmode = "cube",
#     camera = list(eye = list(z = 2.5))
#   )
# )

## ----echo=FALSE, fig.align='center', fig.align='center'-----------------------
# plotly::save_image(foo, "man/figures/sptcv_cstf_multiplot.png", width = 1200, height = 1200)
knitr::include_graphics("../man/figures/sptcv_cstf_multiplot.png")

## ----echo=FALSE, cache=TRUE, fig.width=10, fig.height=8-----------------------
library(patchwork)
library(ggplot2)
library(mlr3)
library(mlr3spatiotempcv)
task_ecuador = tsk("ecuador")
task_cookfarm = tsk("cookfarm_mlr3")

set.seed(42)

# block
resampling_block = rsmp("spcv_block", range = 1000L, folds = 3)
p_block = autoplot(resampling_block, task_ecuador,
  fold_id = 1,
  show_blocks = TRUE, size = 0.6
) +
  scale_y_continuous(breaks = seq(-3.97, -4, -0.02)) +
  scale_x_continuous(breaks = seq(-79.085, -79.055, 0.02)) +
  theme(legend.position = "none")

# buffer
resampling_buffer = rsmp("spcv_buffer", theRange = 1000L)
p_buffer = autoplot(resampling_buffer, task_ecuador, fold_id = 1, size = 0.6) +
  scale_y_continuous(breaks = seq(-3.97, -4, -0.02)) +
  scale_x_continuous(breaks = seq(-79.085, -79.055, 0.02)) +
  theme(legend.position = "none")

# coords
resampling_coords = rsmp("spcv_coords", folds = 3)
p_coords = autoplot(resampling_coords, task_ecuador, fold_id = 1, size = 0.6) +
  scale_y_continuous(breaks = seq(-3.97, -4, -0.02)) +
  scale_x_continuous(breaks = seq(-79.085, -79.055, 0.02)) +
  theme(legend.position = "none")

# env
resampling_env = rsmp("spcv_env", folds = 3, features = "distroad")
p_env = autoplot(resampling_env, task_ecuador, fold_id = 1, size = 0.6) +
  scale_y_continuous(breaks = seq(-3.97, -4, -0.02)) +
  scale_x_continuous(breaks = seq(-79.085, -79.055, 0.02)) +
  theme(legend.position = "none")

# disc
resampling_disc = rsmp("spcv_disc", folds = 3L, radius = 200L, buffer = 200L)
p_disc = autoplot(resampling_disc, task_ecuador, fold_id = 1, size = 0.6) +
  scale_y_continuous(breaks = seq(-3.97, -4, -0.02)) +
  scale_x_continuous(breaks = seq(-79.085, -79.055, 0.02)) +
  theme(legend.position = "none")

# tiles
resampling_tiles = rsmp("spcv_tiles", nsplit = c(4L, 3L), reassign = FALSE)
p_tiles = autoplot(resampling_tiles, task_ecuador, fold_id = 1, size = 0.6) +
  scale_y_continuous(breaks = seq(-3.97, -4, -0.02)) +
  scale_x_continuous(breaks = seq(-79.085, -79.055, 0.02)) +
  theme(legend.position = "none")

# plot
p_block + p_coords + p_env + p_disc + p_tiles + p_buffer +
  plot_layout(ncol = 3) +
  plot_annotation(tag_levels = "A")

