## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
fig.dim = c(6, 4)
)

## ----setup, message=FALSE, warning=FALSE--------------------------------------
library(phylospatial); library(tmap); library(magrittr)
ps <- moss()

## ----beta, message=FALSE, warning=FALSE---------------------------------------
ps <- ps_add_dissim(ps, method = "sorensen", endemism = TRUE, normalize = TRUE)
ps

## ----ordinate, message=FALSE, warning=FALSE-----------------------------------
ps %>%
      ps_ordinate(method = "pca", k = 4) %>%
      tm_shape() +
      tm_raster(col.scale = tm_scale_continuous(values = "brewer.pi_yg"),
                col.free = FALSE) +
      tm_title("Phylogenetic community ordination")

## ----rgb, message=FALSE, warning=FALSE----------------------------------------
ps %>%
      ps_rgb(method = "cmds") %>%
      tm_shape() +
      tm_rgb(col.scale = tm_scale_rgb(max_color_value = 1), 
             interpolate = FALSE) +
      tm_title("Phylogenetic community ordination")

## ----k, message=FALSE, warning=FALSE------------------------------------------
ps_regions_eval(ps, k = 1:15, plot = TRUE, method = "average")

## ----regions, message=FALSE, warning=FALSE------------------------------------
ps %>%
      ps_regions(k = 4, method = "average") %>%
      tm_shape() +
      tm_raster(col.scale = tm_scale_categorical(values = "brewer.dark2"),
                col.legend = tm_legend(show = FALSE)) +
      tm_title("phylogenetic region")

