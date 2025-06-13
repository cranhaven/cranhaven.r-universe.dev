## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(ggplot2)
theme_set(theme_bw())

## ----pdfsense-prepare---------------------------------------------------------
library(liminal)
data(pdfsense)

## ----pdfsense-----------------------------------------------------------------
pcs  <- prcomp(pdfsense[, 7:ncol(pdfsense)])

## ---- echo = TRUE-------------------------------------------------------------
res <- data.frame(
  component = 1:56, 
  variance_explained = cumsum(pcs$sdev / sum(pcs$sdev))
)

ggplot(res, aes(x = component, y = variance_explained)) +
  geom_point() +
  scale_x_continuous(
    breaks = seq(0, 60, by = 5)
  ) +
  scale_y_continuous(
    labels = function(x) paste0(100*x, "%")
  )

## -----------------------------------------------------------------------------
pdfsense <- dplyr::bind_cols(
  pdfsense, 
  as.data.frame(pcs$x)
)
pdfsense$Type <- factor(pdfsense$Type)

## ---- eval = FALSE------------------------------------------------------------
#  limn_tour(pdfsense, PC1:PC6, Type)

## -----------------------------------------------------------------------------
set.seed(3099)
start <- clamp_sd(as.matrix(dplyr::select(pdfsense, PC1, PC2)), sd = 1e-4)
tsne <- Rtsne::Rtsne(
  dplyr::select(pdfsense, PC1:PC56),
  pca = FALSE, 
  normalize = TRUE,
  perplexity = 50,
  exaggeration_factor = nrow(pdfsense) / 100,
  Y_init = start
)

## ----tsne---------------------------------------------------------------------
tsne_embedding <- as.data.frame(tsne$Y)
tsne_embedding <- dplyr::rename(tsne_embedding, tsneX = V1, tsneY = V2)
tsne_embedding$Type <- pdfsense$Type

## -----------------------------------------------------------------------------
ggplot(tsne_embedding, 
       aes(x = tsneX, y = tsneY, color = Type)) +
  geom_point() +
  scale_color_manual(values = limn_pal_tableau10())

## ---- eval = FALSE------------------------------------------------------------
#  limn_tour_link(
#    tour_data = pdfsense,
#    embed_data = tsne_embedding,
#    cols = PC1:PC6,
#    color = Type
#  )

