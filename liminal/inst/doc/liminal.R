## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

library(ggplot2)
theme_set(theme_bw())
library(liminal)

## ----pc-view------------------------------------------------------------------
library(liminal)
data("fake_trees")
pcs  <- prcomp(fake_trees[, -ncol(fake_trees)])
# var explained
head(cumsum(pcs$sdev / sum(pcs$sdev)))

## ----pca-xy-------------------------------------------------------------------
library(ggplot2)
fake_trees <- dplyr::bind_cols(fake_trees, as.data.frame(pcs$x))
ggplot(fake_trees, aes(x = PC1, y = PC2, color = branches)) +
  geom_point() +
  scale_color_manual(values = limn_pal_tableau10())

## ---- eval = FALSE------------------------------------------------------------
#  # this loads a shiny app on the first fifteen PCs
#  limn_tour(fake_trees, cols = PC1:PC15, color = branches)

## ----interface, echo = FALSE, fig.align='center'------------------------------
knitr::include_graphics("figures/limn_tour_basic.png")

## ----tsne-xy------------------------------------------------------------------
set.seed(2099)
tsne <- Rtsne::Rtsne(dplyr::select(fake_trees, dplyr::starts_with("dim")))

tsne_df <- data.frame(tsneX = tsne$Y[,1],
                      tsneY = tsne$Y[,2])
ggplot(tsne_df, aes(x = tsneX, y = tsneY, color = fake_trees$branches)) +
  geom_point() +
  scale_color_manual(values = limn_pal_tableau10())

## ---- eval = FALSE------------------------------------------------------------
#  limn_tour_link(embed_data = tsne_df,
#                 tour_data = fake_trees,
#                 cols = PC1:PC10, # tour columns to select
#                 color = branches # variable to highlight across both view, can come for either data frames
#  )

## ----limn-interface, echo = FALSE, fig.align='center'-------------------------
knitr::include_graphics("figures/limn_tour_linked.png")

## ---- eval = FALSE------------------------------------------------------------
#  res <- limn_tour_link(embed_data = tsne_df,
#                 tour_data = fake_trees,
#                 cols = PC1:PC10, # tour columns to select
#                 color = branches # variable to highlight across both view, can come for either data frames
#  )

