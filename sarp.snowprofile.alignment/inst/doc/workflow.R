## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  out.width = "100%"
)

## ----setup--------------------------------------------------------------------
library(sarp.snowprofile.alignment)

## -----------------------------------------------------------------------------
## Compute alignment:
dtwAlignment <- dtwSP(SPpairs$A_modeled, SPpairs$A_manual, open.end = FALSE)

## ----eval=FALSE---------------------------------------------------------------
#  ## Plot alignment:
#  plotSPalignment(dtwAlignment = dtwAlignment)

## ----echo=FALSE---------------------------------------------------------------
knitr::include_graphics("figures/alignment.png")

## ----echo=FALSE, out.width="100%"---------------------------------------------
knitr::include_graphics("figures/legend_gtype.png")

## ----eval=FALSE---------------------------------------------------------------
#  ## Inspect local cost:
#  plotCostDensitySP(dtwAlignment)

## ----echo=FALSE---------------------------------------------------------------
knitr::include_graphics("figures/costDensity.png")

## -----------------------------------------------------------------------------
dtwAlignment$sim <- simSP(dtwAlignment$reference, dtwAlignment$queryWarped, verbose = TRUE, simType = "HerlaEtAl2021")

## ----medoid, eval=TRUE--------------------------------------------------------
## rescaling and resampling of the snow profiles:
setRR <- reScaleSampleSPx(SPgroup)$set

## compute the pairwise distance matrix:
distmat <- distanceSP(setRR)

## hierarchichal clustering:
setRR_hcl <- stats::hclust(distmat, method = "complete")

## ----echo=FALSE, out.width="100%"---------------------------------------------
knitr::include_graphics("figures/cluster_hierarchy.png")

## ----echo=FALSE, eval=FALSE---------------------------------------------------
#  ## This can be used to produce the cluster hierarchy plot:
#  
#  ## prepare plot:
#  cluster_colors <- c("dark orange", "blue", "dark green", "red")
#  setRR_dend <- stats::as.dendrogram(setRR_hcl)
#  dendextend::labels_colors(setRR_dend) <- cluster_colors[stats::cutree(setRR_hcl, 4)[order.dendrogram(setRR_dend)]]
#  dendextend::labels_cex(setRR_dend) <- 2.5
#  dendextend::labels(setRR_dend) <- seq(12)
#  
#  layout(matrix(c(1, 1, 2, 2), 2, 2, byrow = T), heights = c(1, 2))
#  
#  ## plot hierarchy
#  plot(setRR_dend, yaxt = "n", xlim = c(1, nrow(distmat)))
#  mtext("Cluster hierarchy", side = 2, line = 1)
#  
#  ## plot profiles
#  plot(setRR[order.dendrogram(setRR_dend)], SortMethod = 'unsorted', box = F, ylab = "",
#       yPadding = 0, xPadding = 0, xaxs = 'i', yaxs = 'i')
#  mtext("Rescaled snow height", side = 2, line = 1, las = 0)
#  mtext("Individual snow profiles", side = 1, line = 2)
#  
#  ## plot vertical lines between most dominant clusters
#  abline(v = 4.5, lwd = 3)
#  abline(v = 7.5, lwd = 2, lty = "dashed")
#  abline(v = 9.5, lwd = 2, lty = "dotted")
#  

## ----eval=TRUE----------------------------------------------------------------
unname(medoidSP(distmat = distmat[1:4]))

## ----eval=FALSE---------------------------------------------------------------
#  fit <- smacof::mds(as.dist(distmat), type = "ordinal")

## ----echo=FALSE, out.width="100%"---------------------------------------------
knitr::include_graphics("figures/configuration_plots.png")

