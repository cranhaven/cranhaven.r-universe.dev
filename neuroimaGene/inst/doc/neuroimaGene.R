## ----setup, include = FALSE---------------------------------------------------
library(knitr)
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
  )

## -----------------------------------------------------------------------------
library(neuroimaGene, quietly = TRUE)
gene_list <- c('TRIM35', 'PROSER3', 'EXOSC6', 'PICK1', 'UPK1A', 'ESPNL', 'ZIC4')
ng <- neuroimaGene(gene_list, atlas = NA, mtc = 'BH', vignette = TRUE)

kable(head(ng, n=6))

## ----fig.asp = 0.5, fig.width = 7, out.width = "100%"-------------------------
plot_gns(ng, maxGns = 15)


## ----fig.asp = 1, fig.width = 8, out.width = "75%"----------------------------
plot_nidps(ng, maxNidps = 20)


## ----fig.asp = 0.8, fig.width = 6, out.width = "75%"--------------------------
plot_gnNIDP(ng, maxNidps = 20, maxGns = 15)


## ----fig.asp = 0.4, fig.width = 6, out.width = "80%"--------------------------
neuro_vis(ng, atlas = 'Desikan', lowcol = 'darkred', midcol = 'white', highcol = 'blue4')

neuro_vis(ng, atlas = 'Subcortex', lowcol = 'darkgreen', midcol = 'yellow2', highcol = 'darkorange')


