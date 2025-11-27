## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(autoRasch)

## -----------------------------------------------------------------------------
pcm_res <- pcm(shortDIF)

## -----------------------------------------------------------------------------
grMap <- matrix(c(rep(0,50),rep(1,50)),ncol = 1, dimnames = list(c(1:100),c("cov")))
pcmdif_res <- pcm_dif(shortDIF, groups_map = grMap)

## -----------------------------------------------------------------------------
summary(pcm_res, par="beta")

## -----------------------------------------------------------------------------
summary(pcmdif_res, par="delta")

## ----fig.height=3.5, fig.width=7----------------------------------------------
plot_PImap(pcm_res, main = "Person-Item map of the PCM")

## ----fig.height=3.5, fig.width=7----------------------------------------------
plot_PImap(pcmdif_res, main = "Person-Item map of the PCM-DIF")

## ----fig.height=3.5, fig.width=7----------------------------------------------
plot_ICC(pcm_res, itemno = 2, main = "ICC of I 17; estimated using PCM")

## -----------------------------------------------------------------------------
pcm_fit <- fitStats(pcm_res)
itemfit(pcm_fit)

## -----------------------------------------------------------------------------
pcmdif_fit <- fitStats(pcmdif_res)
itemfit(pcmdif_fit)

## -----------------------------------------------------------------------------
gpcm_res <- gpcm(shortDIF)

## -----------------------------------------------------------------------------
grMap <- matrix(c(rep(0,50),rep(1,50)),ncol = 1, dimnames = list(c(1:100),c("cov")))
gpcmdif_res <- gpcm_dif(shortDIF, groups_map = grMap)

## -----------------------------------------------------------------------------
summary(gpcm_res, par="alpha")

## -----------------------------------------------------------------------------
summary(gpcmdif_res, par="delta")

