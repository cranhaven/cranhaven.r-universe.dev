## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----include=FALSE------------------------------------------------------------
dir.data <- system.file("data", package="FORTLS")
setwd(dir.data)
load("Rioja.data.RData")
tree.list <- Rioja.data$tree.tls

library(FORTLS)

## ----eval=FALSE, include=TRUE-------------------------------------------------
# metrics <- metrics.variables(tree.tls = tree.list,
#                   tree.ds = tree.ds, tree.field = tree.field,
#                   plot.design = c("fixed.area", "k.tree", "angle.count"),
#                   plot.parameters = data.frame(radius = 10, k = 10, BAF = 1),
#                   scan.approach = "single", var.metr = NULL,
#                   dbh.min = 4, h.min = 1.3, max.dist = Inf,
#                   dir.data = dir.data, save.result = FALSE, dir.result = NULL)

## ----eval=TRUE, include=TRUE--------------------------------------------------
tree.ds <- distance.sampling(tree.tls = tree.list,
                             id.plots = NULL,
                             strata.attributes = NULL)

## ----eval=TRUE, include=TRUE--------------------------------------------------
head(tree.ds$tree)

## ----eval=TRUE, include=TRUE--------------------------------------------------
head(tree.ds$par)

head(tree.ds$AIC)

## ----include=FALSE------------------------------------------------------------
dir.data <- system.file("exdata", package="FORTLS")
metrics <- read.csv(paste(dir.data, "metrics.variables.fixed.area.plot.csv", sep = "/"))

## ----eval=FALSE, include=TRUE-------------------------------------------------
# metrics[1:6, -c(3:36)]

## ----echo=FALSE---------------------------------------------------------------
kableExtra::scroll_box(kable_input = kableExtra::kable(metrics[1:6, -c(3:36)], format = "html"), 
                       width = "100%")

## ----eval=FALSE, include=TRUE-------------------------------------------------
# metrics[1:6, 1:36]

## ----echo=FALSE---------------------------------------------------------------
kableExtra::scroll_box(kable_input = kableExtra::kable(metrics[1:6, 1:36], format = "html"), 
                       width = "100%")

