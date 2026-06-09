## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----include=FALSE------------------------------------------------------------
dir.data <- system.file("data", package="FORTLS")
setwd(dir.data)
load("Rioja.data.RData")
load("Rioja.simulations.RData")
tree.tls <- Rioja.data$tree.tls
tree.field <- Rioja.data$tree.field
dir.data <- system.file("exdata", package="FORTLS")
library(FORTLS)

## ----fig.height=7, fig.width=7, fig.align = "center", message=FALSE, warning=FALSE,fig.alt = "Line charts output obtained with the estimation.plot.size function"----
estimation.plot.size(tree.tls = tree.tls,
                     plot.parameters = data.frame(radius.max = 25, k.max = 50, BAF.max = 4),
                     dbh.min = 4,
                     average = TRUE, all.plot.designs = FALSE)

## ----eval=FALSE, include=TRUE-------------------------------------------------
# simulations <- simulations(tree.tls = tree.tls, tree.ds = tree.ds, tree.field = tree.field,
#             plot.design = c("fixed.area", "k.tree", "angle.count"),
#             plot.parameters = data.frame(radius.max = 25, k.max = 50, BAF.max = 4),
#             scan.approach = "single", var.metr = list(tls = NULL, field = NULL),
#             dbh.min = 4, h.min = 1.3, max.dist = Inf,
#             dir.data = dir.data, save.result = FALSE, dir.result = NULL)

## ----eval=FALSE, include=TRUE-------------------------------------------------
# head(simulations$fixed.area)

## ----echo=FALSE---------------------------------------------------------------
kableExtra::scroll_box(kable_input = kableExtra::kable(head(Rioja.simulations$fixed.area), 
                                                       format = "html"), width = "100%")

## ----eval=FALSE, include=TRUE-------------------------------------------------
# tail(simulations$fixed.area)

## ----echo=FALSE---------------------------------------------------------------
kableExtra::scroll_box(kable_input = kableExtra::kable(tail(Rioja.simulations$fixed.area), 
                                                       format = "html"), width = "100%")

## ----eval=TRUE----------------------------------------------------------------
bias <- relative.bias(simulations = Rioja.simulations,
              variables = c("N", "G", "d", "dg", "d.0", "h", "h.0"),
              save.result = FALSE, dir.result = NULL)

## ----eval=FALSE, include=TRUE-------------------------------------------------
# head(bias$fixed.area)

## ----echo=FALSE---------------------------------------------------------------
kableExtra::scroll_box(kable_input = kableExtra::kable(head(bias$fixed.area), 
                                                       format = "html"), width = "100%")

## -----------------------------------------------------------------------------
fixed.area.simulations <- list(fixed.area = Rioja.simulations$fixed.area[Rioja.simulations$fixed.area$radius < 7.5, ])
cor <- correlations(simulations = fixed.area.simulations,
             variables = c("N", "G", "d", "dg", "d.0", "h", "h.0"),
             method = c("pearson", "spearman"), 
             save.result = FALSE, dir.result = NULL)

## ----eval=FALSE, include=TRUE-------------------------------------------------
# cor$correlations$pearson$fixed.area[20:26,1:15]

## ----echo=FALSE---------------------------------------------------------------
kableExtra::scroll_box(kable_input = kableExtra::kable(
  cor$correlations$pearson$fixed.area[20:26,1:15], format = "html"), width = "100%")

## ----eval=FALSE, include=TRUE-------------------------------------------------
# cor$correlations.pval$pearson$fixed.area[20:26,1:15]

## ----echo=FALSE---------------------------------------------------------------
kableExtra::scroll_box(kable_input = kableExtra::kable(
  cor$correlations.pval$pearson$fixed.area[20:26,1:15], format = "html"), width = "100%")

## ----eval=FALSE, include=TRUE-------------------------------------------------
# cor$opt.correlations$pearson$fixed.area[20:26,]

## ----echo=FALSE---------------------------------------------------------------
kableExtra::scroll_box(kable_input = kableExtra::kable(
  cor$opt.correlations$pearson$fixed.area[20:26,], format = "html"), width = "100%")

## ----eval=FALSE---------------------------------------------------------------
# optimize.plot.design(correlations = cor$opt.correlations,
#                      variables = c("N", "G", "d", "dg", "d.0", "h", "h.0"),
#                      dir.result = NULL)

