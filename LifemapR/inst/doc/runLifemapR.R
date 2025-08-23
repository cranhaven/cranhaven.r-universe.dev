## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----install, eval = FALSE----------------------------------------------------
# remotes::install_github("Lifemap-ToL/LifemapR")

## ----load, eval = FALSE-------------------------------------------------------
# require("LifemapR")

## ----buildLF, eval = TRUE, echo = FALSE, results = FALSE, warning = FALSE-----
require("LifemapR", quietly = TRUE)

data(eukaryotes_1000)
LM_eukaryotes <- build_Lifemap(eukaryotes_1000)

## ----printLF, eval = TRUE, echo = FALSE---------------------------------------
if (is.lifemap_obj(LM_eukaryotes))
  LM_eukaryotes$df[6:10, 1:5]

## ----eval = FALSE-------------------------------------------------------------
# require("LifemapR")
# 
# LM <- build_Lifemap(data)

## ----eval = FALSE-------------------------------------------------------------
# # Example with default representation
# 
# # one layer
# lifemap(LM) + lm_markers()
# 
# # three different layers
# lifemap(LM) + lm_markers() + lm_branches()

## ----eval = FALSE-------------------------------------------------------------
# data(kraken_res)
# 
# LM_kraken <- build_Lifemap(df = kraken_res)

## ----eval = FALSE-------------------------------------------------------------
# lifemap(LM_kraken) +
#   lm_markers(var_fillColor = "coverage_percent", fillColor = "PiYG")

## ----includekrakenfig1, eval = TRUE, echo = FALSE, out.width = "40%", out.height = "20%", fig.cap = 'Visualisation of kraken data', fig.show = 'hold', fig.align = 'center'----
knitr::include_graphics("figures/kraken_base.png")

## ----eval = FALSE-------------------------------------------------------------
# # All the nodes that were requested by the user.
# lifemap(LM_kraken) +
#   lm_markers(var_fillColor = "coverage_percent", fillColor = "PiYG", display = "requested")
# 
# # Only the nodes that have no descendants.
# lifemap(LM_kraken) +
#   lm_markers(var_fillColor = "coverage_percent", fillColor = "PiYG", display = "leaves")
# 

## ----includekrakenfig2, eval = TRUE, echo = FALSE, out.width = "40%", out.height = "20%", fig.cap = 'left : display = "requested", right : display = "leaves"', fig.show = 'hold', fig.align = 'center'----
knitr::include_graphics(c("figures/kraken_requested.png", "figures/kraken_leaves.png"))

## ----eval = FALSE-------------------------------------------------------------
# # When clicking on a node, display the desired information.
# lifemap(LM_kraken) +
#   lm_markers(var_fillColor = "coverage_percent", fillColor = "PiYG", popup = "name")

## ----includekrakenfig3, eval = TRUE, echo = FALSE, out.width = "40%", out.height = "20%", fig.cap = 'Usage of the ```popup``` argument', fig.show = 'hold', fig.align = 'center'----
knitr::include_graphics("figures/kraken_popup.png")

## ----eval = FALSE-------------------------------------------------------------
# # Information on branche's color.
# lifemap(LM_kraken) +
#   lm_branches(var_color = "coverage_percent", color = "PiYG")
# 
# # Information on branche's size.
# lifemap(LM_kraken) +
#   lm_branches(size = "coverage_percent")

## ----includekrakenfig4, eval = TRUE, echo = FALSE, out.width = "45%", out.height = "30%", fig.cap = "left : branche's color, right : branche's size", fig.show = 'hold', fig.align = 'center'----
knitr::include_graphics(c("figures/kraken_branches_color.png", "figures/kraken_branches_size.png"))

## ----eval = FALSE-------------------------------------------------------------
# data(gen_res)
# LM_gen <- build_Lifemap(df = gen_res)

## ----eval = FALSE-------------------------------------------------------------
# # Visualisation of the Genome size on the fillColor and the TEcontent on the size of markers.
# lifemap(LM_gen) +
#   lm_markers(var_fillColor = "Genome_size", fillColor = "PiYG", radius  = "TEcontent_bp", FUN = mean)

## ----includegenfig, eval = TRUE, echo = FALSE, out.width = "100%", out.height = "20%", fig.cap = 'Visualisation of genomics data', fig.show = 'hold', fig.align = 'center'----
knitr::include_graphics("figures/gen_base.png")

## ----eval = FALSE-------------------------------------------------------------
# # Visualisation of the Genome size on the fillColor and the TEcontent on the size of markers.
# lifemap(LM_gen) +
#   lm_branches() +
#   lm_markers(var_fillColor = "Genome_size", fillColor = "PiYG", radius  = "TEcontent_bp", FUN = mean)

## ----includegenmarkfig, eval = TRUE, echo = FALSE, out.width = "100%", out.height = "20%", fig.cap = 'Visualisation of genomics data with markers and subtree', fig.show = 'hold', fig.align = 'center'----
knitr::include_graphics("figures/gen_markers_branches.png")

## ----eval = FALSE-------------------------------------------------------------
# data(eukaryotes_1000)
# LM_eukaryotes <- build_Lifemap(df = eukaryotes_1000)

## ----eval = FALSE-------------------------------------------------------------
# # Visualisation of eukaryotes data.
# lifemap(LM_eukaryotes) +
#   lm_markers()

## ----includeeukaryotefig1, eval = TRUE, echo = FALSE, out.width = "50%", out.height = "20%", fig.cap = 'Basic visualisation', fig.show = 'hold', fig.align = 'center'----
knitr::include_graphics("figures/eukaryotes_base.png")

## ----eval = FALSE-------------------------------------------------------------
# # Visualisation of Plants.
# lifemap(LM_eukaryotes) +
#   lm_markers(data = LM_eukaryotes$df[LM_eukaryotes$df$Group %in% "Plants", ])

## ----includeeukaryotefig2, eval = TRUE, echo = FALSE, out.width = "50%", out.height = "20%", fig.cap = 'Visualisation of Plants', fig.show = 'hold', fig.align = 'center'----
knitr::include_graphics("figures/eukaryotes_data.png")

## ----eval = FALSE-------------------------------------------------------------
# # Visualisation of the maximum assembly level.
# lifemap(LM_eukaryotes) +
#   lm_piecharts(param = "Group")

## ----includeeukaryotefig3, eval = TRUE, echo = FALSE, out.width = "50%", out.height = "20%", fig.cap = 'Visualisation of the maximum assembly level', fig.show = 'hold', fig.align = 'center'----
knitr::include_graphics("figures/eukaryotes_piecharts.png")

