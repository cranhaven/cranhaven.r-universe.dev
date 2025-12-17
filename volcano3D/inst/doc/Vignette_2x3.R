## ----setup, include = FALSE, echo = FALSE-------------------------------------
knitr::opts_chunk$set(echo = TRUE, 
warning = FALSE, 
message = FALSE, 
fig.height = 7, 
fig.width=7, 
fig.align = "center")
library(knitr)
library(kableExtra)

## ---- echo=FALSE--------------------------------------------------------------
library(ggplot2)
library(ggpubr)
library(plotly)
library(usethis)

## ---- eval=FALSE--------------------------------------------------------------
#  library(volcano3D)
#  
#  # Basic DESeq2 set up
#  library(DESeq2)
#  
#  counts <- matrix(rnbinom(n=3000, mu=100, size=1/0.5), ncol=30)
#  rownames(counts) <- paste0("gene", 1:100)
#  cond <- rep(factor(rep(1:3, each=5), labels = c('A', 'B', 'C')), 2)
#  resp <- factor(rep(1:2, each=15), labels = c('non.responder', 'responder'))
#  metadata <- data.frame(drug = cond, response = resp)
#  
#  # Full dataset object construction
#  dds <- DESeqDataSetFromMatrix(counts, metadata, ~response)
#  
#  # Perform 3x DESeq2 analyses comparing binary response for each drug
#  res <- deseq_2x3(dds, ~response, "drug")

## ---- eval=FALSE--------------------------------------------------------------
#  library(easylabel)
#  df <- as.data.frame(res[[1]])  # results for the first drug
#  easyVolcano(df)

## ---- eval=FALSE--------------------------------------------------------------
#  # Generate polar object
#  obj <- deseq_2x3_polar(res)
#  
#  # 2d plot
#  radial_plotly(obj)
#  
#  # 3d plot
#  volcano3D(obj)

## ---- eval=FALSE--------------------------------------------------------------
#  obj <- deseq_2x3_polar(data1)
#  labs <- c('MS4A1', 'TNXA', 'FLG2', 'MYBPC1')
#  radial_plotly(obj, type=2, label_rows = labs) %>% toWebGL()

## ----radial_2x3_pos, echo = FALSE, message=FALSE, fig.align='center', out.width='70%', out.extra='style="border: 0;"'----
knitr::include_graphics("radial_2x3_pos.png")

## ----volc_2x3, echo = FALSE, message=FALSE, fig.align='center', out.width='70%', out.extra='style="border: 0;"'----
knitr::include_graphics("volc3d_2x3.png")

## ---- eval=FALSE--------------------------------------------------------------
#  obj <- deseq_2x3_polar(data1, process = "negative")
#  labs <- c('MS4A1', 'TNXA', 'FLG2', 'MYBPC1')
#  radial_plotly(obj, type=2, label_rows = labs) %>% toWebGL()

## ----radial_2x3_neg, echo = FALSE, message=FALSE, fig.align='center', out.width='70%', out.extra='style="border: 0;"'----
knitr::include_graphics("radial_2x3_neg.png")

## ----eval=FALSE---------------------------------------------------------------
#  polar_obj <- polar_coords_2x3(vstdata, metadata, "ACR.response.status",
#                                "Randomised.Medication")
#  
#  radial_plotly(polar_obj, type=2)
#  volcano3D(polar_obj)

## ---- eval=FALSE--------------------------------------------------------------
#  forest_ggplot(obj, c("MS4A1", "FLG2", "SFN")

## ----forest, echo = FALSE, message=FALSE, fig.align='center', out.width='70%', out.extra='style="border: 0;"'----
knitr::include_graphics("forest.png")

## -----------------------------------------------------------------------------
citation("volcano3D")

