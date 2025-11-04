## ----knitr-opts, include=FALSE------------------------------------------------
knitr::opts_chunk$set(comment = "#",collapse = TRUE,results = "hold",
                      fig.align = "center",dpi = 120)

## ----load-pkgs, message=FALSE, warning=FALSE----------------------------------
library(Matrix)
library(fastTopics)
library(ggplot2)
library(cowplot)
set.seed(1)

## ----load-data----------------------------------------------------------------
data(pbmc_facs)
counts <- pbmc_facs$counts
dim(counts)

## ----nonzeros-----------------------------------------------------------------
mean(counts > 0)

## ----fit-topic-model, eval=FALSE----------------------------------------------
# fit <- fit_topic_model(counts,k = 6)

## ----load-fit-----------------------------------------------------------------
fit <- pbmc_facs$fit

## ----loadings-1---------------------------------------------------------------
dim(fit$L)

## ----loadings-2---------------------------------------------------------------
rows <- "GATATATGTCAGTG-1-b_cells"
round(fit$L[rows,],digits = 3)

## ----loadings-3---------------------------------------------------------------
rows <- c("GACAGTACCTGTGA-1-memory_t",
          "TGAAGCACACAGCT-1-b_cells")
round(fit$L[rows,],digits = 3)

## ----summary-subpop-----------------------------------------------------------
samples <- pbmc_facs$samples
summary(samples$subpop)

## ----structure-plot-with-celltype-labels, fig.width=7.5, fig.height=1.75, results="hide", message=FALSE----
topic_colors <- c("skyblue","forestgreen","darkmagenta","dodgerblue",
                  "gold","darkorange")
structure_plot(fit,colors = topic_colors,topics = 1:6,gap = 25,
               grouping = samples$subpop)

## ----test-data----------------------------------------------------------------
counts_test <- pbmc_facs$counts_test
dim(counts_test)

## ----predict, results="hide", message=FALSE-----------------------------------
Ltest <- predict(fit,counts_test)

## ----structure-plot-test, fig.width=7.5, fig.height=1.75, results="hide", message=FALSE----
fit_test <- list(F = fit$F,L = Ltest)
class(fit_test) <- c("multinom_topic_model_fit","list")
structure_plot(fit_test,topics = 1:6,colors = topic_colors,gap = 2,
               grouping = pbmc_facs$samples_test$subpop)

## ----factors-1----------------------------------------------------------------
dim(fit$F)

## ----factors-2----------------------------------------------------------------
genes <- pbmc_facs$genes
rbind(fit$F[genes$symbol == "CD79B",],
      fit$F[genes$symbol == "CD79A",])

## ----de-analysis-1, eval=FALSE------------------------------------------------
# set.seed(1)
# de <- de_analysis(fit,counts,pseudocount = 0.1,
#                   control = list(ns = 1e4,nc = 4))

## ----de-analysis-2------------------------------------------------------------
pbmc_facs_file <- tempfile(fileext = ".RData")
pbmc_facs_url <- "https://stephenslab.github.io/fastTopics/pbmc_facs.RData"
download.file(pbmc_facs_url,pbmc_facs_file)
load(pbmc_facs_file)
de <- pbmc_facs$de

## ----diff-count-analysis-3----------------------------------------------------
rbind(de$postmean[genes$symbol == "CD79A",],
      de$postmean[genes$symbol == "CD79B",])

## ----volcano-plot-bcells, fig.height=4, fig.width=4.5-------------------------
volcano_plot(de,k = 4,labels = genes$symbol)

## ----volcano-plot-nk, fig.height=4, fig.width=4.5-----------------------------
volcano_plot(de,k = 1,labels = genes$symbol,ymax = 100)

## ----volcano-plot-tcells, fig.height=4, fig.width=4.5-------------------------
volcano_plot(de,k = 6,labels = genes$symbol,ymax = 100)

## ----session-info-------------------------------------------------------------
sessionInfo()

