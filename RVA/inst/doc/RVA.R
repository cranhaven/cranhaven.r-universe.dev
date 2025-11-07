## ----eval=FALSE---------------------------------------------------------------
#  devtools::install_github("THERMOSTATS/RVA_prod")

## ----message=FALSE, warning=FALSE---------------------------------------------
library(RVA)

## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, rows.print=25, comment = "")

options(
  ggplot2.continuous.colour = 'viridis',
  ggplot2.continuous.fill = 'viridis'
)

## -----------------------------------------------------------------------------
df <- RVA::Sample_summary_statistics_table
df1 <- RVA::Sample_summary_statistics_table1 
d1 <- list(df, df1)

## ---- echo=FALSE--------------------------------------------------------------
knitr::kable(head(d1[[1]]))

## ----eval=FALSE---------------------------------------------------------------
#  plot_cutoff(data = data,
#    comp.names = NULL,
#    FCflag = "logFC",
#    FDRflag = "adj.P.Val",
#    FCmin = 1.2,
#    FCmax = 2,
#    FCstep = 0.1,
#    p.min = 0,
#    p.max = 0.2,
#    p.step = 0.01,
#    plot.save.to = NULL,
#    gen.3d.plot = TRUE,
#    gen.plot = TRUE)

## -----------------------------------------------------------------------------
cutoff.result <- plot_cutoff(data = df,
                       gen.plot = TRUE,
                       gen.3d.plot = TRUE)

## ---- eval=FALSE--------------------------------------------------------------
#  head(cutoff.result[[1]])

## ---- echo= FALSE-------------------------------------------------------------
knitr::kable(head(cutoff.result[[1]]))

## ---- warning=FALSE, eval=FALSE-----------------------------------------------
#  cutoff.result[[2]]

## ---- warning=FALSE-----------------------------------------------------------
cutoff.result[[3]]

## ---- eval=FALSE--------------------------------------------------------------
#  plot_cutoff(data = df,
#              plot.save.to = "cut_off_selection_plot.png")

## ---- eval=FALSE--------------------------------------------------------------
#  library(ggplot2)
#  ggsave("cut_off_selection_plot.png", cutoff.result[[3]], width = 5, height = 5, dpi = 300)

## ---- message=FALSE-----------------------------------------------------------
cutoff.result.list <- plot_cutoff(data = d1, 
                                  comp.names = c('a', 'b'))

## ---- eval=FALSE--------------------------------------------------------------
#  head(cutoff.result.list[[1]])

## ---- echo=FALSE--------------------------------------------------------------
knitr::kable(head(cutoff.result.list[[1]]))

## -----------------------------------------------------------------------------
cutoff.result.list

## ---- eval=FALSE--------------------------------------------------------------
#  plot_cutoff(data = d1,
#              comp.names = c("A", "B"),
#              plot.save.to = "cut_off_list_plot.png")

## ---- eval=FALSE--------------------------------------------------------------
#  library(ggplot2)
#  ggsave("cut_off_list_plot.png", cutoff.result.list, width = 5, height = 5, dpi = 300)

## ---- results='hide'----------------------------------------------------------
qq.result <- plot_qq(df)
qq.result

## ---- eval=FALSE--------------------------------------------------------------
#  plot_qq(data = df,
#          plot.save.to = "qq_plot.png")

## ---- eval=FALSE--------------------------------------------------------------
#  library(ggplot2)
#  ggsave("qq_plot.png", qq.result, width = 5, height = 5, dpi = 300)

## ---- results='hide'----------------------------------------------------------
qq.list.result <- plot_qq(data = d1, 
        comp.names = c('A', 'B'))
qq.list.result

## ---- eval=FALSE--------------------------------------------------------------
#  plot_qq(data = d1,
#          comp.names = c("A", "B"),
#          plot.save.to = "qq_list_plot.png")

## ---- eval=FALSE--------------------------------------------------------------
#  library(ggplot2)
#  ggsave("qq_list_plot.png", qq.list.result, width = 5, height = 5, dpi = 300)

## ----eval=FALSE---------------------------------------------------------------
#  plot_volcano(
#    data = data,
#    comp.names = NULL,
#    geneset = NULL,
#    geneset.FCflag = "logFC",
#    highlight.1 = NULL,
#    highlight.2 = NULL,
#    upcolor = "#FF0000",
#    downcolor = "#0000FF",
#    plot.save.to = NULL,
#    xlim = c(-4, 4),
#    ylim = c(0, 12),
#    FCflag = "logFC",
#    FDRflag = "adj.P.Val",
#    highlight.FC.cutoff = 1.5,
#    highlight.FDR.cutoff = 0.05,
#    title = "Volcano plot",
#    xlab = "log2 Fold Change",
#    ylab = "log10(FDR)"
#  )

## ----message=FALSE, results='hide', warning=FALSE-----------------------------
plot_volcano(data = df)

## ----message=FALSE, results='hide', warning=FALSE-----------------------------
plot_volcano(data = d1, 
             comp.names = c('a', 'b'))

## -----------------------------------------------------------------------------
#disease gene set used to color volcanoplot
dgs <- RVA::Sample_disease_gene_set 

## ----eval=FALSE---------------------------------------------------------------
#  head(dgs)

## ----echo=FALSE---------------------------------------------------------------
knitr::kable(head(dgs))

## ----message=FALSE,warning=FALSE----------------------------------------------
plot_volcano(data = df,
             geneset = dgs,
             upcolor = "#FF0000",
             downcolor = "#0000FF",
             xlim = c(-3,3),
             ylim = c(0,14))

## ----message=FALSE, warning=FALSE---------------------------------------------
plot_volcano(data = d1,
             comp.names = c('a', 'b'),
             geneset = dgs,
             upcolor = "#FF0000",
             downcolor = "#0000FF",
             xlim = c(-3,3),
             ylim = c(0,14))

## ----message=FALSE,warning=FALSE----------------------------------------------
volcano.result <- plot_volcano(data = df,
                  highlight.1 = c("ENSG00000169031.19","ENSG00000197385.5","ENSG00000111291.8"),
                  highlight.2 = c("ENSG00000123610.5","ENSG00000120217.14", "ENSG00000138646.9", "ENSG00000119922.10","ENSG00000185745.10"),
                  upcolor = "darkred",
                  downcolor = "darkblue",
                  xlim = c(-3,3),
                  ylim = c(0,14))
volcano.result

## ---- warning=FALSE, eval=FALSE-----------------------------------------------
#  plot_volcano(data = df,
#               geneset = dgs,
#               plot.save.to = "volcano_plot.png")

## ---- eval=FALSE--------------------------------------------------------------
#  library(ggplot2)
#  ggsave("volcano_plot.png", volcano.result, width = 5, height = 5, dpi = 300)

## ----eval=FALSE---------------------------------------------------------------
#  plot_pathway(
#    data = df,
#    comp.names = NULL,
#    gene.id.type = "ENSEMBL",
#    FC.cutoff = 1.3,
#    FDR.cutoff = 0.05,
#    FCflag = "logFC",
#    FDRflag = "adj.P.Val",
#    Fisher.cutoff = 0.1,
#    Fisher.up.cutoff = 0.1,
#    Fisher.down.cutoff = 0.1,
#    plot.save.to = NULL,
#    pathway.db = "rWikiPathways"
#    )

## ----message=FALSE, warning=FALSE, results="hide"-----------------------------
pathway.result <- plot_pathway(data = df, pathway.db = "Hallmark", gene.id.type = "ENSEMBL")

## ----eval=FALSE---------------------------------------------------------------
#  head(pathway.result[[1]])

## ----echo=FALSE---------------------------------------------------------------
knitr::kable(head(pathway.result[[1]]))

## ----eval=FALSE---------------------------------------------------------------
#  head(pathway.result[[2]])

## ----echo=FALSE---------------------------------------------------------------
knitr::kable(head(pathway.result[[2]]))

## -----------------------------------------------------------------------------
pathway.result[[3]]

## -----------------------------------------------------------------------------
pathway.result[[4]]

## -----------------------------------------------------------------------------
pathway.result[[5]]

## ----eval=FALSE---------------------------------------------------------------
#  library(ggplot2)
#  ggsave("joint_plot.png",pathway.result[[5]], width = 5, height = 5, dpi = 300)

## ----message=FALSE, warning=FALSE, results="hide"-----------------------------
list.pathway.result <- plot_pathway(data = list(df,df1),comp.names=c("A","B"),pathway.db = "Hallmark", gene.id.type = "ENSEMBL")

## ----eval=FALSE---------------------------------------------------------------
#  head(list.pathway.result[[1]])

## ----echo=FALSE---------------------------------------------------------------
knitr::kable(head(list.pathway.result[[1]]))

## ----eval=FALSE---------------------------------------------------------------
#  head(list.pathway.result[[2]])

## ----echo=FALSE---------------------------------------------------------------
knitr::kable(head(list.pathway.result[[2]]))

## -----------------------------------------------------------------------------
list.pathway.result[[3]]

## -----------------------------------------------------------------------------
list.pathway.result[[4]]

## ---- eval=FALSE--------------------------------------------------------------
#  library(ggplot2)
#  ggsave("non-directional.png",pathway.result[[4]], width = 5, height = 5, dpi = 300)

## -----------------------------------------------------------------------------
count <- RVA::count_table[,1:50]

## ---- eval=FALSE--------------------------------------------------------------
#  count[1:6,1:5]

## ----echo=FALSE---------------------------------------------------------------
knitr::kable(count[1:6,1:5])

## -----------------------------------------------------------------------------
annot <- RVA::sample_annotation[1:50,]

## ----eval=FALSE---------------------------------------------------------------
#  head(annot)

## ----echo=FALSE---------------------------------------------------------------
knitr::kable(head(annot))

## ----message=FALSE------------------------------------------------------------
hm.expr <- plot_heatmap.expr(data = count, 
                             annot = annot,
                             sample.id = "sample_id",
                             annot.flags = c("day", "Treatment"),
                             ct.table.id.type = "ENSEMBL",
                             gene.id.type = "SYMBOL",
                             gene.names = NULL,
                             gene.count = 10,
                             title = "RVA Heatmap",
                             fill = "CPM",
                             baseline.flag = "day",
                             baseline.val = "0",
                             plot.save.to = NULL,
                             input.type = "count")

## ---- echo=FALSE--------------------------------------------------------------
hm.expr[[1]]

## ---- eval=FALSE--------------------------------------------------------------
#  head(hm.expr[[2]])

## ---- echo=FALSE--------------------------------------------------------------
knitr::kable(head(hm.expr[[2]]))

## ----results='hide', eval=FALSE-----------------------------------------------
#  library(ComplexHeatmap)
#  png("heatmap_plots2cp.png", width = 500, height = 500)
#  draw(hm.expr$gp)
#  dev.off()
#  

## ----message=FALSE------------------------------------------------------------
hm.expr.cfb <- plot_heatmap.expr(data = count, 
                                 annot = annot,
                                 sample.id = "sample_id",
                                 annot.flags = c("day", "Treatment"),
                                 ct.table.id.type = "ENSEMBL",
                                 gene.id.type = "SYMBOL",
                                 gene.names = NULL,
                                 gene.count = 10,
                                 title = "RVA Heatmap",
                                 fill = "CFB",
                                 baseline.flag = "day",
                                 baseline.val = "0",
                                 plot.save.to = NULL,
                                 input.type = "count")


## ---- echo=FALSE--------------------------------------------------------------
hm.expr.cfb[[1]]

## ---- eval=FALSE--------------------------------------------------------------
#  head(hm.expr.cfb[[2]])

## ---- echo=FALSE--------------------------------------------------------------
knitr::kable(head(hm.expr.cfb[[2]]))

## ----results='hide', eval=FALSE-----------------------------------------------
#  library(ComplexHeatmap)
#  png("heatmap_plots1cf.png", width = 500, height = 500)
#  draw(hm.expr.cfb$gp)
#  dev.off()

## -----------------------------------------------------------------------------
anno <- RVA::sample_annotation

## ---- eval=FALSE--------------------------------------------------------------
#  head(anno)

## ---- echo=FALSE--------------------------------------------------------------
knitr::kable(head(anno))

## -----------------------------------------------------------------------------
ct <- RVA::sample_count_cpm

## ----eval=FALSE---------------------------------------------------------------
#  ct[1:6,1:5]

## ----echo=FALSE---------------------------------------------------------------
knitr::kable(ct[1:6,1:5])

## -----------------------------------------------------------------------------
gene.result <- plot_gene(ct, 
               anno,
               gene.names = c("AAAS", "A2ML1", "AADACL3", "AARS"),
               ct.table.id.type = "ENSEMBL",
               gene.id.type = "SYMBOL",
               treatment = "Treatment",
               sample.id = "sample_id",
               time = "day",
               log.option = TRUE,
               plot.save.to = NULL,
               input.type = "cpm")

## ---- echo = FALSE------------------------------------------------------------
gene.result[[1]]

## ---- eval = FALSE------------------------------------------------------------
#  head(gene.result[[2]])

## ---- echo = FALSE------------------------------------------------------------
knitr::kable(head(gene.result[[2]]))

## ----message=FALSE, eval=FALSE------------------------------------------------
#  library(ggplot2)
#  ggsave(gene.result, "gene_plots1_4.png", device = "png", width = 100, height = 100, dpi = 200, limitsize = FALSE)

