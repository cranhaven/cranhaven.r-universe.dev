## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse  = TRUE,
  comment   = "#>",
  eval      = TRUE,
  cache     = FALSE,
  warning   = FALSE,
  message   = FALSE,
  fig.width = 7,
  fig.height = 5
)

## ----load---------------------------------------------------------------------
library(BioUtils)
library(plotly)
library(msigdbr)
library(ggplot2)

geo <- extract.expression(
  load.geo.soft(accession = "GDS507", log.transform = TRUE)
)

## ----pca----------------------------------------------------------------------
pca.plot(geo$expression, geo$phenotype, color.by = "disease.state")

## ----de-----------------------------------------------------------------------
de.results <- run.limma.de(geo, condition.col = "disease.state")

sig <- de.results[de.results$adj.P.Val < 0.05 & abs(de.results$logFC) > 1, ]
cat("Significant DE probes (FDR < 0.05, |logFC| > 1): ", nrow(sig), "\n")
cat("Upregulated in RCC:                              ", sum(sig$logFC > 0), "\n")
cat("Downregulated in RCC:                            ", sum(sig$logFC < 0), "\n")

## ----volcano_static-----------------------------------------------------------
volcano.plot(de.results, fc.threshold = 1, fdr.threshold = 0.05)

## ----volcano_plotly-----------------------------------------------------------
de.plot <- de.results
de.plot$neg.log10.fdr <- -log10(de.plot$adj.P.Val)
de.plot$status <- "Not Significant"
de.plot$status[de.plot$adj.P.Val < 0.05 & de.plot$logFC > 1] <- "Upregulated"
de.plot$status[de.plot$adj.P.Val < 0.05 & de.plot$logFC < -1] <- "Downregulated"

de.plot$gene <- ""
top.idx <- order(de.plot$adj.P.Val)[1:50]
de.plot$gene[top.idx] <- get.gene.name(
  geo$gene, rownames(de.plot)[top.idx], use.symbols = TRUE
)

plotly::plot_ly(
  data = de.plot,
  x = ~logFC,
  y = ~neg.log10.fdr,
  color = ~status,
  colors = c("Upregulated" = "firebrick",
             "Downregulated" = "steelblue",
             "Not Significant" = "grey70"),
  text = ~paste0("Gene: ", gene,
                 "<br>logFC: ", round(logFC, 2),
                 "<br>FDR: ", signif(adj.P.Val, 3)),
  hoverinfo = "text",
  type = "scatter",
  mode = "markers",
  marker = list(size = 4, opacity = 0.6)
) %>%
  plotly::layout(
    title = "Volcano Plot: RCC vs Normal Kidney (Interactive, hover for gene labels)",
    xaxis = list(title = "Log2 Fold Change", zeroline = TRUE),
    yaxis = list(title = "-Log10 Adjusted P-Value", zeroline = FALSE),
    shapes = list(
      list(type = "line", x0 = 1,  x1 = 1,
           y0 = 0, y1 = max(de.plot$neg.log10.fdr, na.rm = TRUE),
           line = list(dash = "dot", color = "black")),
      list(type = "line", x0 = -1, x1 = -1,
           y0 = 0, y1 = max(de.plot$neg.log10.fdr, na.rm = TRUE),
           line = list(dash = "dot", color = "black"))
    )
  )

## ----top_candidates-----------------------------------------------------------
top.probes <- rownames(head(de.results[order(de.results$adj.P.Val), ], 10))
top.symbols <- get.gene.name(geo$gene, top.probes, use.symbols = TRUE)
valid <- which(top.symbols != "")
top.probes <- top.probes[valid]
top.symbols <- top.symbols[valid]

expr.mat <- get.gene.expression(geo$expression, top.probes)
df.multi <- build.analysis.df(expr.mat, geo$phenotype, geo$gene)

gene.analysis.plot(df.multi)

## ----single-------------------------------------------------------------------
df.single <- df.multi[df.multi$gene == top.symbols[1], ]
gene.analysis.plot(df.single)
result <- analyze.gene(df.single)
cat(result$interpretation)

## ----coexp--------------------------------------------------------------------
cor.mat <- gene.correlation.matrix(
  geo$expression, top.probes, method = "pearson"
)
correlation.heatmap.plot(cor.mat, gene.names = top.symbols)

## ----gsea---------------------------------------------------------------------
hallmark.df <- msigdbr(species = "Homo sapiens", category = "H")
pathways <- split(hallmark.df$gene_symbol, hallmark.df$gs_name)
gsea.results <- run.gsea(de.results, geo$gene, pathways)
gsea.results <- gsea.results[order(gsea.results$padj), ]
top.pathways <- head(gsea.results[!is.na(gsea.results$padj), ], 15)

## ----gsea_plot, fig.height=7--------------------------------------------------
top.pathways$short.name <- gsub("HALLMARK_", "", top.pathways$pathway)
top.pathways$direction <- ifelse(top.pathways$NES > 0,
                                 "Upregulated in RCC",
                                 "Downregulated in RCC")

ggplot2::ggplot(
  top.pathways,
  ggplot2::aes(
    x = reorder(short.name, NES),
    y = NES,
    fill = direction
  )
) +
  ggplot2::geom_col() +
  ggplot2::coord_flip() +
  ggplot2::scale_fill_manual(
    values = c("Upregulated in RCC" = "firebrick",
               "Downregulated in RCC" = "steelblue")
  ) +
  ggplot2::geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  ggplot2::labs(
    title = "Hallmark Pathway Enrichment in RCC vs Normal Kidney",
    subtitle = "Bars extend from zero; direction indicates enrichment in RCC (red) or normal (blue)",
    x = NULL,
    y = "Normalized Enrichment Score (NES)",
    fill = NULL
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    plot.title = ggplot2::element_text(hjust = 0.5, size = 13),
    plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 9,
                                          color = "grey40"),
    legend.position = "bottom"
  )

## ----lasso--------------------------------------------------------------------
phenotype.binary <- ifelse(geo$phenotype$disease.state == "RCC", 1, 0)
lasso.fit <- fit.lasso(geo$expression, phenotype.binary)

coef.mat <- coef(lasso.fit, s = "lambda.1se")
selected <- coef.mat[coef.mat[, 1] != 0, , drop = FALSE]
n.selected <- nrow(selected) - 1  # subtract intercept

cat("Number of genes selected by LASSO:", n.selected, "\n\n")
print(round(selected, 4))

## ----lasso_plot---------------------------------------------------------------
# Visualize the cross-validation curve to show model selection
plot(lasso.fit, main = "LASSO Cross-Validation Curve")

