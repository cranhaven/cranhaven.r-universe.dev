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

eset <- load.geo.soft("", "GDS507", log.transform = TRUE)

## ----extract------------------------------------------------------------------
geo <- extract.expression(eset)

## ----pca----------------------------------------------------------------------
pca.plot(geo$expression, geo$phenotype, color.by = "disease.state")

## ----limma--------------------------------------------------------------------
de.results <- run.limma.de(geo, condition.col = "disease.state")

## ----inspect_de---------------------------------------------------------------
# Look at the top 10 most significantly differentially expressed probes
head(de.results[order(de.results$adj.P.Val), ], 10)

## ----volcano------------------------------------------------------------------
volcano.plot(de.results, fc.threshold = 1, fdr.threshold = 0.05)

## ----top_probes---------------------------------------------------------------
# Extract the 10 most significant probe IDs (row names of the TopTable)
top.probes <- rownames(head(de.results[order(de.results$adj.P.Val), ], 10))

# Resolve probe IDs to human-readable gene names
top.genes <- get.gene.name(geo$gene, top.probes)

# Some probes on the array do not map to annotated genes, remove those
annotated.mask <- which(top.genes != "")
top.probes <- top.probes[annotated.mask]
top.genes <- top.genes[annotated.mask]

cat("Top differentially expressed genes:\n")
print(top.genes)

## ----single_prep--------------------------------------------------------------
# Retrieve expression values for all top probes as a matrix
expr.mat <- get.gene.expression(geo$expression, top.probes)

# Build the long-format analysis data frame
df.multi <- build.analysis.df(expr.mat, geo$phenotype, geo$gene)

## ----multi_plot---------------------------------------------------------------
# Faceted violin + boxplot across all top genes simultaneously
gene.analysis.plot(df.multi)

## ----single_gene--------------------------------------------------------------
# Subset to a single gene of interest
gene.of.interest <- get.gene.name(geo$gene, top.probes[1], use.symbols=TRUE)
df.single <- df.multi[which(df.multi$gene == gene.of.interest), ]

# Annotated single-gene visualization
gene.analysis.plot(df.single)

## ----analyze------------------------------------------------------------------
result <- analyze.gene(df.single)
cat(result$interpretation)

## ----interpret----------------------------------------------------------------
# Inspect individual components
cat("Effect size (Cohen's d):", round(result$effect.size, 3), "\n")
cat("Effect size class:      ", result$effect.size.class, "\n")
cat("95% CI:                 [",
    round(result$confidence.interval$lower, 3), ",",
    round(result$confidence.interval$upper, 3), "]\n")
cat("Parametric p-value:     ", signif(result$p.value, 3), "\n")
cat("Nonparametric p-value:  ", signif(result$nonparametric.p, 3), "\n")
cat("Robustness:             ", result$robustness, "\n")
cat("Biological assessment:  ", result$biological.relevance, "\n")

## ----gene_sets----------------------------------------------------------------
# Download Hallmark gene sets for Homo sapiens
hallmark.df <- msigdbr::msigdbr(species = "Homo sapiens", category = "H")

# Convert to a named list: pathway name -> character vector of gene symbols
pathways <- split(hallmark.df$gene_symbol, hallmark.df$gs_name)

# Each element is a vector of gene symbols belonging to that pathway
cat("Number of Hallmark gene sets:", length(pathways), "\n")
cat("Example - first 6 genes in HALLMARK_HYPOXIA:\n")
print(head(pathways[["HALLMARK_HYPOXIA"]]))

## ----gsea---------------------------------------------------------------------
gsea.results <- run.gsea(de.results, geo$gene, pathways, min.size = 15, max.size = 500)

# Sort by adjusted p-value and inspect the top enriched pathways
gsea.results <- gsea.results[order(gsea.results$padj), ]
print(head(gsea.results[, c("pathway", "NES", "pval", "padj", "size")], 10))

## ----gsea_followup------------------------------------------------------------
# Extract the leading edge genes of the top enriched pathway
top.pathway <- gsea.results$pathway[1]
leading.genes <- unlist(gsea.results$leadingEdge[1])

cat("Top enriched pathway:", top.pathway, "\n")
cat("Leading edge genes:\n")
print(leading.genes)

# Find probes for the leading edge genes and perform single-gene deep-dives
leading.probes <- find.probe.by.gene(geo$gene, leading.genes)
leading.probes <- leading.probes[leading.probes != ""]

if(length(leading.probes) > 0)
{
  leading.expr <- get.gene.expression(geo$expression, leading.probes)
  df.leading <- build.analysis.df(leading.expr, geo$phenotype, geo$gene)
  gene.analysis.plot(df.leading)
}

## ----coexp--------------------------------------------------------------------
# Compute pairwise Pearson correlations between our top probes
cor.mat <- gene.correlation.matrix(geo$expression, top.probes, method = "pearson")

# Visualize with hierarchical clustering
correlation.heatmap.plot(cor.mat, gene.names = get.gene.name(geo$gene, top.probes, use.symbols=TRUE))

## ----lasso--------------------------------------------------------------------
# Encode disease state as a binary outcome variable
phenotype.binary <- ifelse(geo$phenotype$disease.state == "RCC", 1, 0)

# Fit LASSO logistic regression across all top probes simultaneously
lasso.fit <- fit.lasso(geo$expression, phenotype.binary)

# Extract genes selected by LASSO at the 1-standard-error lambda
coef.mat <- coef(lasso.fit, s = "lambda.1se")
selected  <- coef.mat[coef.mat[, 1] != 0, ]
print(selected)

## ----full_pipeline------------------------------------------------------------
library(BioUtils)

# == 1. Import =================================================================
eset <- load.geo.soft("", "GDS507", log.transform = TRUE)
geo <- extract.expression(eset)

# == 2. Quality Control ========================================================
pca.plot(geo$expression, geo$phenotype, color.by = "disease.state")

# == 3. Genome-Wide Differential Expression ====================================
de.results <- run.limma.de(geo, condition.col = "disease.state")
volcano.plot(de.results, fc.threshold = 1, fdr.threshold = 0.05)

# == 4. Select Top Candidates ==================================================
top.probes <- rownames(head(de.results[order(de.results$adj.P.Val), ], 10))
top.genes <- get.gene.name(geo$gene, top.probes)
valid.mask <- which(top.genes != "")
top.probes <- top.probes[valid.mask]
top.genes <- top.genes[valid.mask]

# == 5. Build Analysis Data Frame ==============================================
expr.mat <- get.gene.expression(geo$expression, top.probes)
df.multi <- build.analysis.df(expr.mat, geo$phenotype, geo$gene)

# == 6. Multi-Gene Visualization ===============================================
gene.analysis.plot(df.multi)

# == 7. Single-Gene Deep-Dive ==================================================
df.single <- df.multi[which(df.multi$gene == get.gene.name(geo$gene, top.probes[1], use.symbols=TRUE)), ]
gene.analysis.plot(df.single)
result <- analyze.gene(df.single)
cat(result$interpretation)

# == 8. Co-expression Analysis =================================================
cor.mat <- gene.correlation.matrix(geo$expression, top.probes, method = "pearson")
correlation.heatmap.plot(cor.mat, gene.names = get.gene.name(geo$gene, top.probes, use.symbols=TRUE))

# == 9. Pathway Enrichment =====================================================
hallmark.df <- msigdbr::msigdbr(species = "Homo sapiens", category = "H")
pathways <- split(hallmark.df$gene_symbol, hallmark.df$gs_name)
gsea.results <- run.gsea(de.results, geo$gene, pathways)
gsea.results <- gsea.results[order(gsea.results$padj), ]
print(head(gsea.results[, c("pathway", "NES", "pval", "padj")], 10))

# == 10. Multi-Gene Predictive Modeling ========================================
phenotype.binary <- ifelse(geo$phenotype$disease.state == "RCC", 1, 0)
lasso.fit <- fit.lasso(geo$expression, phenotype.binary)
coef.mat <- coef(lasso.fit, s = "lambda.1se")
selected <- coef.mat[coef.mat[, 1] != 0, ]
print(selected)

