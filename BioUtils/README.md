# BioUtils

**BioUtils** is an end-to-end R toolkit for analyzing gene expression data from GEO datasets.
It provides a unified workflow for differential expression, statistical testing, visualization, and biological interpretation.

---

## Features

* Load and preprocess GEO datasets
* Differential expression analysis using limma
* Visualization (PCA, volcano plots, gene-level plots)
* Statistical testing (adaptive t-test, effect size, bootstrapped CI)
* Gene set enrichment analysis (GSEA)
* Machine learning (LASSO biomarker selection)
* Gene co-expression analysis

---

## Installation

```r
install.packages("remotes")
remotes::install_github("spencertreadway/BioUtils")
```

---

## Example Workflow

```r
# Load data
eset <- load.geo.soft("GDS507.soft", log.transform = TRUE)
geo <- extract.expression(eset)

# PCA visualization
pca.plot(geo$expression, geo$phenotype, color.by = "disease.state")

# Differential expression
de.results <- run.limma.de(geo)

# Volcano plot
volcano.plot(de.results, fc.threshold = 0.3)

# Select top genes
top.genes <- head(rownames(de.results[order(de.results$adj.P.Val), ]), 5)
probe.ids <- find.probe.by.gene(geo$gene, top.genes)

# Single gene analysis
expr <- get.gene.expression(geo$expression, probe.ids[1])
df <- build.analysis.df(expr, geo$phenotype, geo$gene)
gene.analysis.plot(df)

# LASSO model
phenotype.binary <- ifelse(geo$phenotype$disease.state == "disease", 1, 0)
lasso.fit <- fit.lasso(geo$expression, phenotype.binary)
```

---

## Interpretation

BioUtils integrates multiple layers of analysis:

* **PCA** reveals global structure in the data
* **Differential expression (limma)** identifies significant genes
* **Effect size & CI** quantify biological impact
* **LASSO** selects predictive biomarkers
* **GSEA** links results to biological pathways

---

## Documentation

Full documentation is available at https://spencertreadway.github.io/BioUtils/ or via:

```r
help(package = "BioUtils")
```

---

## Notes

* Probe-to-gene mapping depends on GEO platform annotations
* Fold-change thresholds are user-defined and dataset-dependent
* Statistical significance does not always imply biological relevance

---

## License

MIT

---

## Author

Spencer Treadway
