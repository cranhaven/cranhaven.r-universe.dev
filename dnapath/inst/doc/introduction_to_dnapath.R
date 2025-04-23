## ----setup, message=FALSE, echo=FALSE-----------------------------------------
reset_options_scipen <- getOption("scipen")
reset_options_digits <- getOption("digits")
options(scipen = 1, digits = 5)
library(dnapath)
set.seed(12345)

## -----------------------------------------------------------------------------
data(meso)
str(meso)

## ----warning=FALSE------------------------------------------------------------
# Run dnapath using the gene expression and group information from meso dataset.
results <- dnapath(meso$gene_expression, 
                   pathway_list = NULL, 
                   group_labels = meso$groups)
results

## -----------------------------------------------------------------------------
plot(results, alpha = 0.05, only_dc = TRUE)

## -----------------------------------------------------------------------------
data(meso) # Load the gene expression data
data(p53_pathways)

# Run the differential network analysis.
results <- dnapath(x = meso$gene_expression,
                   pathway_list = p53_pathways,
                   group_labels = meso$groups,
                   seed = 0)

results

## -----------------------------------------------------------------------------
results <- filter_pathways(results, alpha_pathway = 0.1)
results

## -----------------------------------------------------------------------------
results <- sort(results, decreasing = TRUE, by = "n_dc")
results

## -----------------------------------------------------------------------------
# The plot layout is stochastic. Setting the RNG seed allows for reproducible plots.
set.seed(0)
plot(results[[1]], alpha = 0.05, only_dc = TRUE)

## -----------------------------------------------------------------------------
results <- rename_genes(results, to = "symbol", species = "human",
                        dir_save = tempdir())
results[[1]] # Print the results for the first pathway.

## -----------------------------------------------------------------------------
set.seed(0) # Reset seed to use same layout as previous plot.
plot(results[[1]], alpha = 0.05, only_dc = TRUE)

## -----------------------------------------------------------------------------
# Summary table of the edges in pathway 1.
summarize_edges(results[[1]], alpha = 0.05)

## -----------------------------------------------------------------------------
library(dplyr)
tab <- summarize_edges(results[[1]])
tab <- dplyr::arrange(tab, p_value, decreasing = FALSE)
tab <- dplyr::filter(tab, pmax(abs(nw1), abs(nw2)) > 0.2)
tab

## -----------------------------------------------------------------------------
plot_pair(results, "BANP", "TP53")

## -----------------------------------------------------------------------------
plot_pair(results, "BANP", "TP53", method = "lm")

## -----------------------------------------------------------------------------
set.seed(0) # Reset seed to use same layout as previous plot.
plot(results[[1]], alpha = 0.05, only_dc = TRUE, require_dc_genes = TRUE)
summarize_edges(results[[1]], alpha = 0.05, require_dc_genes = TRUE)

## -----------------------------------------------------------------------------
set.seed(0) # Reset seed to use same layout as previous plot.
plot(results[[1]], alpha = 0.05)

## ----echo=FALSE---------------------------------------------------------------
options(scipen = reset_options_scipen, digits = reset_options_digits)

