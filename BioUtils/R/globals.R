#' BioUtils: Tools for Biological Data Analysis and Visualization
#'
#' BioUtils provides a collection of tools for preprocessing biological
#' datasets, performing statistical analyses, and generating visualizations.
#' See \code{vignette("bioutils-case-study")} for a full workflow walkthrough.
#'
#' @keywords internal
#' @importFrom stats cor model.matrix na.omit oneway.test p.adjust prcomp quantile sd t.test var.test wilcox.test
#' @importFrom utils head globalVariables
"_PACKAGE"

utils::globalVariables(c(
  # build.analysis.df — tidyselect column reference
  "probe",
  # stats_tests.R / visualization.R — ggplot2 aes() and formula objects
  "expression", "group", "gene", "ID",
  # pca.plot — columns created from prcomp output before aes()
  "PC1", "PC2",
  # volcano.plot — columns created on de.results before aes()
  "logFC", "neg.log10.fdr", "status"
))
