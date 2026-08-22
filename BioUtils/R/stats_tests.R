#' Compute Effect Size (Cohen's d)
#'
#' Calculates the standardized mean difference between two groups.
#'
#' @param df A data frame containing at least two columns:
#' \describe{
#'   \item{expression}{Numeric vector of gene expression values.}
#'   \item{group}{Character or factor vector with exactly two group labels,
#'     as produced by \code{build.analysis.df()}.}
#' }
#'
#' @return Numeric. Cohen's d value. Positive values indicate the first
#'   group (alphabetically) has higher mean expression; negative values
#'   indicate the second group is higher.
#'
#' @export
compute.effect.size <- function(df)
{
  groups <- split(df$expression, df$group)

  if(length(groups) < 2)
  {
    return(NA)
  }

  g1 <- groups[[1]]
  g2 <- groups[[2]]

  mean1 <- mean(g1)
  mean2 <- mean(g2)

  sd1 <- sd(g1)
  sd2 <- sd(g2)

  n1 <- length(g1)
  n2 <- length(g2)

  pooled.sd <- sqrt(((n1 - 1)*sd1^2 + (n2 - 1)*sd2^2) / (n1 + n2 - 2))
  d <- (mean1 - mean2) / pooled.sd

  return(d)
}

#' Compute Confidence Interval for Effect Size
#'
#' Estimates a confidence interval for Cohen's d using bootstrap resampling.
#' This provides a measure of uncertainty around the effect size estimate,
#' which is especially useful when sample sizes are small or distributions
#' deviate from normality.
#'
#' @param df A data frame containing at least two columns:
#' \describe{
#'   \item{expression}{Numeric vector of gene expression values.}
#'   \item{group}{Character or factor vector indicating group membership.}
#' }
#' @param alpha Numeric. Significance level used to compute the confidence
#'   interval bounds. Default is \code{0.05}, yielding a 95 percent CI.
#'
#' @param n.boot Integer. Number of bootstrap resamples to perform.
#'   Default is \code{1000}.
#'
#' @return A named list with two elements:
#' \describe{
#'   \item{lower}{Lower bound of the bootstrapped confidence interval.}
#'   \item{upper}{Upper bound of the bootstrapped confidence interval.}
#' }
#'
#' @details
#' The function repeatedly resamples the input data with replacement and
#' recomputes Cohen's d for each resample. The confidence interval is
#' derived from the empirical distribution of bootstrapped effect sizes
#' using the percentile method.
#'
#' @examples
#' \donttest{
#' # Build a minimal example data frame
#' analysis.df <- data.frame(
#'   expression = c(1.2, 2.3, 1.8, 2.1, 3.4, 2.9, 3.1, 2.7),
#'   group = rep(c("normal", "RCC"), each = 4)
#' )
#' ci <- compute.ci(analysis.df, alpha = 0.05, n.boot = 100)
#' }
#'
#' @export
compute.ci <- function(df, alpha=0.05, n.boot=1000)
{
  boot.ds <- numeric(n.boot)

  for(i in 1:n.boot)
  {
    sample.df <- df[sample(nrow(df), replace=TRUE), ]
    boot.ds[i] <- compute.effect.size(sample.df)
  }

  boot.ds <- boot.ds[!is.na(boot.ds)]

  ci <- quantile(boot.ds, c(alpha / 2, 1 - (alpha / 2)), na.rm=TRUE)

  return(list(
    lower = ci[1],
    upper = ci[2]
  ))
}

#' Perform Nonparametric Group Comparison
#'
#' Conducts a Wilcoxon rank-sum test (Mann-Whitney U test) to compare
#' expression values between two groups without assuming normality.
#'
#' @param df A data frame containing at least two columns:
#' \describe{
#'   \item{expression}{Numeric vector of gene expression values.}
#'   \item{group}{Character or factor vector with exactly two group labels.}
#' }
#'
#' @return A named list with two elements:
#' \describe{
#'   \item{p.value}{Numeric. P-value from the Wilcoxon rank-sum test.}
#'   \item{test}{Character. Name of the test used (\code{"Wilcoxon rank-sum"}).}
#' }
#'
#' @details
#' This test is useful as a robustness check against parametric methods
#' such as the t-test, especially when data are skewed or contain outliers.
#' The result is used by \code{analyze.gene()} to populate the
#' \code{robustness} field of its return value.
#'
#' @examples
#' \donttest{
#' analysis.df <- data.frame(
#'   expression = c(1.2, 2.3, 1.8, 2.1, 3.4, 2.9, 3.1, 2.7,
#'                  1.5, 2.0, 1.9, 2.4),
#'   group      = rep(c("normal", "RCC"), each = 6)
#' )
#' res <- nonparametric.test(analysis.df)
#' res$p.value
#' }
#'
#' @export
nonparametric.test <- function(df)
{
  res <- wilcox.test(expression ~ group, data=df)

  return(list(
    p.value = res$p.value,
    test = "Wilcoxon rank-sum"
  ))
}

#' Classify Effect Size
#'
#' Categorizes an absolute Cohen's d value into a qualitative magnitude label
#' using conventional thresholds.
#'
#' @param d Numeric. Cohen's d effect size value (signed or unsigned).
#'
#' @return Character string. One of \code{"negligible"}, \code{"small"},
#'   \code{"moderate"}, or \code{"large"} based on the absolute value of
#'   \code{d}, using the thresholds: negligible < 0.2 <= small < 0.5 <=
#'   moderate < 0.8 <= large.
#'
#' @examples
#' \donttest{
#' classify.effect.size(0.3)   # "small"
#' classify.effect.size(-0.9)  # "large"
#' }
#'
#' @export
classify.effect.size <- function(d)
{
  abs.d <- abs(d)

  if(abs.d < 0.2)
  {
    return("negligible")
  }
  else if(abs.d < 0.5)
  {
    return("small")
  }
  else if(abs.d < 0.8)
  {
    return("moderate")
  }
  else
  {
    return("large")
  }
}

#' Assess Biological Relevance of Gene Expression Differences
#'
#' Provides a heuristic interpretation of whether a statistically significant
#' difference in gene expression is likely to be biologically meaningful,
#' based on effect size magnitude and p-value.
#'
#' @param effect.size Numeric. Cohen's d or other standardized effect size.
#' @param p.value Numeric. P-value from a statistical test.
#' @param alpha Numeric. Significance threshold used for the p-value
#'   comparison. Default is \code{0.05}.
#'
#' @return Character string. One of three heuristic labels:
#' \describe{
#'   \item{"Potentially biologically meaningful"}{p < alpha and |d| > 0.5}
#'   \item{"Statistically significant but small effect"}{p < alpha and |d| <= 0.5}
#'   \item{"No strong evidence of biological relevance"}{p >= alpha}
#' }
#'
#' @details
#' These rules are heuristic and intended as guidance rather than definitive
#' biological conclusions. The thresholds for effect size (0.5) and
#' significance are conventional starting points; domain knowledge should
#' inform interpretation. This function is called internally by
#' \code{analyze.gene()} using the same \code{alpha} passed to that function.
#'
#' @examples
#' \donttest{
#' flag.biological.relevance(effect.size = 0.6, p.value = 0.01)
#' }
#'
#' @export
flag.biological.relevance <- function(effect.size, p.value, alpha=0.05)
{
  if(p.value < alpha && abs(effect.size) > 0.5)
  {
    return("Potentially biologically meaningful")
  }

  if(p.value < alpha && abs(effect.size) <= 0.5)
  {
    return("Statistically significant but small effect")
  }

  return("No strong evidence of biological relevance")
}

#' Perform Adaptive T-Test
#'
#' Automatically selects between Welch's and Student's t-test based on the
#' result of a variance equality test, then returns a unified result structure
#' regardless of which branch was taken.
#'
#' @param df A data frame containing at least two columns:
#' \describe{
#'   \item{expression}{Numeric vector of gene expression values.}
#'   \item{group}{Character or factor vector with exactly two group labels.}
#' }
#' @param alpha Numeric. Significance level for the variance equality test
#'   (\code{var.test()}). Default is \code{0.05}.
#'
#' @return A named list with three elements:
#' \describe{
#'   \item{variance.test}{The result object from \code{var.test()} or
#'     \code{oneway.test()}, depending on the branch taken.}
#'   \item{t.test}{The result object from \code{t.test()}.}
#'   \item{p.value}{Numeric. The p-value from the t-test.}
#' }
#'
#' @details
#' If \code{var.test()} returns a p-value below \code{alpha}, variances are
#' considered unequal and Welch's t-test (\code{var.equal = FALSE}) is used.
#' Otherwise, Student's t-test (\code{var.equal = TRUE}) is applied alongside
#' a one-way ANOVA as a confirmatory check. The returned list is consumed
#' internally by \code{analyze.gene()}.
#'
#' @examples
#' \donttest{
#' # Build a minimal example data frame
#' analysis.df <- data.frame(
#'   expression = c(1.2, 2.3, 1.8, 2.1, 3.4, 2.9, 3.1, 2.7),
#'   group = rep(c("normal", "RCC"), each = 4)
#' )
#' result <- adaptive.t.test(analysis.df, alpha = 0.05)
#' result$p.value
#' }
#'
#' @export
adaptive.t.test <- function(df, alpha=0.05)
{
  variance.result <- var.test(expression ~ group, data=df)

  if(variance.result$p.value < alpha)
  {
    t.result <- t.test(expression ~ group, data=df, var.equal=FALSE, conf.level=1-alpha)

    return(list(
      variance.test = variance.result,
      t.test = t.result,
      p.value = t.result$p.value
    ))
  }
  else
  {
    t.result <- t.test(expression ~ group, data=df, var.equal=TRUE, conf.level=1-alpha)
    anova.result <- oneway.test(expression ~ group, data=df, var.equal=TRUE)

    return(list(
      variance.test = anova.result,
      t.test = t.result,
      p.value = t.result$p.value
    ))
  }
}

#' Analyze Gene Expression Between Groups
#'
#' Performs a comprehensive statistical analysis comparing gene expression
#' between two groups. Integrates parametric and nonparametric testing,
#' effect size estimation, bootstrapped confidence intervals, and a
#' heuristic biological relevance assessment.
#'
#' @param df A data frame as produced by \code{build.analysis.df()},
#'   containing at least two columns:
#' \describe{
#'   \item{expression}{Numeric vector of gene expression values.}
#'   \item{group}{Character or factor vector with exactly two group labels.}
#' }
#' When the data frame contains multiple genes (i.e., a \code{gene} column
#' with more than one unique value), expression values from all genes are
#' pooled together. Subset the data frame to a single gene before calling
#' this function for per-gene results.
#' @param alpha Numeric. Significance threshold. Default is \code{0.05}.
#' @param n.boot Integer. Number of bootstrap resamples for the confidence
#'   interval. Default is \code{1000}.
#'
#' @return A named list with nine elements:
#' \describe{
#'   \item{p.value}{Numeric. P-value from the adaptive t-test.}
#'   \item{effect.size}{Numeric. Cohen's d.}
#'   \item{effect.size.class}{Character. Qualitative magnitude label from
#'     \code{classify.effect.size()}.}
#'   \item{confidence.interval}{Named list with \code{lower} and \code{upper}
#'     bounds for the bootstrapped CI around Cohen's d.}
#'   \item{nonparametric.p}{Numeric. P-value from the Wilcoxon rank-sum test.}
#'   \item{robustness}{Character. Agreement assessment between parametric and
#'     nonparametric tests.}
#'   \item{biological.relevance}{Character. Heuristic relevance label from
#'     \code{flag.biological.relevance()}.}
#'   \item{interpretation}{Character. Human-readable summary combining all
#'     of the above.}
#'   \item{raw}{List. Raw output from the parametric and nonparametric tests
#'     for further inspection.}
#' }
#'
#' @details
#' This function integrates multiple statistical perspectives to provide a
#' more complete picture of group differences than a p-value alone. The
#' result is designed to feed directly into \code{plot.gene.analysis()} for
#' visualization.
#'
#' @examples
#' \donttest{
#' geo <- extract.expression(load.geo.soft(accession = "GDS3268", log.transform = TRUE))
#' probe <- find.probe.by.gene(geo$gene, "mucin 20, cell surface associated")
#' expr <- get.gene.expression(geo$expression, probe)
#' analysis.df <- build.analysis.df(expr, geo$phenotype, geo$gene)
#' result <- analyze.gene(analysis.df, n.boot=100)
#' cat(result$interpretation)
#' }
#'
#' @export
analyze.gene <- function(df, alpha=0.05, n.boot=1000)
{
  if(!all(c("expression", "group") %in% colnames(df)))
  {
    stop("Data frame must contain 'expression' and 'group' columns.")
  }

  if(length(unique(df$group)) != 2)
  {
    stop("The 'group' column must contain exactly two groups.")
  }

  test.results <- adaptive.t.test(df, alpha)
  p.value <- test.results$p.value

  d <- compute.effect.size(df)
  d.class <- classify.effect.size(d)

  ci <- compute.ci(df, alpha, n.boot)

  np.result <- nonparametric.test(df)
  np.p <- np.result$p.value

  if(p.value < alpha && np.p < alpha)
  {
    robustness <- "robust across parametric and nonparametric tests"
  }
  else if(p.value < alpha && np.p >= alpha)
  {
    robustness <- "sensitive to distributional assumptions"
  }
  else
  {
    robustness <- "no consistent evidence of difference"
  }

  bio.flag <- flag.biological.relevance(d, p.value, alpha)

  interpretation <- paste(
    "The difference is ",
    ifelse(p.value < alpha, "statistically significant ", "not statistically significant "),
    "with a ", d.class, " effect size.",
    "\nResult is ", robustness, ".\n",
    bio.flag, ".", sep=""
  )

  return(list(
    p.value = p.value,
    effect.size = d,
    effect.size.class = d.class,
    confidence.interval = ci,
    nonparametric.p = np.p,
    robustness = robustness,
    biological.relevance = bio.flag,
    interpretation = interpretation,
    raw = list(
      parametric = test.results,
      nonparametric = np.result
    )
  ))
}

#' Run Differential Expression Analysis Using limma
#'
#' Performs genome-wide differential expression analysis across all probes
#' using the limma linear modeling framework. This is the recommended approach
#' for microarray data from GEO, as it borrows variance information across
#' genes to produce more stable estimates than gene-by-gene t-tests.
#'
#' @param geo Named list as returned by \code{extract.expression()}, containing
#'   at minimum \code{$expression} (probe x sample matrix) and
#'   \code{$phenotype} (sample metadata data frame).
#'
#' @param condition.col Character. Name of the column in \code{geo$phenotype}
#'   to use as the grouping variable. Default is \code{"disease.state"}.
#'
#' @param adjust.method Character. P-value correction method passed to
#'   \code{limma::topTable()}. Default is \code{"BH"} (Benjamini-Hochberg FDR).
#'
#' @return A data frame (TopTable) with one row per probe, sorted by evidence
#'   of differential expression, containing:
#' \describe{
#'   \item{logFC}{Log2 fold-change between conditions.}
#'   \item{AveExpr}{Average log2 expression across all samples.}
#'   \item{t}{Moderated t-statistic.}
#'   \item{P.Value}{Raw p-value.}
#'   \item{adj.P.Val}{FDR-adjusted p-value (BH by default).}
#'   \item{B}{Log-odds that the gene is differentially expressed.}
#' }
#'
#' @details
#' Unlike running \code{analyze.gene()} on each probe individually, limma fits
#' a linear model to all probes simultaneously and applies empirical Bayes
#' shrinkage to the variance estimates. This stabilizes results for genes with
#' few observations and is the standard method for microarray DE analysis.
#'
#' The returned TopTable is the primary input for downstream functions such as
#' \code{volcano.plot()} and \code{run.gsea()}, and can be filtered by
#' \code{adj.P.Val} and \code{logFC} thresholds to define a gene signature.
#'
#' @examples
#' \donttest{
#' geo <- extract.expression(load.geo.soft(accession = "GDS3268", log.transform = TRUE))
#' de.results <- run.limma.de(geo, condition.col = "disease.state")
#' head(de.results)
#' }
#'
#' @export
run.limma.de <- function(geo, condition.col="disease.state", adjust.method="BH")
{
  group <- factor(geo$phenotype[[condition.col]])
  design <- model.matrix(~ group)

  fit <- limma::lmFit(geo$expression, design)
  fit <- limma::eBayes(fit)

  return(limma::topTable(fit, adjust.method=adjust.method, number=Inf))
}

#' Adjust P-Values for Multiple Comparisons
#'
#' Applies a multiple testing correction to a vector of raw p-values, reducing
#' the false discovery rate when many genes are tested simultaneously.
#'
#' @param p.values Numeric vector of raw p-values, one per gene or probe.
#' @param method Character. Correction method passed to \code{p.adjust()}.
#'   Common options are \code{"BH"} (Benjamini-Hochberg), \code{"bonferroni"},
#'   and \code{"holm"}. Default is \code{"BH"}.
#'
#' @return A numeric vector of adjusted p-values, the same length and order
#'   as the input. For the BH method values represent the estimated false
#'   discovery rate (FDR); for Bonferroni and Holm they represent the
#'   family-wise error rate.
#'
#' @details
#' When \code{analyze.gene()} is applied across many probes in a loop, each
#' test is performed independently and p-values are not corrected for
#' multiplicity. This function should be applied to the resulting p-value
#' vector before interpreting significance. BH correction is recommended for
#' exploratory genomic analyses; Bonferroni is more conservative and suited
#' to confirmatory settings. For genome-wide analysis, prefer
#' \code{run.limma.de()} which handles correction internally.
#'
#' @examples
#' \donttest{
#' geo <- extract.expression(load.geo.soft(accession = "GDS507", log.transform = TRUE))
#' probe.ids <- find.probe.by.gene(geo$gene, c("GENE1", "GENE2", "GENE3"))
#'
#' raw.pvals <- sapply(probe.ids, function(id) {
#'   expr <- get.gene.expression(geo$expression, id)
#'   df <- build.analysis.df(expr, geo$phenotype, geo$gene)
#'   analyze.gene(df)$p.value
#' })
#' adj.pvals <- adjust.pvalues(raw.pvals, method = "BH")
#' }
#'
#' @export
adjust.pvalues <- function(p.values, method="BH")
{
  return(p.adjust(p.values, method=method))
}

#' Compute Pairwise Gene Co-expression Correlation Matrix
#'
#' Calculates pairwise correlations between a set of genes across all samples,
#' producing a symmetric correlation matrix that quantifies co-expression
#' relationships.
#'
#' @param expression.matrix Numeric matrix of gene expression values as returned
#'   by \code{extract.expression()$expression}. Rows are probes, columns are
#'   samples.
#'
#' @param probe.ids Integer vector of probe IDs to include, as returned by
#'   \code{find.probe.by.gene()}.
#' @param method Character. Correlation method: \code{"pearson"} (default),
#'   \code{"spearman"}, or \code{"kendall"}. Spearman is recommended when
#'   distributions are skewed or outliers are a concern.
#'
#' @return A symmetric numeric matrix of dimensions \code{length(probe.ids)} x
#'   \code{length(probe.ids)}, where each cell contains the pairwise correlation
#'   coefficient across all samples. Diagonal values are 1. Row and column
#'   names correspond to probe IDs.
#'
#' @details
#' Co-expression correlations capture whether two genes tend to be
#' simultaneously up- or down-regulated across samples, which can suggest
#' shared regulatory control or pathway membership. The resulting matrix is
#' the direct input for \code{plot.correlation.heatmap()}.
#'
#' @examples
#' \donttest{
#' set.seed(42)
#' expr.mat <- matrix(rnorm(400), nrow = 4, ncol = 100)
#' rownames(expr.mat) <- c(101, 102, 103, 104)
#' probe.ids <- c(101, 102, 103, 104)
#' cor.mat <- gene.correlation.matrix(expr.mat, probe.ids)
#' correlation.heatmap.plot(cor.mat, gene.names = c("BRCA1", "TP53", "MYC", "EGFR"))
#' }
#'
#' @export
gene.correlation.matrix <- function(expression.matrix, probe.ids, method="pearson")
{
  subset <- expression.matrix[which(rownames(expression.matrix) %in% probe.ids),]
  return(cor(t(subset), method=method))
}

#' Fit LASSO Regression for Multi-Gene Biomarker Discovery
#'
#' Applies cross-validated LASSO logistic regression to identify a sparse set
#' of genes that jointly predict a binary phenotype. Unlike univariate tests,
#' LASSO accounts for the joint contribution of all genes simultaneously,
#' automatically penalizing redundant predictors.
#'
#' @param expression.matrix Numeric matrix of gene expression values as returned
#'   by \code{extract.expression()$expression}. Rows are probes, columns are
#'   samples. The matrix is transposed internally so samples become rows as
#'   required by \code{glmnet}.
#'
#' @param phenotype.vector Factor or integer vector of binary phenotype labels,
#'   one per sample (e.g., \code{0} for control, \code{1} for disease). Must
#'   be the same length as the number of columns in \code{expression.matrix}.
#' @param alpha Numeric. Elastic net mixing parameter. \code{1} (default) gives
#'   pure LASSO; \code{0} gives Ridge; values between 0 and 1 give elastic net.
#' @param nfolds Integer. Number of cross-validation folds. Default is \code{10}.
#'
#' @return A \code{cv.glmnet} object. Key elements:
#' \describe{
#'   \item{lambda.min}{Lambda with minimum cross-validated error.}
#'   \item{lambda.1se}{Largest lambda within 1 SE of the minimum (sparser model).}
#'   \item{glmnet.fit}{The full sequence of fitted models across lambda values.}
#' }
#' Extract selected genes with \code{coef(fit, s = "lambda.1se")}; probes with
#' non-zero coefficients are the LASSO-selected biomarkers.
#'
#' @details
#' LASSO is suited to high-dimensional genomic data where the number of genes
#' far exceeds samples. It shrinks most coefficients to exactly zero, retaining
#' only genes with independent predictive value. This complements
#' \code{run.limma.de()} - while limma ranks genes by individual differential
#' expression, LASSO identifies the minimal subset with non-redundant joint
#' predictive signal.
#'
#' @examples
#' \donttest{
#' # Synthetic example — small expression matrix with binary outcome
#' set.seed(42)
#' expr.mat <- matrix(rnorm(200), nrow = 20, ncol = 10)
#' rownames(expr.mat) <- paste0("probe", 1:20)
#' colnames(expr.mat) <- paste0("sample", 1:10)
#' phenotype.binary <- c(0, 0, 0, 0, 0, 1, 1, 1, 1, 1)
#' lasso.fit <- fit.lasso(expr.mat, phenotype.binary)
#' coef(lasso.fit, s = "lambda.1se")
#' }
#'
#' @export
fit.lasso <- function(expression.matrix, phenotype.vector, alpha=1, nfolds=10)
{
  x <- t(expression.matrix)
  fit <- glmnet::cv.glmnet(
    x, phenotype.vector,
    alpha = alpha,
    family = "binomial",
    nfolds = nfolds
  )

  return(fit)
}

#' Run Gene Set Enrichment Analysis
#'
#' Tests whether predefined gene sets (e.g., pathways or GO terms) are
#' systematically enriched among genes ranked by differential expression,
#' using the fgseaMultilevel algorithm from \code{fgsea}.
#'
#' @param de.results Data frame as returned by \code{run.limma.de()}.
#'   Must contain a \code{logFC} column. Row names are probe IDs.
#' @param genes Data frame of gene annotations as returned by
#'   \code{extract.expression()$gene}. Must contain columns \code{"ID"} for
#'   probe identifiers and \code{"Gene symbol"} for gene symbols matching
#'   the format used by pathway databases such as MSigDB.
#' @param pathways Named list of character vectors, where each element is a
#'   gene set and names are pathway labels. MSigDB or GO gene sets in this
#'   format can be loaded via \code{msigdbr}.
#' @param min.size Integer. Minimum number of genes required in a gene set
#'   for it to be tested. Default is \code{15}.
#' @param max.size Integer. Maximum gene set size. Default is \code{500}.
#'
#' @return A data frame with one row per tested pathway, containing:
#' \describe{
#'   \item{pathway}{Gene set name.}
#'   \item{pval}{Nominal p-value.}
#'   \item{padj}{BH-adjusted p-value across all tested pathways.}
#'   \item{NES}{Normalized enrichment score. Positive values indicate
#'     enrichment among upregulated genes; negative values indicate
#'     enrichment among downregulated genes.}
#'   \item{size}{Number of genes from the pathway present in the ranked list.}
#'   \item{leadingEdge}{List-column of genes driving the enrichment score.}
#' }
#'
#' @details
#' Uses the \code{fgseaMultilevel} algorithm, which estimates p-values using
#' an adaptive multilevel splitting Monte Carlo approach. This is more
#' accurate than the original permutation-based \code{fgseaSimple} and does
#' not require a fixed permutation count.
#'
#' GSEA operates on the full ranked gene list from \code{run.limma.de()}
#' rather than a filtered subset, preserving information from all genes. Probe
#' IDs from the TopTable row names are resolved to gene symbols via the
#' \code{genes} annotation data frame before matching against pathway gene
#' sets. Probes with no gene annotation are silently dropped from the ranked
#' list, as they cannot be matched to any pathway. The \code{leadingEdge}
#' genes are strong candidates for follow-up with \code{analyze.gene()} or
#' \code{gene.analysis.plot()}.
#'
#' @examples
#' \donttest{
#' library(msigdbr)
#' geo <- extract.expression(load.geo.soft(accession = "GDS507",
#'                                                  log.transform = TRUE))
#' hallmark.df <- msigdbr(species = "Homo sapiens", category = "H")
#' pathways <- split(hallmark.df$gene_symbol, hallmark.df$gs_name)
#' de.results <- run.limma.de(geo)
#' gsea.results <- run.gsea(de.results, geo$gene, pathways)
#' head(gsea.results[order(gsea.results$padj), ])
#' }
#'
#' @export
run.gsea <- function(de.results, genes, pathways, min.size=15, max.size=500)
{
  # Resolve probe IDs to gene symbols
  matched <- genes[which(genes$ID %in% rownames(de.results)), ]
  gene.symbols <- matched[["Gene symbol"]]
  probe.ids <- matched[["ID"]]

  ranked.genes <- de.results[probe.ids, "logFC"]
  names(ranked.genes) <- gene.symbols

  # Drop unannotated probes
  ranked.genes <- ranked.genes[names(ranked.genes) != ""]

  # Collapse duplicate gene symbols by keeping the probe with the
  # highest absolute fold change. Multiple probes per gene is common
  # on microarrays and fgsea requires unique gene names
  ranked.genes <- ranked.genes[order(abs(ranked.genes), decreasing=TRUE)]
  ranked.genes <- ranked.genes[!duplicated(names(ranked.genes))]
  ranked.genes <- sort(ranked.genes, decreasing=TRUE)

  fgsea::fgsea(
    pathways = pathways,
    stats = ranked.genes,
    minSize = min.size,
    maxSize = max.size
  )
}
