#' Run mixed linear model with random effect using lme4
#'
#' @param X input data frame
#' @return a dataframe
#'
#' @importFrom lme4 lmer
#' @importFrom stats coef
lme4_model <- function(X) {
  lmeModel <- lmer(log_cov ~ GC_content + (1 | sample:contig), data = X, REML = FALSE)

  lmeModelCoef <- coef(lmeModel)$`sample:contig`
  lmeModelCoef$s_c <- rownames(lmeModelCoef)

  lmeModelCoef
}

#' A function representing the pipeline of four steps
#' including GC bias correction, sample filtration, PCA and contig filtration
#' @param Y a matrix of coverages
#' @param i cutoff of filtering samples changes according to parameter i; i=1, cutoffRatio is 0.5; i=2, cutoffRatio is 1 as contig is clean
#' @return a named list
#' \itemize{
#'   \item samples: final list of filtered samples
#'   \item correct_ys: dataframe with correct Y values per contig/sample
#'   \item pc1: PC1 results of PCA per contig
#'   \item pc1_range: range of PC1
#'   \item samples_y: samples filtered for reliable coverage
#' }
#'
#' @importFrom stats coef cor cor.test ks.test p.adjust aggregate
pipeline <- function(Y, i) {
  PC1 <- contig <- correctY <- NULL

  lmeModelCoef <- lme4_model(Y)

  summeryMeanY <- aggregate(GC_content ~ (sample:contig), Y, FUN = "mean")
  summeryMeanY$s_c <- paste(summeryMeanY$sample, summeryMeanY$contig, sep = ":")

  summeryMeanYSort <- merge(lmeModelCoef, summeryMeanY, by = "s_c")
  summeryMeanYSort$correctY <- summeryMeanYSort$GC_content.x * mean(summeryMeanYSort$GC_content.y) + summeryMeanYSort$`(Intercept)` ###

  # remove samples with no coverage for most of contigs
  summeryMeanYSort2 <- summeryMeanYSort

  ### cutoff of filtering samples changes according to parameter i
  ### i=1, cutoffRatio is 0.5; i=2, cutoffRatio is 1 as contig is clean
  i <- 1
  Samples_filteredY <- filter_sample(summeryMeanYSort2, 0, 1 / (3 - i))
  if (length(Samples_filteredY) < 2) {
    return("too few (<2) samples with reliable coverages for the set of contigs")
  }

  # summeryMeanYSortFilterWide is the relatively clean matrix with more confident samples and the corresponding contigs with available values
  summeryMeanYSortFilterWide <- reshape_filtered(Samples_filteredY, summeryMeanYSort2)

  # do PCA for contigs
  pca <- contig_pca(summeryMeanYSortFilterWide)
  ksResult <- ks.test(pca$PC1, "punif", min(pca$PC1), max(pca$PC1))

  # all good contigs follow uniform distribution
  range <- select_by_ks_test(sort(pca$PC1))

  if (range[3] == TRUE) {
    contigPCAPC1Filtered <- subset(pca, PC1 >= range[1] & PC1 <= range[2])
  } else {
    return("cannot find a continuous set of contigs in uniform distribution")
  }
  # largerClusterContig contains contigs within the range consistent with uniform distribution
  largerClusterContig <- rownames(contigPCAPC1Filtered)

  summeryMeanYSort2 <- subset(summeryMeanYSort, contig %in% largerClusterContig)
  summeryMeanYSortWide <- reshape2::dcast(subset(summeryMeanYSort2, select = c("sample", "contig", "correctY")), contig ~ sample, value.var = "correctY")
  summeryMeanYSortWide2 <- subset(summeryMeanYSortWide, select = -c(contig))
  rownames(summeryMeanYSortWide2) <- summeryMeanYSortWide$contig
  # summeryMeanYSortWide <- reshapeRmNA(summeryMeanYSort2)
  ### debug to skip cor.test when too few values
  summaryMeanCor <- data.frame(Mean = apply(summeryMeanYSortWide2, 2, function(x) mean(x, na.rm = TRUE)), NAprtg = apply(summeryMeanYSortWide2, 2, function(x) length(x[is.na(x)])), cor.p = apply(summeryMeanYSortWide2, 2, function(x) ifelse(length(x[!is.na(x)]) >= 5, cor.test(contigPCAPC1Filtered$PC1, x)$p.value, NA)), KS.p = apply(summeryMeanYSortWide2, 2, function(x) ifelse(length(x[!is.na(x)]) >= 5, ks(x), NA)))
  ### use the cutoff adjust.cor.p
  ### should have few missing contigs(cleaned), cutoff of percentage 0.05 here

  summaryMeanCor$adjust.cor.p <- p.adjust(summaryMeanCor$cor.p, "BH")
  summaryMeanCor$adjust.KS.p <- p.adjust(summaryMeanCor$KS.p, "BH")
  SamplesFilteredFinal <- rownames(summaryMeanCor[summaryMeanCor$NAprtg <= 0.05 * nrow(summeryMeanYSortWide2) & (summaryMeanCor$Mean > 0 | summaryMeanCor$adjust.cor.p < 0.05), ])

  summeryMeanYSortFilteredSampleContig <- subset(summeryMeanYSort2, sample %in% SamplesFilteredFinal, select = c(sample, contig, correctY))

  return(list(samples = SamplesFilteredFinal, correct_ys = summeryMeanYSortFilteredSampleContig, pc1 = contigPCAPC1Filtered, pc1_range = range, samples_y = Samples_filteredY))
}

#' A function for iteration of pipeline until convergence
#' @param Z a matrix of coverages
#' @return a named list
#' \itemize{
#'   \item samples: vector of final filtered samples
#'   \item correct_ys: matrix of sample, contig and corrected coverages
#'   \item pc1: matrix of contig and PC1 values
#'   \item pc1_range: vector of PC1 range
#'   \item samples_y: samples filtered for reliable coverage
#' }
iterate_pipelines <- function(Z) {
  contig <- NULL

  repeat {
    pipeline <- pipeline(Z, 1)
    if (length(pipeline) == 1) {
      return(pipeline)
    }

    ### until convergence
    if ((length(unique(Z$sample)) == length(pipeline$samples)) && (length(unique(Z$contig)) == length(unique(pipeline$pc1$contig)))) {
      return(list(samples = pipeline$samples, correct_ys = pipeline$correct_ys, pc1 = pipeline$pc1, pc1_range = pipeline$pc1_range, samples_y = pipeline$samples_y))
    } else {
      Z <- subset(Z, sample %in% pipeline$samples & contig %in% pipeline$pc1$contig)
    }
  }
}
