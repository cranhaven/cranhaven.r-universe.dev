#' Construct disease PRS using LDpred2
#'
#' Using snp_ldpred2_grid function from bigsnpr function
#' @param DIS_GWAS a numeric matrix containing disease GWAS summary statistics, including SNP ID, position, \eqn{\beta}, SE(\eqn{\beta}), p-value, N, and MAF
#' @param G_reference a numeric matrix containing the individual-level genotype information from the reference panel (e.g., 1KG)
#' @param h2 a numeric value indicating the estimated heritability
#' @param pcausal a numeric value indicating the hyper-parameter as the proportion of causal variants
#' @details PRS-Dis-LDpred2 automatically sets predictive effect sizes equivalent to the prognostic effect sizes; and only need disease GWAS summary statistics and external reference genotype
#' @return A numeric list, the first sublist contains estimated prognostic effect sizes, the second sublist contains estimated predictive effect sizes
#' @references Prive, F., Arbel, J. & Vilhjalmsson, B.J. LDpred2: better, faster, stronger. Bioinformatics 36, 5424-5431 (2020).
#' @references Zhai, S., Zhang, H., Mehrotra, D.V. & Shen, J. Paradigm Shift from Disease PRS to PGx PRS for Drug Response Prediction using PRS-PGx Methods (submitted).
#' @author Song Zhai
#' @export
#' @examples
#' \donttest{
#' data(PRSPGx.example); attach(PRSPGx.example)
#' coef_est <- PRS_Dis_LDpred2(DIS_GWAS, G_reference, pcausal = 0.1, h2 = 0.4)
#' summary(coef_est$coef.G)
#' summary(coef_est$coef.TG)
#' }
#'
PRS_Dis_LDpred2 <- function(DIS_GWAS, G_reference, pcausal, h2){
  add.info <- DIS_GWAS[,c("beta_hat","beta_se","N")]
  colnames(add.info) <- c("beta","beta_se","n_eff")
  rownames(add.info) <- colnames(G_reference)

  corr0 <- as(cor(G_reference), "dgCMatrix")#; tmp <- tempfile(tmpdir = tmpdir)
  corr <- as_SFBM(corr0)

  params <- expand.grid(p = c(pcausal, 0.1), h2 = h2, sparse = TRUE)
  NCORES <- nb_cores()

  beta_grid <- snp_ldpred2_grid(corr, add.info, params, ncores = NCORES)
  coef.G <- beta_grid[,1]; names(coef.G) <- colnames(G_reference)

  re <- list(coef.G = coef.G, coef.TG = coef.G)
  return(re)
}
