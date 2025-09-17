#' Second alternative allele percentage
#' @param f Input raw file
#' @return Percent of the second alternative allele
#' @export
getAlt2 <- function(f) {
  tmp <- f[grep(",", f$Alt), ]
  percentage <- nrow(tmp)/nrow(f)
  return(percentage)
}

#' Annotation rate
#' @param f Input raw file
#' @return Percentage of annotation locus
getAnnoRate <- function(f) {
  tmp <- f[ f$dbID !=".", ]
  percentage <- nrow(tmp)/nrow(f)
  return(percentage)
}

#' Low depth percentage
#' @param f Input raw file
#' @param ldpthred Threshold to determine low depth, default is 20
#' @return Percentage of low depth
getLowDepth <- function(f, ldpthred) {
  tmp <- f[which(f$DP < ldpthred), ]
  percentage <- nrow(tmp)/nrow(f)
  return(percentage)
}

#' SNV percentage
#' @param df Input raw file
#' @return Percentage of SNV
getSNVRate <- function(df) {
  tmp_df <- df[nchar(df$Ref)==1 & nchar(df$Alt)==1, ]
  return (nrow(tmp_df)/nrow(df))
}

#' Calculate zygosity variable
#' @param df Input modified file
#' @param state Zygosity state
#' @param hom_mle MLE in hom model
#' @param het_mle MLE in het model
#' @return Zygosity variable
getVar <- function(df, state, hom_mle, het_mle) {
  df_sub <- df[df$ZG == state,]
  if (state == "hom") {
    expected <- hom_mle
  } else if (state == "het") {
    expected <- het_mle
  }
  df_sub <- df_sub[(abs(df_sub$AF - expected) < 0.49),]
  tmp <- sum(((df_sub$AF - expected)^2) * df_sub$DP) / sum(df_sub$DP)
  return(tmp)
}

#' Calculate average log-likelihood
#' @param df Input modified file
#' @param hom_mle Hom MLE of p in Beta-Binomial model, default is 0.9981416 from NA12878_1_L5
#' @param het_mle Het MLE of p in Beta-Binomial model, default is 0.4737897 from NA12878_1_L5
#' @param hom_rho Hom MLE of rho in Beta-Binomial model, default is 0.04570275 from NA12878_1_L5
#' @param het_rho Het MLE of rho in Beta-Binomial model, default is 0.02224098 from NA12878_1_L5
#' @importFrom VGAM dbetabinom
#' @return meanLL
getAvgLL <- function(df, hom_mle, het_mle, hom_rho, het_rho) {
  df_hom <- df[df$ZG == "hom",]
  df_het <- df[df$ZG == "het",]
  df_hom$LL <- dbetabinom(x = df_hom$AC,
                          size = df_hom$DP,
                          prob = hom_mle,
                          rho = hom_rho,
                          log = TRUE)
  df_het$LL <- dbetabinom(x = df_het$AC,
                          size = df_het$DP,
                          prob = het_mle,
                          rho = het_rho,
                          log = TRUE)
  df <- rbind(df_hom, df_het)
  meanLL <- mean(df$LL)
  return (meanLL)
}

#' @title Feature Generation for Contamination Detection Model
#' @description Generates features from each pair of input VCF objects for training contamination detection model.
#'
#' @param file VCF input object
#' @param hom_p The initial value for p in Homozygous Beta-Binomial model, default is 0.999
#' @param het_p The initial value for p in Heterozygous Beta-Binomial model, default is 0.5
#' @param hom_rho The initial value for rho in Homozygous Beta-Binomial model, default is 0.005
#' @param het_rho The initial value for rho in Heterozygous Beta-Binomial model, default is 0.1
#' @param mixture A vector of whether the sample is contaminated: 0 for pure; 1 for contaminated
#' @param homcut Cutoff allele frequency value between hom and high, default is 0.99
#' @param highcut Cutoff allele frequency value between high and het, default is 0.7
#' @param hetcut Cutoff allele frequency value between het and low, default is 0.3
#'
#' @return A data frame with all features for training model of contamination detection
#' @export
generate_feature <- function(file, hom_p = 0.999, het_p = 0.5, hom_rho = 0.005, het_rho = 0.1,
                             mixture, homcut = 0.99, highcut = 0.7, hetcut = 0.3) {
  Name <- file$file_sample_name[1]
  if ((mixture != 0) & (mixture != 1)) {
    stop("Use 0 and 1 for mixture!")
  }
  Mixture <- mixture
  vcf <- file$VCF

  LOH <- nrow(vcf[vcf$ZG == "het",])/nrow(vcf[vcf$ZG == "hom",])
  HomVar <- getVar(df = vcf, state = "hom", hom_mle = hom_p, het_mle = het_p)
  HetVar <- getVar(df = vcf, state = "het", hom_mle = hom_p, het_mle = het_p)
  HomRate <- nrow(vcf[(vcf$AF > homcut),]) / nrow(vcf)
  HighRate <- nrow(vcf[((vcf$AF > highcut) & (vcf$AF < homcut)),]) / nrow(vcf)
  HetRate <- nrow(vcf[((vcf$AF > hetcut) & (vcf$AF < highcut)),]) / nrow(vcf)
  LowRate <- nrow(vcf[(vcf$AF < hetcut),]) / nrow(vcf)
  AvgLL <- getAvgLL(df = vcf, hom_mle = hom_p, het_mle = het_p, hom_rho = hom_rho, het_rho = het_rho)

  res_df <- data.frame(Name, Mixture, LOH, HomVar, HetVar, HomRate, HighRate, HetRate, LowRate, AvgLL)
  return(res_df)
}
