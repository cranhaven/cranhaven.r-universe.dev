#' Get the ratio of allele frequencies with a region
#' @param subdf Dataframe with calculated statistics
#' @param lower Lower bound for allele frequency region
#' @param upper Upper bound for allele frequency region
#' @return Ratio of allele frequencies with a region
getRatio <- function(subdf, lower, upper) {
  ratio <- nrow(subdf[((subdf$AF>lower) & (subdf$AF<upper)),])/nrow(subdf)
  return (ratio)
}

#' Get absolute value of skewness
#' @param subdf Input dataframe
#' @import e1071
#' @return Absolute value of skewness
getSkewness <- function(subdf) {
  value <- abs(skewness(subdf$AF))
  return (value)
}

#' Remove CNV regions within VCF files by changepoint method
#' @param vcf Input VCF files
#' @param threshold Threshold for allele frequency
#' @param skew Skewness for allele frequency
#' @param lower Lower bound for allele frequency region
#' @param upper Upper bound for allele frequency region
#' @importFrom changepoint cpt.var
#' @return VCF object without changepoint region
rmChangePoint <- function(vcf, threshold, skew, lower, upper) {
  vcf_Het <- vcf[vcf$ZG == "het",]
  vcf_Hom <- vcf[vcf$ZG == "hom",]
  res <- cpt.var(data = vcf_Het$AF,
                 penalty = "CROPS",
                 pen.value = c(100, 101),
                 method = "PELT",
                 class = FALSE, # Logical. If TRUE then an object of class cpt is returned.
                 param.estimates = TRUE) # Logical. (1) If TRUE and class=TRUE then parameter estimates are returned.
  #          (2) If FALSE or class=FALSE no parameter estimates are returned.
  # Here Class=F & param.estimates=T, an object of class list (not cpt) is returned.
  if (!is.na(res$changepoints[[length(res$changepoints)]][1])) { # This only looks at the last value.
    # For most cases, if change points exist, NA value will not show up.
    # In very few situations, possible change point output can be:
    # 1,2,3,4,5
    # 1,3,4,5,NA
    # 1,3,5,NA,NA
    # NA values are all on the right. Here we choose the first one.
    res$changepoints <- c(1, res$changepoints[[length(res$changepoints)]], nrow(vcf_Het))
    f <- NULL
    for (i in 1:(length(res$changepoints)-1)) {
      tmp <- vcf_Het[(res$changepoints[i]:res$changepoints[i+1]),]
      if ((getRatio(subdf = tmp, lower, upper) > threshold) | (getSkewness(subdf = tmp) > skew)) {
        f <- rbind(f, tmp) # The region which is NOT considered as CNV
      }
    }
    vcf <- rbind(f, vcf_Hom)
    return (vcf)
  }
  return (vcf)
}

#' Remove CNV regions within VCF files given cnv file
#' @param vcf Input VCF files
#' @param cnvobj cnv object
#' @return VCF object without changepoint region
rmCNVinVCF <- function(vcf, cnvobj) {
  if (nrow(cnvobj) > 0) {
    for (i in 1:nrow(cnvobj)) {
      index <- which((vcf$Chr == cnvobj$Chr[i])&((vcf$Pos > cnvobj$Start[i])&(vcf$Pos < cnvobj$Stop[i])))
      if (length(index) > 0) {
        vcf <- vcf[-index,]
      }
    }
  }
  return (vcf)
}

#' Remove CNV regions within VCF files
#' @param rmCNV Remove CNV regions, default is FALSE
#' @param vcf Input VCF files
#' @param cnvobj cnv object, default is NULL
#' @param threshold Threshold for allele frequency, default is 0.1
#' @param skew Skewness for allele frequency, default is 0.5
#' @param lower Lower bound for allele frequency region, default is 0.45
#' @param upper Upper bound for allele frequency region, default is 0.55
#' @return VCF file without CNV region
#' @export
update_vcf <- function(rmCNV = FALSE, vcf, cnvobj = NULL, threshold = 0.1, skew = 0.5, lower = 0.45, upper = 0.55) {
  if (rmCNV) {
    if (is.null(cnvobj)) {
      vcf <- rmChangePoint(vcf, threshold, skew, lower, upper)
    } else {
      vcf <- rmCNVinVCF(vcf, cnvobj)
    }
  }
  return (vcf)
}
