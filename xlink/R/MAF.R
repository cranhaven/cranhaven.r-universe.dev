#' Select SNP by MAF.
#'
#' \code{MAF} returns SNPs with higher MAF than default value.
#'
#' @param snp SNP name.
#' @param gender gender variable.
#' @param male male information.
#' @param MAF_v minimum MAF value.
#' @param data a dataset.
MAF <- function(snp, gender, male, MAF_v, data) {
    data <- stats::na.omit(data[, which(colnames(data) %in% c(snp, gender))])
    snp_v <- data[,which(colnames(data) %in% snp)]
    n <- length(snp_v)
    ind <- (data[,which(colnames(data) %in% gender)] == male)
    MAF_R <- (2 * sum(snp_v[ind] != 0) + 2 * sum(snp_v[!ind] == 2) + 1 * sum(snp_v[!ind] == 1)) /(2 * n)
    MAF_R <- round(MAF_R,4)
    if (MAF_R >= MAF_v) {
        return(c(snp, MAF_R))
    } else {
        return(NULL)
    }
}
