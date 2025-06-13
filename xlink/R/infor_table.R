#' Information table.
#' 
#' \code{infor_table} returns information table of estimated coefficients/hazard ratio, confidence interval and P value.
#' 
#' @param x An output from continuous/binary/survival model.
#' @param snp Single SNP name.
#' @param covar_n Covariate names.
#' @param MAF_value A minimum value of minor allele frequency.
#' @param model Model type.
#' @return Information table. If linear or binary model is chosen, it returns estimated coefficients, 
#' confidence interval and P value. If survival model is chosen, it returns hazard ratio, confidence interval and P value.
infor_table <- function(x, snp, covar_n, MAF_value, model) {
    
    if (model == "survival") {
        covar_n[1] <- snp
        CI_up <- round(exp(x[, 1] + 1.96 * x[, 3]), 4)
        CI_low <- round(exp(x[, 1] - 1.96 * x[, 3]), 4)
        hz <- round(x[, 2], 4)
        pv <- x[, 5]
        MAF_c <- c(MAF_value, rep("NA", length(covar_n) - 1))
        CI <- paste("[", CI_low, ",", CI_up, "]", sep = "")
        Table <- cbind(hz, as.data.frame(CI), pv, MAF_c)
        rownames(Table) <- covar_n
        colnames(Table) <- c("Hazard Ratio", "Confidence Interval (95%)", "P Value", "MAF")
        return(Table)
    }
    
    if (model %in% c("binary", "linear")) {
        covar_n[2] <- snp
        CI_up <- round((x[, 1] + 1.96 * x[, 2]), 4)
        CI_low <- round((x[, 1] - 1.96 * x[, 2]), 4)
        es <- round(x[, 1], 4)
        pv <- x[, 4]
        MAF_c <- c("NA", MAF_value, rep("NA", length(covar_n) - 2))
        CI <- paste("[", CI_low, ",", CI_up, "]", sep = "")
        Table <- cbind(es, as.data.frame(CI), pv, MAF_c)
        rownames(Table) <- covar_n
        colnames(Table) <- c("Estimate", "Confidence Interval (95%)", "P Value", "MAF")
        return(Table)
    }
    
}
