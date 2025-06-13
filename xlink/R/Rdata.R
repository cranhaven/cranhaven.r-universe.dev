#' Simulation data for Genetic association models for X-chromosome SNPS
#'
#' A simulated dataset containing 400 observations. The variables list as follows:
#'
#' \itemize{
#'   \item ID. Identification number.
#'   \item OS. Survival indicator, 1 for death, 0 for censoring.
#'   \item OS_time. Duration time of survival.
#'   \item gender. Binary value 0,1 with P(x=1)=0.5 and hazard ratio is 1.
#'   \item Age. Uniform distribution in [20,80] and hazard ratio is 1.02.
#'   \item Smoking. Binary value 0,1 with P(x=1)=0.3 and hazard ratio is 1.2.
#'   \item Treatment. Binary value 0,1 with P(X=1)=0.3 and hazard ratio is 1.2.
#'   \item snp_1. True type in coxph model is 'XCI', minor allele frequency is 0.2, hazard ratio is 1.5.
#'   \item snp_2. True type in coxph model is 'XCI-E', minor allele frequency is 0.3, hazard ratio is 1.5.
#'   \item snp_3. True type in coxph model is 'XCI-S', minor allele frequency is 0.4, hazard ratio is 1.5.
#'   \item snp_4. True type in coxph model is 'XCI', minor allele frequency is 0.3, hazard ratio is 1.
#'   \item snp_5. True type in coxph model is 'XCI-E', minor allele frequency is 0.1, hazard ratio is 1.
#'   \item snp_6. True type in coxph model is 'XCI', minor allele frequency is 0.2, hazard ratio is 1.
#'   \item snp_7. True type in coxph model is 'XCI', minor allele frequency is 0.15, hazard ratio is 1.
#'   \item snp_8. True type in coxph model is 'XCI-E', minor allele frequency is 0.1, hazard ratio is 1.
#'   \item snp_9. True type in coxph model is 'XCI', minor allele frequency is 0.2, hazard ratio is 1.
#'   \item snp_10. True type in coxph model is 'XCI', minor allele frequency is 0.15, hazard ratio is 1.
#' }
#'
#' @docType data
#' @keywords datasets
#' @name Rdata
#' @usage data(Rdata)
#' @format A data frame with 400 rows and 17 variables.
NULL
