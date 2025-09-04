#' Generate comorbidity association result file
#'
#' Association tests between each co-morbidity and given phenotype (susceptibility, mortality or severity) with the adjustment of covariates.
#' @param pheno phenotype dataframe - output from makePheno function
#' @param covariates covariate dataframe - output from risk.factor function.  Optional.
#' @param cormorbidity Comorbidity summary generated from comorbidity.summary.
#' @param population Choose self-report population/ethnic background group from "all", white", "black", "asian", "mixed", or "other". By default, population="all", include all ethnic groups.
#' @param cov.name Selected covariates names. By default, cov.name=c("sex","age","bmi"), covariates are sex age and BMI.
#' @param phe.name Phenotype name.
#' @param ICD10.file The ICD10 code file, which is included in the package.
#' @return Outputs a comorbidity association test result with OR, 95% CI and p-value.
#' @export comorbidity_asso
#' @import questionr
#' @import utils
#' @examples
#' \dontrun{
#' comorb.asso <- comorbidity_asso(pheno=phe,
#' covariates=covar,
#' cormorbidity=comorb,
#' population="white",
#' cov.name=c("sex","age","bmi","SES","smoke","inAgedCare"),
#' phe.name="hospitalisation",
#' ICD10.file=covid_example("ICD10.coding19.txt.gz"))
#' }
#'

comorbidity_asso <- function(pheno, covariates, cormorbidity, population = "all", cov.name=c("sex","age","bmi"), phe.name, ICD10.file){
  res <- inner_join(pheno, covariates, by="ID")
  code <- as.character(read.table(ICD10.file, sep = "\t")[,2])
  #if(any(c(population,cov.name) %in% (colnames(res)))) res <- res[,!(colnames(res) %in% c(population,cov.name))]
  #load(file = cormorbidity.file)
  comorb.name <- colnames(cormorbidity)[-1]
  res.cov <- merge(res, cormorbidity, by.x = "ID", by.y="ID", all.x = T, sort = F)
  if(population == "all") res.cov <- res.cov
  if(population == "white") res.cov <- res.cov[res.cov$white == 1 & !(is.na(res.cov$white)),]
  if(population == "black") res.cov <- res.cov[res.cov$black == 1 & !(is.na(res.cov$black)),]
  if(population == "asian") res.cov <- res.cov[res.cov$asian == 1 & !(is.na(res.cov$asian)),]
  if(population == "mixed") res.cov <- res.cov[res.cov$mixed == 1 & !(is.na(res.cov$mixed)),]
  if(population == "other") res.cov <- res.cov[res.cov$other.ppl == 1 & !(is.na(res.cov$other.ppl)),]
  n.covars <- length(cov.name)
  n.comorb <- length(comorb.name)
  comorb.asso <- as.data.frame(matrix(NA, ncol=6, nrow = n.comorb))
  colnames(comorb.asso) <- c("ICD10","Estimate","OR","2.5%","97.5%","p")
  rownames(comorb.asso) <- comorb.name
  for(j in 1:n.comorb){
    covars <- c(cov.name, comorb.name[j])
    comorb.asso[j,1] <- code[startsWith(code, comorb.name[j])]
    # length(table(res.cov[,comorb.name[j]]))==1
    if(any(table(res.cov[,comorb.name[j]])==0) | sum(res.cov[,comorb.name[j]] == 1) <5){
      comorb.asso[j,-1] <- NA
    }else{
      comorb.asso[j,-1] <- log_cov(pheno=res.cov[,c("ID",phe.name)], 
                                   covariates=res.cov[,!(colnames(res.cov) %in% phe.name)], 
                                   phe.name, cov.name=covars)[n.covars+2,]
    }
  }

  attr(comorb.asso, "class") <- "data.frame"
  comorb.asso
}

