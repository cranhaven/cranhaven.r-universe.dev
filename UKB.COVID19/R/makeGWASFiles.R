
#' Generate files for GWAS Software. SAIGE and Plink currently supported.
#'
#' @param ukb.data tab delimited UK Biobank phenotype file, containing sample qc fields (with default UKBiobank codes as column names)
#' @param pheno phenotype dataframe - output from makePhenotype function
#' @param covariates covariate dataframe - output from risk.factor function.  Optional.
#' @param phe.name phenotypes to be included in outputted data. multiple phenotypes can be specified as a vector. if null, all phenotypes will be outputted.
#' @param cov.name covariates to be included in outputted data. Optional. multiple covariates can be specified as a vector. if null, all covariates in file will be outputted
#' @param includeSampsFile list of samples to be included GWAS. File with the first column containing sample IDs to be kept. Can contain other columns. output from sampleQC function may be used.  Optional - if null, all samples will be outputted.
#' @param software specify "SAIGE" or "plink" - defaults to "SAIGE"
#' @param outDir specify directory to output file
#' @param prefix prefix for file - optional
#'
#' @return outputs file, suitable for reading by chosen GWAS software
#' @export makeGWASFiles
#' @import data.table
#' @importFrom magrittr %>%
#' @import tidyverse
#' @import here
#' @import utils
#'
#' @examples
#' \dontrun{
#' makeGWASFiles(ukb.data=covid_example("sim_ukb.tab.gz"), 
#' pheno=phe, 
#' covariates=covar, 
#' phe.name="hospitalisation", 
#' cov.name=NULL, 
#' includeSampsFile=NULL, 
#' software="SAIGE", 
#' outDir=covid_example("results"), 
#' prefix="hospitalisation")
#' }
#' 

makeGWASFiles <- function(ukb.data, pheno, covariates, phe.name, cov.name=NULL, includeSampsFile=NULL, software="SAIGE", outDir="", prefix) {
  
  if(is.null(pheno)) {
    stop("Please specify pheno dataframe")
  }
  
  if(is.null(phe.name)) {
    print("No phenotypes specified - all included in pheno dataframe will be included in outfile")
  }
  
  if(!is.null(cov.name) & is.null(covariates)) {
    stop("Please specify covariate dataframe")
  }
  
  if(is.null(cov.name) & !is.null(covariates)) {
    print("No covriates specified - all included in covariate dataframe will be included in outfile")
  }
  
  if(!(software %in% c("plink", "SAIGE"))) {
    stop("Please specify GWAS software: \"plink\" or \"SAIGE\"")
  }
  
  ## Read in PCs and batch, to determine array used
  cols <-c("f.eid","f.22000.0.0", "f.22009.0.1", "f.22009.0.2", "f.22009.0.3",
           "f.22009.0.4", "f.22009.0.5", "f.22009.0.6", "f.22009.0.7", "f.22009.0.8",
           "f.22009.0.9", "f.22009.0.10", "f.22009.0.11", "f.22009.0.12",
           "f.22009.0.13", "f.22009.0.14", "f.22009.0.15", "f.22009.0.16",
           "f.22009.0.17", "f.22009.0.18", "f.22009.0.19", "f.22009.0.20")
  names <- c("ID", "batch", "PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7", "PC8", "PC9",
             "PC10", "PC11", "PC12", "PC13", "PC14", "PC15", "PC16", "PC17",
             "PC18", "PC19", "PC20")
  
  geno <- as.data.frame(fread(ukb.data, header = T, select = cols, quote="", col.names = names))
  geno$array <-NA
  geno[geno$batch < 0 & !(is.na(geno$batch)),"array"] <-"UKBL"
  geno[geno$batch >=0 & !(is.na(geno$batch)), "array"] <- "UKBB"
  geno <- geno[, which(colnames(geno) != "batch")]
  geno$ID <- as.character(geno$ID)
  
  if(!is.null(phe.name)) {
    cols <- c("ID", phe.name)
    pheno <- pheno[,colnames(pheno) %in% cols]
  }
  
  ## add covariates to pheno data
  if(!is.null(covariates)) {
    
    if(!is.null(cov.name) ) {
      cols <- c("ID", cov.name)
      covariates <- covariates[, colnames(covariates) %in% cols]
    }
    
    pheno <- merge(pheno, covariates, by = "ID")
  }
  
  ## bring all together..
  outfile <- merge(pheno, geno, by = "ID")
  
  # restrict ids, if applicable
  if(!is.null(includeSampsFile)) {
    includeIDs <- as.data.frame(fread(includeSampsFile, select=1, col.names="ID"))[,"ID"]
    outfile <-  outfile[outfile$ID %in% includeIDs,]
  }
  
  
  if(software=="SAIGE") {
    
    if(is.null(prefix)) {
      prefix <- "phenotypes_SAIGE"
    }
    
    filename <-  here(outDir, paste0(prefix,".txt"))
    print(paste("outputting phenotype file:", filename))
    
    write.table(outfile, file=filename, row.names=F, col.names=T, quote=F)
    
  }
  
  if(software=="plink") {
    
    if(is.null(prefix)) {
      prefix <- "phenotypes_plink"
    }
    
    mycols <- c("FID", names(outfile))
    outfile <- outfile[, colnames(outfile) %in% mycols]
    setnames(outfile, c(1,2), c("FID", "IID"))
    
    filename <-  here(outDir, paste0(prefix,".txt"))
    print(paste("outputting phenotype file:", filename))
    
    write.table(outfile, file=filename, row.names=F, col.names=T, quote=F)
    
  }
  
}
