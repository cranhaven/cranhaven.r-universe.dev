#' Create comorbidity summary file
#' 
#' summarise disease history records of each individual from the hospital inpatient diagnosis data.
#' @param ukb.data tab delimited UK Biobank phenotype file, containing sample qc fields (with default UKBiobank codes as column names)
#' @param hesin.file Latest hospital inpatient master file.
#' @param hesin_diag.file Latest hospital inpatient diagnosis file.
#' @param primary TRUE: include primary diagnosis only; FALSE: include all diagnoses.
#' @param ICD10.file The ICD10 code file, which is included in the package.
#' @param Date.start Date, dd/mm/yyyy, select the start date of hospital inpatient record period. 
#' @param Date.end Date, dd/mm/yyyy, select the end date of hospital inpatient record period.
#' @return Outputs comorbidity summary table, named comorbidity_<Date.start>_<Date.end>.RData, including phenotype, non-genetic risk factors and all comorbidities, which will be used in the comorbidity association tests.
#' @import data.table
#' @import utils
#' @importFrom magrittr %>%
#' @export comorbidity_summary
#' @examples 
#' \dontrun{
#' comorb <- comorbidity_summary(ukb.data=covid_example("sim_ukb.tab.gz"),
#' hesin.file=covid_example("sim_hesin.txt.gz"), 
#' hesin_diag.file=covid_example("sim_hesin_diag.txt.gz"), 
#' ICD10.file=covid_example("ICD10.coding19.txt.gz"),
#' primary = FALSE,
#' Date.start = "16/03/2020")
#' }
#' 

### comorbidity
comorbidity_summary <- function(ukb.data, hesin.file, hesin_diag.file, primary=FALSE, ICD10.file, Date.start=NULL, Date.end=NULL){
  cov <- fread(ukb.data, header = T, select = "f.eid", data.table = F, quote="", col.names = "ID")
  hesin_diag <- fread(hesin_diag.file) %>% as.data.frame
  hesin <- fread(hesin.file) %>% as.data.frame
  # select hesin data in a certain period by given start and end dates
  hesin$epistart <- as.Date(hesin$epistart, format="%d/%m/%Y")
  if(is.null(Date.start)) {
    Date.start <- min(hesin$epistart,na.rm = T)
  }else{
    Date.start <- as.Date(Date.start, format="%d/%m/%Y")
  }
  if(is.null(Date.end)) {
    Date.end <- max(hesin$epistart,na.rm = T)
  }else{
    Date.end <- as.Date(Date.end, format="%d/%m/%Y")
  }
  
  hesin.select <-subset(hesin, epistart >= Date.start & epistart <= Date.end)
  hesin.select$eid.index <- paste0(hesin.select$eid,"_",hesin.select$ins_index)
  hesin_diag$eid.index <- paste0(hesin_diag$eid,"_",hesin_diag$ins_index)
  hesin.select.diag <- hesin_diag[hesin_diag$eid.index %in% hesin.select$eid.index,]
  hesin.select.diag.l1 <- hesin.select.diag[hesin.select.diag$level==1,]
  
  code<-as.data.frame(fread(ICD10.file))
  
  for(i in 1:nrow(code)){
    name1 <- strsplit(code[i,1]," ")[[1]][2]
    name2 <- strsplit(name1,"[-]")[[1]]
    class <- substr(name2[1],1,1)
    if(class %in% c("O","P","R","S","T","U","V","W","X","Y","Z")) next
    sub.class <- seq(sub(".","",name2[1]),sub(".","",name2[2]))
    if(any(sub.class<10)) sub.class[sub.class<10] <- paste0("0",sub.class[sub.class<10])
    class.names <- paste0(class,sub.class)
    cov[,name1] <- 0
    if(primary){
      for(j in 1:length(class.names)){
        hes.p.id <- hesin.select.diag.l1[which(startsWith(hesin.select.diag.l1$diag_icd10,class.names[j])),"eid"] %>% unique
        cov[cov$ID %in% hes.p.id,name1] <- 1
      } 
    }else{
      for(j in 1:length(class.names)){
        hes.a.id <- hesin.select.diag[which(startsWith(hesin.select.diag$diag_icd10,class.names[j])),"eid"] %>% unique
        cov[cov$ID %in% hes.a.id,name1] <- 1
      } 
    }  
  }
 
  cov <- data_reform(cov, type = "comorbidity")
  attr(cov, "class") <- "data.frame"
  cov
}

