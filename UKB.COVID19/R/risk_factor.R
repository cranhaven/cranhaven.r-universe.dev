#' Generate covariate file
#'
#' This function formats and outputs a covariate table, used for input for other functions.
#' @param ukb.data tab delimited UK Biobank phenotype file. The file should include fields of gender, year of birth, BMI, ethnic background, SES, and smoking.
#' @param ABO.data Latest yyyymmdd_covid19_misc.txt file.
#' @param hesin.file Latest yyyymmdd_hesin.txt file.
#' @param res.eng Latest covid result file/files for England.
#' @param res.wal Latest covid result file/files for Wales. Only available for downloads after April 2021.
#' @param res.sco Latest covid result file/files for Scotland. Only available for downloads after April 2021.
#' @param fields User specified field codes from ukb.data file.
#' @param field.names User specified field names.
#' @export risk_factor
#' @return Outputs a covariate table, used for input for other functions. Automatically returns sex, age at birthday in 2020, SES, self-reported ethnicity, most recently reported BMI, most recently reported pack-years, whether they reside in aged care (based on hospital admissions data, and covid test data) and blood type. Function also allows user to specify fields of interest (field codes, provided by UK Biobank), and allows the users to specify more intuitive names, for selected fields.
#' @import data.table
#' @importFrom magrittr %>%
#' @import tidyverse
#' @import utils
#' @examples
#' \dontrun{
#' covars <- risk_factor(ukb.data=covid_example("sim_ukb.tab.gz"),
#' ABO.data=covid_example("sim_covid19_misc.txt.gz"),
#' hesin.file=covid_example("sim_hesin.txt.gz"),
#' res.eng=covid_example("sim_result_england.txt.gz"))
#' }
#'

risk_factor <- function(ukb.data, ABO.data = NULL, hesin.file, res.eng, res.wal = NULL, res.sco = NULL, fields = NULL, field.names = NULL){
  
  
  colnames <- fread(ukb.data, nrows=0)
  
  idx <- c(names(colnames) %in% c("f.eid", "f.31.0.0", "f.34.0.0", "f.189.0.0") %>% which, #, 'f.22000.0.0'
           names(colnames) %like% "f.21001." %>% which,
           names(colnames) %like% "f.21000." %>% which,
           names(colnames) %like% "f.20161." %>% which)
  
  if(!("f.31.0.0" %in% names(colnames))) stop("no sex information (f.31.0.0) in the ukb.tab file.")
  if(!("f.34.0.0" %in% names(colnames))) stop("no year of birth information (f.34.0.0) in the ukb.tab file.")
  if(!("f.189.0.0" %in% names(colnames))) stop("no SES information (f.189.0.0) in the ukb.tab file.")
  if(!(any(names(colnames) %like% "f.21001."))) stop("no BMI information (f.21001.) in the ukb.tab file.")
  if(!(any(names(colnames) %like% "f.21000."))) stop("no self-reported ethnicity information (f.21000.) in the ukb.tab file.")
  if(!(any(names(colnames) %like% "f.20161."))) stop("no reported pack-years information (f.20161.) in the ukb.tab file.")
  
  if(!is.null(fields)) {
    
    idx <- names(colnames) %in% fields %>% which %>%
      c(idx, .)
    
  }
  
  db <- fread(ukb.data, header = T, select = idx, data.table = F, quote="")
  
  # sex: 1- male, 0- female
  phe <- db[,c("f.eid","f.31.0.0")]
  colnames(phe) <- c("ID","sex")
  
  # age: 2020 - year of birth
  phe$age <- 2020-db$f.34.0.0
  
  # Body mass index (BMI): the lastest
  bmi <- db[,startsWith(colnames(db),"f.21001.")]
  for(i in 2:ncol(bmi)){
    bmi[!(is.na(bmi[,i])),1] <- bmi[!(is.na(bmi[,i])),i]
  }
  phe$bmi <- bmi[,1]

  # Ethnic background
  ethnic <- db[,startsWith(colnames(db),"f.21000.")]
  for(i in 2:ncol(ethnic)){
    ethnic[!(is.na(ethnic[,i])),1] <- ethnic[!(is.na(ethnic[,i])),i]
  }
  if(any(ethnic[,1] <0 & !(is.na(ethnic[,1])))) ethnic[ethnic[,1] <0 & !(is.na(ethnic[,1])),1] <- NA
  phe$ethnic <- ethnic[,1]
  
  phe$white <-  phe$mixed <- phe$asian <- phe$black <- phe$other.ppl <- 0
  phe[phe$ethnic %in% c(1001:1003,1) & !(is.na(phe$ethnic)),"white"] <- 1
  phe[phe$ethnic %in% c(2001:2004,2) & !(is.na(phe$ethnic)),"mixed"] <- 1
  phe[phe$ethnic %in% c(3001:3004,3,5) & !(is.na(phe$ethnic)),"asian"] <- 1
  phe[phe$ethnic %in% c(4001:4003,4) & !(is.na(phe$ethnic)),"black"] <- 1
  phe[phe$ethnic == 6 & !(is.na(phe$ethnic)),"other.ppl"] <- 1
  phe[phe$ethnic %in% c(-1,-3),c("white","black","asian","mixed","other.ppl")] <- NA
  
  ### SES
  phe$SES <- db[,startsWith(colnames(db),"f.189.")]
  
  ### smoking
  smoke <- db[,startsWith(colnames(db),"f.20161.")]
  for(i in 2:ncol(smoke)){
    smoke[!(is.na(smoke[,i])),1] <- smoke[!(is.na(smoke[,i])),i]
  }
  smoke[is.na(smoke[,1]),1] <- 0
  phe$smoke <- smoke[,1]
  
  ### ABO blood type
  if(!(is.null(ABO.data))){
    
    ABO <- read.table(ABO.data,header = T, sep = "\t",stringsAsFactors = F)
    phe <- merge(phe,ABO,by.x = "ID", by.y = "eid",all = T)
    phe$A <- phe$B <- phe$AB <- phe$O <- 0
    phe[phe$blood_group %in% c("AA","AO") & !(is.na(phe$blood_group)),"A"] <- 1
    phe[phe$blood_group %in% c("BB","BO") & !(is.na(phe$blood_group)),"B"] <- 1
    phe[phe$blood_group == "AB" & !(is.na(phe$blood_group)),"AB"] <- 1
    phe[phe$blood_group == "OO" & !(is.na(phe$blood_group)),"O"] <- 1
    phe[is.na(phe$blood_group),c("A","B","AB","O")] <- NA
    
  }
  
  hesIn <- data.table::fread(hesin.file)
  
  # Evidence of aged care from HES data
  agedCare <- hesIn[, sourceAged := admisorc_uni %in% c(7000, 7001, 7002, 7003)] %>%
    .[, dischargeAged := disdest_uni %in% c(7000, 7001, 7002, 7003)]
  
  agedCareIds <- agedCare[sourceAged==T | dischargeAged==T , eid] %>% unique
  noAgedCareIds <- agedCare[sourceAged==F & dischargeAged==F  & !(eid %in% agedCareIds), eid] %>% unique
  
  # Also bring in evidence of being in aged care from covid tests data.
  
  if(!is.null(res.eng)) {
    
    agedCareIdsEng <- fread(res.eng, select=c("eid", "reqorg")) %>%
      .[reqorg == 9, eid] %>%
      unique
    
  }
  
  if(!is.null(res.wal)) {
    
    agedCareIdsWal <- fread(res.wal, select=c("eid", "perstype")) %>%
      .[perstype %in% c(1, 10,  25, 30, 36, 39, 79), eid] %>%
      unique
    
  }
  if(!is.null(res.sco)) {
    
    agedCareIdsSco <- fread(res.sco, select=c("eid", "factype")) %>%
      .[factype == 7, eid] %>%
      unique
    
  }
  
  agedCareIdsAlt <- c(get0("agedCareIdsEng"), get0("agedCareIdsWal"), get0("agedCareIdsSco")) %>%
    unique
  
  
  phe[, "inAgedCare"] <- NA
  phe[(phe$ID %in% agedCareIds) | (phe$ID %in% agedCareIdsAlt), "inAgedCare"] <- 1
  phe[phe$ID %in% noAgedCareIds, "inAgedCare"] <- 0
  
  
  # user specified custom fields
  if(!is.null(fields)) {
    
    custom <- db[,c("f.eid", fields)]
    
    if(is.null(field.names)) {
      colnames(custom) <- c("eid", field.names)
    }
    
    phe <- merge(phe, custom, by.x = "ID", by.y = "eid",all = T)
    
  }
  
  # reform variables
  cov <- data_reform(phe, type = "cov")
  attr(cov, "class") <- "data.frame"
  cov
}
