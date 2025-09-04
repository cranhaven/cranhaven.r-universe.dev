#' Generate COVID-19 phenotypes
#'
#' @param ukb.data tab delimited UK Biobank phenotype file.
#' @param res.eng Latest covid result file/files for England.
#' @param res.wal Latest covid result file/files for Wales. Only available for downloads after April 2021.
#' @param res.sco Latest covid result file/files for Scotland. Only available for downloads after April 2021.
#' @param death.file Latest death register file.
#' @param death.cause.file Latest death cause file.
#' @param hesin.file Latest hospital inpatient master file.
#' @param hesin_diag.file Latest hospital inpatient diagnosis file.
#' @param hesin_oper.file Latest hospital inpatient operation file.
#' @param hesin_critical.file Latest hospital inpatient critical care file.
#' @param code.file The operation code file, which is included in the package.
#' @param pheno.type The phenotype options, which include "susceptibility", "severity", and "mortality".
#' @param Date Date, ddmmyyyy, select the results until a certain date. By default, Date = NULL, the latest hospitalization date.
#' @return Returns a data.frame with phenotypes for COVID-19 susceptibility, severity and mortality.
#' @import data.table
#' @importFrom magrittr %>%
#' @import tidyverse
#' @importFrom dplyr inner_join
#' @importFrom dplyr full_join
#' @export makePhenotypes
#' @examples
#' \dontrun{
#' pheno <- makePhenotypes(ukb.data=covid_example("sim_ukb.tab.gz"),
#' res.eng=covid_example("sim_result_england.txt.gz"),
#' death.file=covid_example("sim_death.txt.gz"),
#' death.cause.file=covid_example("sim_death_cause.txt.gz"),
#' hesin.file=covid_example("sim_hesin.txt.gz"),
#' hesin_diag.file=covid_example("sim_hesin_diag.txt.gz"),
#' hesin_oper.file=covid_example("sim_hesin_oper.txt.gz"),
#' hesin_critical.file=covid_example("sim_hesin_critical.txt.gz"),
#' code.file=covid_example("coding240.txt.gz"),
#' pheno.type = "severity")
#' }
#'

makePhenotypes <- function(ukb.data, res.eng, res.wal=NULL, res.sco=NULL, death.file, death.cause.file, hesin.file, hesin_diag.file, hesin_oper.file, hesin_critical.file, code.file, pheno.type = "severity", Date=NULL){
  
  inFiles <- c(get0("res.eng"), get0("res.wal"), get0("res.sco"))
  
  
  result <- lapply(inFiles, fread, select=c("eid", "specdate", "result")) %>%
    rbindlist %>%
    as.data.frame
  
  result$specdate <- as.Date(result$specdate, format= "%d/%m/%Y")
  res.max.date <- max(result$specdate,na.rm=T)
  
  death <- fread(death.file) %>% as.data.frame
  death_cause <- fread(death.cause.file) %>% as.data.frame
  death$date_of_death <- as.Date(death$date_of_death, format= "%d/%m/%Y")
  death.max.date <- max(death$date_of_death,na.rm=T)
  
  hesin <- fread(hesin.file) %>% as.data.frame
  hesin$epiend <- as.Date(hesin$epiend, format = "%d/%m/%Y")
  hesin_diag <- fread(hesin_diag.file) %>% as.data.frame
  hesin_oper <- fread(hesin_oper.file) %>% as.data.frame
  hesin_critical <- fread(hesin_critical.file) %>% as.data.frame
  hes.max.date <- max(hesin$epiend,na.rm=T)
  
  
  if(pheno.type == "susceptibility"){
    if(is.null(Date)){
      Date <- res.max.date
    }else{
      if(Date > res.max.date){
        print("Warning: the date you input is later than the latest record in the input testing result data.")
        Date <- res.max.date
        print(paste0("The date is changed to ", Date,"."))
      }
    } 
  } 
  
  if(pheno.type == "mortality"){
    if(is.null(Date)){
      Date <- min(res.max.date, death.max.date)
    }else{
      if(Date > death.max.date){
        print("Warning: the date you input is later than the latest record in the input death register data.")
        Date <- min(res.max.date, death.max.date)
        print(paste0("The date is changed to ", Date,"."))
      }
    } 
  } 
  
  if(pheno.type == "severity"){
    if(is.null(Date)){
      Date <- min(res.max.date, death.max.date, hes.max.date)
    }else{
      if(Date > hes.max.date){
        print("Warning: the date you input is later than the latest record in the input hospital inpatient data.")
        Date <- min(res.max.date, death.max.date, hes.max.date)
        print(paste0("The date is changed to ", Date,"."))
      }
    } 
  } 
  
  result.date <- result[result$specdate <= Date & !(is.na(result$specdate)),]
  death.date <- death[death$date_of_death <= Date & !(is.na(death$date_of_death)),]
  hesin.date <- hesin[hesin$epiend <= Date & hesin$epiend >= "2020-03-01" & !(is.na(hesin$epiend)),]
  sampleID <- unique(result.date$eid)
  N <- length(sampleID)
  print(paste0(N," participants got tested until ",Date, "."))
  
  
  # summarise test result data
  pos <- result.date$eid[result.date$result == 1] %>% unique
  res <- data.frame(ID = unique(result.date$eid))
  res$result <- ifelse(res$ID %in% pos, 1, 0)
  print(paste0(sum(res$result)," participants got positive test results until ",Date, "."))
  
  
  # summarise death register data
  death.id <- unique(death.date[,"eid"])
  death.U071.id <- unique(death_cause[death_cause$cause_icd10 == "U071" & death_cause$eid %in% death.id,"eid"])
  miss.id <- death.U071.id[!(death.U071.id %in% res$ID)]
  if(length(miss.id)>0){
    print(paste0(length(miss.id)," deaths with COVID-19 but didn't get tested. Added them into phenotype data."))
    res.add <- as.data.frame(cbind(miss.id,1))
    colnames(res.add) <- colnames(res)
    res <- rbind(res,res.add)
  }
  tested.death.id <- unique(res[res$ID %in% death.id,"ID"])
  tested.death.cause <- death_cause[death_cause$eid %in% tested.death.id,]
  covid.death.id <- tested.death.cause[tested.death.cause$cause_icd10 == "U071","eid"]
  primary.covid.death.id <- tested.death.cause[tested.death.cause$cause_icd10 == "U071" & tested.death.cause$level==1,"eid"]
  res$death.U071 <- 0; res[res$ID %in% covid.death.id,"death.U071"] <- 1
  res$mortality <- 0; res[res$ID %in% primary.covid.death.id,"mortality"] <- 1
  error <- res[res$death.U071==1 & res$result == 0,"ID"]
  if(length(error)>0){
    print(paste0(length(error)," deaths with COVID-19 but got negative test results. Modified their test results."))
  }
  res[res$death.U071==1,"result"] <-1
  print(paste0("There are ",sum(res$result==1 & res$death.U071==1)," deaths with COVID-19. ",length(primary.covid.death.id)," of them primary death cause is COVID-19."))
  
  
  # summarise hospital inpatient data
  hesin.eid.ins <- paste0(hesin.date$eid,"_",hesin.date$ins_index)
  hesin_diag$eid.ins <- paste0(hesin_diag$eid,"_",hesin_diag$ins_index)
  hesin_u071 <- hesin_diag[hesin_diag$diag_icd10 == "U071" & 
                                hesin_diag$eid.ins %in% hesin.eid.ins,] 
  hesin_u071.id <- hesin_u071$eid %>% unique
  print(paste0(length(hesin_u071.id)," patients admitted to hospital were diagnosed as COVID-19 until ",Date, "."))
  
  hesin_u071.1.id <- hesin_diag[hesin_diag$diag_icd10 == "U071" & 
                                  hesin_diag$level == 1 & 
                                  hesin_diag$eid.ins %in% hesin.eid.ins,"eid"] %>% unique
  print(paste0(length(hesin_u071.1.id)," patients' primary diagnosis is COVID-19."))
  
  miss.id <- hesin_u071.id[!(hesin_u071.id %in% res$ID)] 
  if(length(miss.id)>0){
    print(paste0(length(miss.id)," hospitalisations with COVID-19 diagnosis were not in the test result file. Added them into phenotype data."))
    res.add <- as.data.frame(cbind(miss.id,1,0,0))
    colnames(res.add) <- colnames(res)
    res <- rbind(res.add,res)
  }
  
  res$hesin.U071 <- 0; res[res$ID %in% hesin_u071.id,"hesin.U071"] <- 1
  res$hospitalisation <- 0; res[res$ID %in% hesin_u071.1.id,"hospitalisation"] <- 1
  error <- res[res$hesin.U071==1 & res$result == 0,"ID"]
  if(length(error)>0){
    print(paste0(length(error)," patients in hospitalization with COVID-19 diagnosis but show negative in the result file. Modified their test results."))
  }
  res[res$hesin.U071==1,"result"] <-1
  
  # operation of covid-19 patients got
  oper4.code <- as.data.frame(fread(code.file))
  hesin.oper.u071 <- inner_join(hesin_u071, hesin_oper, by=c("eid","ins_index","arr_index")) 
  hesin.oper.u071.1 <- hesin.oper.u071[hesin.oper.u071$level.x==1,]
  hesin.oper.u071.1.id <- unique(hesin.oper.u071.1$eid)
  n.heisin.oper <- length(hesin.oper.u071.1.id)
  oper4.u071 <- as.data.frame(cbind(hesin.oper.u071.1.id,NA))
  for(i in 1:n.heisin.oper){
    sampID <- hesin.oper.u071.1.id[i]
    oper.name <- unique(hesin.oper.u071.1[hesin.oper.u071.1$eid == sampID,"oper4"])
    if(length(oper.name)==1) {
      oper4.u071[i,2] <- oper.name
    }else{
      if("X998" %in% oper.name) {
        oper4.u071[i,2] <- paste(oper.name[!(oper.name == "X998")],collapse = ",")
      }else{
        oper4.u071[i,2] <- paste(oper.name,collapse = ",")
      }
    }
  }
  colnames(oper4.u071) <- c("eid","hesin.oper4")
  res <- merge(res,oper4.u071,by.x = "ID",by.y = "eid",all.x=T)
  
  ### critical data
  hesin.c.u071 <- merge(hesin_u071,hesin_critical,by.x = c("eid","ins_index","arr_index"),by.y = c("eid","ins_index","arr_index"))
  hesin.c.u071$L2 <- hesin.c.u071$L3 <- 0
  hesin.c.u071[hesin.c.u071$cclev2days>0 & !(is.na(hesin.c.u071$cclev2days)),"L2"] <- 1
  hesin.c.u071[hesin.c.u071$cclev3days>0 & !(is.na(hesin.c.u071$cclev3days)),"L3"] <- 1
  # hesin.c.u071[hesin.c.u071$L2==0 & hesin.c.u071$L3==0,]
  hesin.res <- hesin.c.u071[,c("eid","L2","L3")]
  l2.id <- unique(hesin.res[hesin.res$L2==1,"eid"]) 
  l3.id <- unique(hesin.res[hesin.res$L3==1,"eid"]) 
  res$severe.level <- NA
  res[res$result==1,"severe.level"] <-0
  res[res$hospitalisation ==1,"severe.level"] <-1
  res[res$ID %in% l2.id,"severe.level"] <-2
  res[res$ID %in% l3.id,"severe.level"] <-3
  check <- res[res$severe.level==1 & !(is.na(res$severe.level)) & res$hesin.oper4 != "X998",]
  if(nrow(check)>0){
    lev2.add <- lev3.add <- c()
    for(ii in 1:nrow(check)){
      oper1 <- strsplit(check[ii,"hesin.oper4"],",")[[1]]
      if("E423" %in% oper1 | "E851" %in% oper1) lev3.add <- c(lev3.add,check[ii,"ID"])
      if("E852" %in% oper1 | "E856" %in% oper1) lev2.add <- c(lev2.add,check[ii,"ID"])
    }
    res[res$ID %in% lev2.add,"severe.level"] <- 2
    res[res$ID %in% lev3.add,"severe.level"] <- 3
  }
  print(paste0("There are ",sum(res$result)," COVID-19 patients identified. ",sum(res$hospitalisation,na.rm=T),
               " individuals are admitted to hospital. ",sum(res$severe.level==2,na.rm=T)," had been in ICU. ",sum(res$severe.level==3,na.rm=T)," had been in advanced ICU."))
  
  res[res$mortality == 1, "severe.level"] <- 4

  ### severe
  res$critical.care <- res$advanced.critical.care <- NA
  res[res$result ==1,"critical.care"] <- res[res$result ==1,"advanced.critical.care"] <-0
  res[res$severe.level >0 & !(is.na(res$severe.level)),"hospitalisation"] <- 1
  res[res$severe.level >1 & !(is.na(res$severe.level)),"critical.care"] <-1
  res[res$severe.level >2 & !(is.na(res$severe.level)),"advanced.critical.care"] <-1
  
  if(pheno.type == "susceptibility"){
    
    pheno <- res[,c("ID","result")]
    
    ## merge with full list of IDs
    ids <- fread(ukb.data, header = T, select = "f.eid", data.table = F, quote="", col.names="ID")
    res <- full_join(pheno, ids, by="ID")
    
    ## update susceptibility phenotypes
    # add phenotype: population v C19+
    res$ppl.result <- 0; res[res$result == 1 & !(is.na(res$result)),"ppl.result"] <- 1
    
    # reform  variables
    res <- data_reform(res, type = "susceptibility")
    
    # rename phenoytpes
    colnames(res)[colnames(res) == "ppl.result"] <- "pos.ppl"
    # rename phenotype: C19+ v C19-
    colnames(res)[colnames(res) == "result"] <- "pos.neg"
  }
  
  if(pheno.type == "mortality"){
    pheno <- res[res$result == 1, c("ID","mortality")]
    res <- data_reform(pheno, type = "mortality")
  }
  
  if(pheno.type == "severity"){
    pheno <- res[res$result == 1, c("ID","hospitalisation","critical.care","advanced.critical.care","mortality")]
    res <- data_reform(pheno, type = "severity")
  }
  
  ## return data.frame
  attr(res, "class") <- "data.frame"
  res
}


