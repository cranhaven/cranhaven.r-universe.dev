#' @import lme4
#' @import stats4
#' @import stats
#' @import plyr
#' @import methods
#' @import ggplot2
#' @import graphics
#' @import grDevices
#' @export
doseRecords <-
function(data){
  ratio <- NULL
  simulation <- NULL
  dose <- NULL
  analyF <- data
  analyF <- analyF[-which(analyF$n == "0"),]
  
  # Calculate the ratio of x = number of toxicities and n = number of allocated.

  analyF$ratio = rep(0, length(analyF$x))
  analyF$ratio =  analyF$x/analyF$n
  analyF$ratio[which(is.na(analyF$ratio))] = 0
  
  analyF <- analyF[order(analyF$dose), ]
  analyF <- analyF[order(analyF$simulation), ]
  
  colNum_Simu <- which(colnames(analyF) == "simulation")
  colNum_dose <- which(colnames(analyF) == "dose")
  colNum_ratio <- which(colnames(analyF) == "ratio")
  
  oanalyF0 = do.call(data.table, c(lapply(unname(analyF[c(colNum_Simu,colNum_dose)]), as.numeric), analyF[colNum_ratio]))
  colnames(oanalyF0) <- c("simulation", "dose", "ratio")
  oanalyF1 = oanalyF0[, .(min = min(ratio)), by= .(simulation, dose)]
  oanalyF2 = oanalyF0[, .(median = median(ratio)), by= .(simulation, dose)]
  oanalyF3 = oanalyF0[, .(max = max(ratio)), by= .(simulation, dose)]
  oanalyF4 = oanalyF0[, .(mean = mean(ratio)), by= .(simulation, dose)]
  oanalyF5 = count(oanalyF0,c('simulation', 'dose'))
  oanalyF = join_all(list(oanalyF5, oanalyF1, oanalyF2, oanalyF3, oanalyF4), by = c("simulation", "dose"))

  for(i in 1:nrow(oanalyF)){
    if(oanalyF$min[i] == "0" && oanalyF$max[i] == "0"){
      oanalyF$grp[i] = 1
    }else{
      oanalyF$grp[i] = 0
    }
  }
  
  oanalyF <- oanalyF[order(oanalyF$simulation, oanalyF$dose),]
  NanalyF <- merge(analyF, oanalyF, by = c("simulation", "dose"))
  
  rows_grp1 <- which(NanalyF$grp == "1")
  NanalyF_R <- NanalyF[-rows_grp1, ]  # This data set contains simulation dose records that have variability and 
                                      # can be used for random effect analysis.
  
  NanalyF_R <- NanalyF_R[, c("simulation", "dose", "Trial", "n", "x", "ratio", "min", "max", "mean", "grp")]
  NanalyF_R <- NanalyF_R[order(NanalyF_R$simulation, NanalyF_R$dose),]
  records <- list(doseRecords = NanalyF_R)
  return(records)
}
