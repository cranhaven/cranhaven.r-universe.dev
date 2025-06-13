#' @import data.table
#' @import plyr
#' @import stats4
#' @export 
VarWT <- function(dataTox, saveName = NULL){
    
    saveResult <-function(saveName, dataFrame){
      write.table(dataFrame, file = paste(saveName,".txt", sep = ""), sep = "\t")
    }
  
    # Nulling the global variables in the global environment.
    sigma2 <- NULL
    V1 <- NULL
    V2 <- NULL
    varwtdlt <- NULL
    n <- NULL

    analyf0 <- dataTox[order(dataTox$simulation), ]    # sort the imported data by simulation.
    # Initialize and import the dlt and lagr vectors in the data.
    analyf0$dlt = rep(0, nrow(analyf0))
    analyf0$lagr = rep(0, nrow(analyf0))
    
    nrow = which(analyf0$n>0)
    first.trial = findFirstLast(analyf0)$Trial
    total = rep(1:nrow(analyf0))
    total = total[-c(nrow, first.trial)]

    analyf0$dlt[nrow] = analyf0$x[nrow]/analyf0$n[nrow]
    analyf0$lagr[nrow] = analyf0$dlt[nrow]
    analyf0$dlt[first.trial] = 0
    analyf0$lagr[first.trial] = analyf0$dlt[first.trial]

    for(k in total){
        analyf0$dlt[k] = analyf0$lagr[k-1]
        analyf0$lagr[k] = analyf0$dlt[k]
    }

    zkodltvar <- analyf0
    colNum_lagr <- which(colnames(zkodltvar) == "lagr")
    zkodltvar <- zkodltvar[, -colNum_lagr]   # drop lagr column of the data frame

    ## This step is to calculate the sigma2 for each simulation, trial and dose level. 
    zkodltvar$sigma2 <- analyf0$n * analyf0$dlt * (1 - analyf0$dlt)
    zkodltvar

    #############################################################################################
    ## Now sum up the total variance for a dose DLT estimate across trials for each simulation ##
    #############################################################################################

    colNum_Simu <- which(colnames(zkodltvar) == "simulation")
    colNum_dose <- which(colnames(zkodltvar) == "dose")
    colNum_sigma2 <- which(colnames(zkodltvar) == "sigma2")
    varsum <- do.call(data.table, c(lapply(unname(zkodltvar[colNum_Simu:colNum_dose]), as.numeric), zkodltvar[colNum_sigma2]))
    varsum <- varsum[, .(sigma2 = sum(sigma2)), by= .(V1, V2)]
    colnames(varsum) <- c("simulation", "dose", "sumsigma2")
    varsum = as.data.frame(varsum)

    ## Now merge total variance for each simulation and dose ##
    zkodltvar <- zkodltvar[order(zkodltvar$simulation, zkodltvar$dose),]
    varmerge <- merge(zkodltvar, varsum, by = c("simulation", "dose"))
    # varmerge

    varweight <- varmerge

    varweight$varwt <- ifelse(varweight$sumsigma2 == 0,
                            1/20,  # if the condition is TRUE
                            varweight$sigma2 / varweight$sumsigma2  # if the condition is FALSE
                            )
    varweight$varwtdlt <- varweight$varwt * varweight$dlt

    ## Now combine the DLT estimates of all trials within a simulation to form
    ## the meta-analysis DLT estimates using variance based weights ##

    colNum_Simu2 <- which(colnames(varweight) == "simulation")
    colNum_dose2 <- which(colnames(varweight) == "dose")
    colNum_n <- which(colnames(varweight) == "n")
    colNum_varwtdlt <- which(colnames(varweight) == "varwtdlt")

    vardlt1 <- do.call(data.table, c(lapply(unname(varweight[c(colNum_Simu2,colNum_dose2)]), as.numeric), varweight[colNum_varwtdlt]))
    vardlt1 <- vardlt1[, .(varwtdlt = sum(varwtdlt)), by= .(V1, V2)]
    colnames(vardlt1) <- c("simulation", "dose", "dltvar")

    vardlt2 <- do.call(data.table, c(lapply(unname(varweight[c(colNum_Simu2,colNum_dose2)]), as.numeric), varweight[colNum_n]))
    vardlt2 <- vardlt2[, .(n = sum(n)), by= .(V1, V2)]
    colnames(vardlt2) <- c("simulation", "dose", "totaln")

    vardlt <- merge(vardlt2, vardlt1, by = c("simulation", "dose"))
    vardlt = as.data.frame(vardlt)
    
    if(is.null(saveName) == "FALSE"){
      saveResult(saveName, vardlt)
    }
    
    return(list(dataTox = vardlt))
}
