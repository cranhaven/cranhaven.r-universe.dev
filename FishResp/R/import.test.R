#' Import Background Respiration Data
#'
#' The function is used to import raw data of background respiration to R environment. The test should be done immediately before and/or after the actual metabolic rate measurements (pre-test and post-test, respectively).
#'
#' @usage
#' import.test(file, info.data,
#'             n.chamber = c(1,2,3,4,5,6,7,8),
#'             logger = c("AutoResp", "FishResp", "QboxAqua"),
#'             meas.to.wait = 0,
#'             plot.temperature = TRUE,
#'             plot.oxygen = TRUE)
#'
#' @param file  the name of a file  which the pre- or post-test data are to be read from. Note, if the file contains more than one measurement phase (e.g. M1 and M2), only the first one (M1) will be imported in R.
#' @param info.data  a data frame obtained by using the function \code{\link{input.info}}
#' @param n.chamber  integer: the number of chambers used in an experiment (including empty ones)
#' @param logger  string: the name of a logger software used for intermittent-flow respirometry:
#' \itemize{
#'   \item 'AutoResp' if you use commercial software by 'Loligo Systems'
#'   \item 'FishResp' if you use free software 'AquaResp' in combination with equipment produced by 'PreSens' or 'Pyroscience', please convert data to the 'FishResp' format using the functions \code{\link{presens.aquaresp}} or \code{\link{pyroscience.aquaresp}}, respectively. \cr If you do not use commercial software or 'AquaResp' for running intermittent-flow respirometry, adjust raw data manually to the 'FishResp' format (see Details below).
#'   \item 'QboxAqua' if you use commercial software by 'Qubit Systems'
#' }
#' @param meas.to.wait  integer: the number of first rows for each measurement phase (M) which should be reassigned to the wait phase (W). The parameter should be used when the wait phase (W) is absent (e.g. in 'Q-box Aqua' logger software) or not long enough to eliminate non-linear change in DO concentration over time from the measurement phase (M) after shutting off water supply from the ambient water source.
#' @param plot.temperature  logical: if TRUE then the graph of raw  temperature data is plotted
#' @param plot.oxygen  logical: if TRUE then the graph of raw oxygen data is plotted
#'
#' @details Do not use this function if an empty chamber is used for controlling background respiration in parallel with actual metabolic rate measurements. See about application of 'parallel' method in the function \code{\link{correct.meas}} \cr If you use closed respirometry approach, please standardize raw data. The example of "FishResp" format for 4-channel respirometry system is shown here:
#' \tabular{cccccccccc}{
#'   Date&Time \tab Phase \tab Temp.1 \tab Ox.1 \tab Temp.2 \tab Ox.2 \tab Temp.3 \tab Ox.3 \tab Temp.4 \tab Ox.4\cr
#'   19/08/2016/18:47:20 \tab F1 \tab 24.49 \tab 7.78 \tab 24.56 \tab 7.73 \tab 24.49 \tab 7.78 \tab 24.56 \tab 7.73\cr
#'   19/08/2016/18:47:21 \tab F1 \tab 24.49 \tab 7.78 \tab 24.56 \tab 7.73 \tab 24.49 \tab 7.78 \tab 24.56 \tab 7.73\cr
#'   19/08/2016/18:47:22 \tab M1 \tab 24.49 \tab 7.77 \tab 24.56 \tab 7.72 \tab 24.49 \tab 7.78 \tab 24.56 \tab 7.73\cr
#'   19/08/2016/18:47:23 \tab M1 \tab 24.49 \tab 7.76 \tab 24.56 \tab 7.72 \tab 24.49 \tab 7.78 \tab 24.56 \tab 7.73\cr
#' } where the items are:
#' \itemize{
#' \item Date&Time should be represented in one of the following formats: "dd/mm/yyyy/hh:mm:ss", "mm/dd/yyyy/hh:mm:ss", or "yyyy/mm/dd/hh:mm:ss". Time step-interval is one second: one row of data per second.
#' \item Phase should have at least two levels: M (measurement) and F (flush). The ordinal number of a phase should be attached to the level of a phase: F1, M1, F2, M2 ...
#' \item Temp.1 contains values of water temperature in Celsius (\eqn{C^{o}}) for Chamber 1
#' \item Ox.1 contains values of dissolved oxygen measured in 'mg/L', 'mmol/L' or 'ml/L' for Chamber 1. If other measurement units were used, convert them to 'mg/L', 'mmol/L' or 'ml/L' using the function \code{\link{convert.respirometry}} or \code{\link{convert.rMR}}.
#' \item ...
#' }
#'
#' @return The function returns a data frame containing standardized raw data of a background respiration test. The data frame should be used in the function \code{\link{correct.meas}} to correct metabolic rate measurements for background respiration.
#'
#' @importFrom grDevices dev.new
#' @importFrom graphics abline legend par plot
#' @importFrom stats coef lm predict.lm
#' @importFrom utils head read.table tail write.table
#'
#' @examples
#' # Import raw data for pre- and post-tests
#'
#' # if the data have been already loaded to R,
#' # skip the first line of the code:
#' data(info)
#'
#' pre.path = system.file("extdata/stickleback/pre_raw.txt.xz", package = "FishResp")
#' pre <- import.test(pre.path,
#'                    info.data = info,
#'                    logger = "AutoResp",
#'                    n.chamber = 4,
#'                    plot.temperature = TRUE,
#'                    plot.oxygen = TRUE)
#'
#' post.path = system.file("extdata/stickleback/post_raw.txt.xz", package = "FishResp")
#' post <- import.test(post.path,
#'                     info.data = info,
#'                     logger = "AutoResp",
#'                     n.chamber = 4,
#'                     plot.temperature = TRUE,
#'                     plot.oxygen = TRUE)
#'
#' @export

import.test <- function(file, info.data,
                        n.chamber = c(1,2,3,4,5,6,7,8),
                        logger = c("AutoResp", "FishResp", "QboxAqua"),
                        meas.to.wait = 0,
                        plot.temperature = TRUE,
                        plot.oxygen = TRUE){
  V1 <- V2 <- V3 <- V4 <- V5 <- V6 <- V7 <- V8 <- V9 <- V10 <- NULL
  V11 <- V12 <- V13 <- V14 <- V15 <- V16 <- V17 <- V18 <- V19 <- NULL
  V20 <- V21 <- V22 <- V23 <- V24 <- V25 <- V26 <- V27 <- V28 <- NULL
  Phase <- Temp.1 <- Ox.1 <- Chamber.No <- Test <- Time <- Init.O2 <- NULL
  Temp <- O2 <- Temp.2 <- Ox.2 <- Temp.3 <- Ox.3 <- Temp.4 <- Ox.4 <- NULL
  Temp.5 <- Ox.5 <- Temp.6 <- Ox.6 <- Temp.7 <- Ox.7 <- Temp.8 <- Ox.8 <- NULL

  ### AutoResp format ###
  if (logger == "AutoResp"){
    test.data<-read.table(file, sep = "\t", skip=38, header=F, strip.white=T)

    if (n.chamber == 1){
      test.data<-subset(test.data, select=c(V1, V2, V6, V7))
      names(test.data)<-c("Time", "Phase", "Temp.1", "Ox.1")
      for(i in 3:ncol(test.data)){test.data[,i] = as.numeric(gsub(',','.', test.data[,i]))}
      aaa <- max(test.data$Ox.1, na.rm = TRUE)
      if(all(is.na(test.data$Ox.1))){test.data$Ox.1[is.na(test.data$Ox.1)] <- aaa}
    }
    else if (n.chamber == 2){
      test.data<-subset(test.data, select=c(V1, V2, V6, V7, V9, V10))
      names(test.data)<-c("Time", "Phase", "Temp.1", "Ox.1", "Temp.2", "Ox.2")
      for(i in 3:ncol(test.data)){test.data[,i] = as.numeric(gsub(',','.', test.data[,i]))}
      aaa <- max(c(test.data$Ox.1, test.data$Ox.2), na.rm = TRUE)
      if(all(is.na(test.data$Ox.1))){test.data$Ox.1[is.na(test.data$Ox.1)] <- aaa}
      if(all(is.na(test.data$Ox.2))){test.data$Ox.2[is.na(test.data$Ox.2)] <- aaa}
    }
    else if (n.chamber == 3){
      test.data<-subset(test.data, select=c(V1, V2, V6, V7, V9, V10, V12, V13))
      names(test.data)<-c("Time", "Phase", "Temp.1", "Ox.1", "Temp.2", "Ox.2", "Temp.3", "Ox.3")
      for(i in 3:ncol(test.data)){test.data[,i] = as.numeric(gsub(',','.', test.data[,i]))}
      aaa <- max(c(test.data$Ox.1, test.data$Ox.2, test.data$Ox.3), na.rm = TRUE)
      if(all(is.na(test.data$Ox.1))){test.data$Ox.1[is.na(test.data$Ox.1)] <- aaa}
      if(all(is.na(test.data$Ox.2))){test.data$Ox.2[is.na(test.data$Ox.2)] <- aaa}
      if(all(is.na(test.data$Ox.3))){test.data$Ox.3[is.na(test.data$Ox.3)] <- aaa}
    }
    else if (n.chamber == 4){
      test.data<-subset(test.data, select=c(V1, V2, V6, V7, V9, V10, V12, V13, V15, V16))
      names(test.data)<-c("Time", "Phase", "Temp.1", "Ox.1", "Temp.2", "Ox.2", "Temp.3", "Ox.3", "Temp.4", "Ox.4")
      for(i in 3:ncol(test.data)){test.data[,i] = as.numeric(gsub(',','.', test.data[,i]))}
      aaa <- max(c(test.data$Ox.1, test.data$Ox.2, test.data$Ox.3, test.data$Ox.4), na.rm = TRUE)
      if(all(is.na(test.data$Ox.1))){test.data$Ox.1[is.na(test.data$Ox.1)] <- aaa}
      if(all(is.na(test.data$Ox.2))){test.data$Ox.2[is.na(test.data$Ox.2)] <- aaa}
      if(all(is.na(test.data$Ox.3))){test.data$Ox.3[is.na(test.data$Ox.3)] <- aaa}
      if(all(is.na(test.data$Ox.4))){test.data$Ox.4[is.na(test.data$Ox.4)] <- aaa}
    }
    else if (n.chamber == 5){
      test.data<-subset(test.data, select=c(V1, V2, V6, V7, V9, V10, V12, V13, V15, V16, V18, V19))
      names(test.data)<-c("Time", "Phase", "Temp.1", "Ox.1", "Temp.2", "Ox.2", "Temp.3", "Ox.3", "Temp.4", "Ox.4", "Temp.5", "Ox.5")
      for(i in 3:ncol(test.data)){test.data[,i] = as.numeric(gsub(',','.', test.data[,i]))}
      aaa <- max(c(test.data$Ox.1, test.data$Ox.2, test.data$Ox.3, test.data$Ox.4,
                   test.data$Ox.5), na.rm = TRUE)
      if(all(is.na(test.data$Ox.1))){test.data$Ox.1[is.na(test.data$Ox.1)] <- aaa}
      if(all(is.na(test.data$Ox.2))){test.data$Ox.2[is.na(test.data$Ox.2)] <- aaa}
      if(all(is.na(test.data$Ox.3))){test.data$Ox.3[is.na(test.data$Ox.3)] <- aaa}
      if(all(is.na(test.data$Ox.4))){test.data$Ox.4[is.na(test.data$Ox.4)] <- aaa}
      if(all(is.na(test.data$Ox.5))){test.data$Ox.5[is.na(test.data$Ox.5)] <- aaa}
    }
    else if (n.chamber == 6){
      test.data<-subset(test.data, select=c(V1, V2, V6, V7, V9, V10, V12, V13, V15, V16, V18, V19, V21, V22))
      names(test.data)<-c("Time", "Phase", "Temp.1", "Ox.1", "Temp.2", "Ox.2", "Temp.3", "Ox.3", "Temp.4", "Ox.4", "Temp.5", "Ox.5", "Temp.6", "Ox.6")
      for(i in 3:ncol(test.data)){test.data[,i] = as.numeric(gsub(',','.', test.data[,i]))}
      aaa <- max(c(test.data$Ox.1, test.data$Ox.2, test.data$Ox.3, test.data$Ox.4,
                   test.data$Ox.5, test.data$Ox.6), na.rm = TRUE)
      if(all(is.na(test.data$Ox.1))){test.data$Ox.1[is.na(test.data$Ox.1)] <- aaa}
      if(all(is.na(test.data$Ox.2))){test.data$Ox.2[is.na(test.data$Ox.2)] <- aaa}
      if(all(is.na(test.data$Ox.3))){test.data$Ox.3[is.na(test.data$Ox.3)] <- aaa}
      if(all(is.na(test.data$Ox.4))){test.data$Ox.4[is.na(test.data$Ox.4)] <- aaa}
      if(all(is.na(test.data$Ox.5))){test.data$Ox.5[is.na(test.data$Ox.5)] <- aaa}
      if(all(is.na(test.data$Ox.6))){test.data$Ox.6[is.na(test.data$Ox.6)] <- aaa}
    }
    else if (n.chamber == 7){
      test.data<-subset(test.data, select=c(V1, V2, V6, V7, V9, V10, V12, V13, V15, V16, V18, V19, V21, V22, V24, V25))
      names(test.data)<-c("Time", "Phase", "Temp.1", "Ox.1", "Temp.2", "Ox.2", "Temp.3", "Ox.3", "Temp.4", "Ox.4", "Temp.5", "Ox.5", "Temp.6", "Ox.6", "Temp.7", "Ox.7")
      for(i in 3:ncol(test.data)){test.data[,i] = as.numeric(gsub(',','.', test.data[,i]))}
      aaa <- max(c(test.data$Ox.1, test.data$Ox.2, test.data$Ox.3, test.data$Ox.4,
                   test.data$Ox.5, test.data$Ox.6, test.data$Ox.7), na.rm = TRUE)
      if(all(is.na(test.data$Ox.1))){test.data$Ox.1[is.na(test.data$Ox.1)] <- aaa}
      if(all(is.na(test.data$Ox.2))){test.data$Ox.2[is.na(test.data$Ox.2)] <- aaa}
      if(all(is.na(test.data$Ox.3))){test.data$Ox.3[is.na(test.data$Ox.3)] <- aaa}
      if(all(is.na(test.data$Ox.4))){test.data$Ox.4[is.na(test.data$Ox.4)] <- aaa}
      if(all(is.na(test.data$Ox.5))){test.data$Ox.5[is.na(test.data$Ox.5)] <- aaa}
      if(all(is.na(test.data$Ox.6))){test.data$Ox.6[is.na(test.data$Ox.6)] <- aaa}
      if(all(is.na(test.data$Ox.7))){test.data$Ox.7[is.na(test.data$Ox.7)] <- aaa}
    }
    else if (n.chamber == 8){
      test.data<-subset(test.data, select=c(V1, V2, V6, V7, V9, V10, V12, V13, V15, V16, V18, V19, V21, V22, V24, V25, V27, V28))
      names(test.data)<-c("Time", "Phase", "Temp.1", "Ox.1", "Temp.2", "Ox.2", "Temp.3", "Ox.3", "Temp.4", "Ox.4", "Temp.5", "Ox.5", "Temp.6", "Ox.6", "Temp.7", "Ox.7", "Temp.8", "Ox.8")
      for(i in 3:ncol(test.data)){test.data[,i] = as.numeric(gsub(',','.', test.data[,i]))}
      aaa <- max(c(test.data$Ox.1, test.data$Ox.2, test.data$Ox.3, test.data$Ox.4,
                   test.data$Ox.5, test.data$Ox.6, test.data$Ox.7, test.data$Ox.8), na.rm = TRUE)
      if(all(is.na(test.data$Ox.1))){test.data$Ox.1[is.na(test.data$Ox.1)] <- aaa}
      if(all(is.na(test.data$Ox.2))){test.data$Ox.2[is.na(test.data$Ox.2)] <- aaa}
      if(all(is.na(test.data$Ox.3))){test.data$Ox.3[is.na(test.data$Ox.3)] <- aaa}
      if(all(is.na(test.data$Ox.4))){test.data$Ox.4[is.na(test.data$Ox.4)] <- aaa}
      if(all(is.na(test.data$Ox.5))){test.data$Ox.5[is.na(test.data$Ox.5)] <- aaa}
      if(all(is.na(test.data$Ox.6))){test.data$Ox.6[is.na(test.data$Ox.6)] <- aaa}
      if(all(is.na(test.data$Ox.7))){test.data$Ox.7[is.na(test.data$Ox.7)] <- aaa}
      if(all(is.na(test.data$Ox.8))){test.data$Ox.8[is.na(test.data$Ox.8)] <- aaa}
    }
    else{
      print("Please, choose number of chambers between 1 and 8")
    }
  }


  ### FishResp format ###

  else if (logger == "FishResp"){
    test.data<-read.table(file, sep = "\t", skip=1, header=F, strip.white=T)
    if (n.chamber == 1){
      test.data<-subset(test.data, select=c(V1, V2, V3, V4))
      names(test.data)<-c("Time", "Phase", "Temp.1", "Ox.1")
      for(i in 3:ncol(test.data)){test.data[,i] = as.numeric(gsub(',','.', test.data[,i]))}
      aaa <- max(test.data$Ox.1, na.rm = TRUE)
      if(all(is.na(test.data$Ox.1))){test.data$Ox.1[is.na(test.data$Ox.1)] <- aaa}
    }
    else if (n.chamber == 2){
      test.data<-subset(test.data, select=c(V1, V2, V3, V4, V5, V6))
      names(test.data)<-c("Time", "Phase", "Temp.1", "Ox.1", "Temp.2", "Ox.2")
      for(i in 3:ncol(test.data)){test.data[,i] = as.numeric(gsub(',','.', test.data[,i]))}
      aaa <- max(c(test.data$Ox.1, test.data$Ox.2), na.rm = TRUE)
      if(all(is.na(test.data$Ox.1))){test.data$Ox.1[is.na(test.data$Ox.1)] <- aaa}
      if(all(is.na(test.data$Ox.2))){test.data$Ox.2[is.na(test.data$Ox.2)] <- aaa}
    }
    else if (n.chamber == 3){
      test.data<-subset(test.data, select=c(V1, V2, V3, V4, V5, V6, V7, V8))
      names(test.data)<-c("Time", "Phase", "Temp.1", "Ox.1", "Temp.2", "Ox.2", "Temp.3", "Ox.3")
      for(i in 3:ncol(test.data)){test.data[,i] = as.numeric(gsub(',','.', test.data[,i]))}
      aaa <- max(c(test.data$Ox.1, test.data$Ox.2, test.data$Ox.3), na.rm = TRUE)
      if(all(is.na(test.data$Ox.1))){test.data$Ox.1[is.na(test.data$Ox.1)] <- aaa}
      if(all(is.na(test.data$Ox.2))){test.data$Ox.2[is.na(test.data$Ox.2)] <- aaa}
      if(all(is.na(test.data$Ox.3))){test.data$Ox.3[is.na(test.data$Ox.3)] <- aaa}
    }
    else if (n.chamber == 4){
      test.data<-subset(test.data, select=c(V1, V2, V3, V4, V5, V6, V7, V8, V9, V10))
      names(test.data)<-c("Time", "Phase", "Temp.1", "Ox.1", "Temp.2", "Ox.2", "Temp.3", "Ox.3", "Temp.4", "Ox.4")
      for(i in 3:ncol(test.data)){test.data[,i] = as.numeric(gsub(',','.', test.data[,i]))}
      aaa <- max(c(test.data$Ox.1, test.data$Ox.2, test.data$Ox.3, test.data$Ox.4), na.rm = TRUE)
      if(all(is.na(test.data$Ox.1))){test.data$Ox.1[is.na(test.data$Ox.1)] <- aaa}
      if(all(is.na(test.data$Ox.2))){test.data$Ox.2[is.na(test.data$Ox.2)] <- aaa}
      if(all(is.na(test.data$Ox.3))){test.data$Ox.3[is.na(test.data$Ox.3)] <- aaa}
      if(all(is.na(test.data$Ox.4))){test.data$Ox.4[is.na(test.data$Ox.4)] <- aaa}
    }
    else if (n.chamber == 5){
      test.data<-subset(test.data, select=c(V1, V2, V3, V4, V5, V6, V7, V8, V9, V10, V11, V12))
      names(test.data)<-c("Time", "Phase", "Temp.1", "Ox.1", "Temp.2", "Ox.2", "Temp.3", "Ox.3", "Temp.4", "Ox.4", "Temp.5", "Ox.5")
      for(i in 3:ncol(test.data)){test.data[,i] = as.numeric(gsub(',','.', test.data[,i]))}
      aaa <- max(c(test.data$Ox.1, test.data$Ox.2, test.data$Ox.3, test.data$Ox.4,
                   test.data$Ox.5), na.rm = TRUE)
      if(all(is.na(test.data$Ox.1))){test.data$Ox.1[is.na(test.data$Ox.1)] <- aaa}
      if(all(is.na(test.data$Ox.2))){test.data$Ox.2[is.na(test.data$Ox.2)] <- aaa}
      if(all(is.na(test.data$Ox.3))){test.data$Ox.3[is.na(test.data$Ox.3)] <- aaa}
      if(all(is.na(test.data$Ox.4))){test.data$Ox.4[is.na(test.data$Ox.4)] <- aaa}
      if(all(is.na(test.data$Ox.5))){test.data$Ox.5[is.na(test.data$Ox.5)] <- aaa}
    }
    else if (n.chamber == 6){
      test.data<-subset(test.data, select=c(V1, V2, V3, V4, V5, V6, V7, V8, V9, V10, V11, V12, V13, V14))
      names(test.data)<-c("Time", "Phase", "Temp.1", "Ox.1", "Temp.2", "Ox.2", "Temp.3", "Ox.3", "Temp.4", "Ox.4", "Temp.5", "Ox.5", "Temp.6", "Ox.6")
      for(i in 3:ncol(test.data)){test.data[,i] = as.numeric(gsub(',','.', test.data[,i]))}
      aaa <- max(c(test.data$Ox.1, test.data$Ox.2, test.data$Ox.3, test.data$Ox.4,
                   test.data$Ox.5, test.data$Ox.6), na.rm = TRUE)
      if(all(is.na(test.data$Ox.1))){test.data$Ox.1[is.na(test.data$Ox.1)] <- aaa}
      if(all(is.na(test.data$Ox.2))){test.data$Ox.2[is.na(test.data$Ox.2)] <- aaa}
      if(all(is.na(test.data$Ox.3))){test.data$Ox.3[is.na(test.data$Ox.3)] <- aaa}
      if(all(is.na(test.data$Ox.4))){test.data$Ox.4[is.na(test.data$Ox.4)] <- aaa}
      if(all(is.na(test.data$Ox.5))){test.data$Ox.5[is.na(test.data$Ox.5)] <- aaa}
      if(all(is.na(test.data$Ox.6))){test.data$Ox.6[is.na(test.data$Ox.6)] <- aaa}
    }
    else if (n.chamber == 7){
      test.data<-subset(test.data, select=c(V1, V2, V3, V4, V5, V6, V7, V8, V9, V10, V11, V12, V13, V14, V15, V16))
      names(test.data)<-c("Time", "Phase", "Temp.1", "Ox.1", "Temp.2", "Ox.2", "Temp.3", "Ox.3", "Temp.4", "Ox.4", "Temp.5", "Ox.5", "Temp.6", "Ox.6", "Temp.7", "Ox.7")
      for(i in 3:ncol(test.data)){test.data[,i] = as.numeric(gsub(',','.', test.data[,i]))}
      aaa <- max(c(test.data$Ox.1, test.data$Ox.2, test.data$Ox.3, test.data$Ox.4,
                   test.data$Ox.5, test.data$Ox.6, test.data$Ox.7), na.rm = TRUE)
      if(all(is.na(test.data$Ox.1))){test.data$Ox.1[is.na(test.data$Ox.1)] <- aaa}
      if(all(is.na(test.data$Ox.2))){test.data$Ox.2[is.na(test.data$Ox.2)] <- aaa}
      if(all(is.na(test.data$Ox.3))){test.data$Ox.3[is.na(test.data$Ox.3)] <- aaa}
      if(all(is.na(test.data$Ox.4))){test.data$Ox.4[is.na(test.data$Ox.4)] <- aaa}
      if(all(is.na(test.data$Ox.5))){test.data$Ox.5[is.na(test.data$Ox.5)] <- aaa}
      if(all(is.na(test.data$Ox.6))){test.data$Ox.6[is.na(test.data$Ox.6)] <- aaa}
      if(all(is.na(test.data$Ox.7))){test.data$Ox.7[is.na(test.data$Ox.7)] <- aaa}
    }
    else if (n.chamber == 8){
      test.data<-subset(test.data, select=c(V1, V2, V3, V4, V5, V6, V7, V8, V9, V10, V11, V12, V13, V14, V15, V16, V17, V18))
      names(test.data)<-c("Time", "Phase", "Temp.1", "Ox.1", "Temp.2", "Ox.2", "Temp.3", "Ox.3", "Temp.4", "Ox.4", "Temp.5", "Ox.5", "Temp.6", "Ox.6", "Temp.7", "Ox.7", "Temp.8", "Ox.8")
      for(i in 3:ncol(test.data)){test.data[,i] = as.numeric(gsub(',','.', test.data[,i]))}
      aaa <- max(c(test.data$Ox.1, test.data$Ox.2, test.data$Ox.3, test.data$Ox.4,
                   test.data$Ox.5, test.data$Ox.6, test.data$Ox.7, test.data$Ox.8), na.rm = TRUE)
      if(all(is.na(test.data$Ox.1))){test.data$Ox.1[is.na(test.data$Ox.1)] <- aaa}
      if(all(is.na(test.data$Ox.2))){test.data$Ox.2[is.na(test.data$Ox.2)] <- aaa}
      if(all(is.na(test.data$Ox.3))){test.data$Ox.3[is.na(test.data$Ox.3)] <- aaa}
      if(all(is.na(test.data$Ox.4))){test.data$Ox.4[is.na(test.data$Ox.4)] <- aaa}
      if(all(is.na(test.data$Ox.5))){test.data$Ox.5[is.na(test.data$Ox.5)] <- aaa}
      if(all(is.na(test.data$Ox.6))){test.data$Ox.6[is.na(test.data$Ox.6)] <- aaa}
      if(all(is.na(test.data$Ox.7))){test.data$Ox.7[is.na(test.data$Ox.7)] <- aaa}
      if(all(is.na(test.data$Ox.8))){test.data$Ox.8[is.na(test.data$Ox.8)] <- aaa}
    }
    else{
      print("Please, choose number of chambers between 1 and 8")
    }
  }


  ### QboxAqua format ###

  else if (logger == "QboxAqua"){
    test.data<-read.table(file, sep = ",", skip=2, header=F, strip.white=T)
    if (n.chamber == 1){
      test.data<-subset(test.data, select=c(V1, V9, V4, ncol(test.data)))
      names(test.data)<-c("Time", "Phase", "Temp.1", "Ox.1")
      for(i in 3:ncol(test.data)){test.data[,i] = as.numeric(gsub(',','.', test.data[,i]))}
      aaa <- max(test.data$Ox.1, na.rm = TRUE)
      if(all(is.na(test.data$Ox.1))){test.data$Ox.1[is.na(test.data$Ox.1)] <- aaa}

      ### Indexing measurement phases for QboxAqua
      test.data$Phase[test.data$Phase == "1"] <- "F"
      test.data$Phase[test.data$Phase == "0"] <- "M"
      bdrs <- which(c(FALSE, tail(test.data$Phase,-1) != head(test.data$Phase,-1)))
      bdrs <- paste(bdrs , test.data$Phase[bdrs], sep = "")
      M.start <- grep('M', bdrs, value=TRUE)
      M.start <- as.integer(sub("M$", "", M.start))
      M.end <- grep('F', bdrs, value=TRUE)
      M.end <- as.integer(sub("F$", "", M.end)) - 1
      if(M.end[1] < M.start[1]){M.end <- M.end[-1]}

      for(i in 1:length(M.end)){
        test.data$Phase[M.start[i]:M.end[i]] <- paste(test.data$Phase[M.start[i]:M.end[i]], i, sep = "")
      }
    }
    else{
      print("If 'Qubit Systems' starts producing multi-chamber systems for aquatic respirometry, please contact us via email: fishresp@gmail.com")
    }
  }


  else{
    print("Please, choose the format of your data: AutoResp, FishResp or QboxAqua")
  }

  rm(aaa)
  test.data <- subset(test.data, Phase == "M1")
  test.data$Phase <- factor(test.data$Phase)

  # cut off first n raws from 'M' phase
  if(meas.to.wait != 0){
    idx <- unlist(tapply(1:nrow(test.data), test.data$Phase, tail, -(meas.to.wait)), use.names=FALSE)
    test.data <- test.data[idx, ]
  }else{
    }

  if(n.chamber == 1){
    test.CH1<-subset(test.data, select=c(Temp.1, Ox.1))
    row.names(test.CH1)<-NULL
    names(test.CH1)<-c("Temp", "O2")
    test.CH1$Chamber.No<-as.factor(rep("CH1", dim(test.CH1)[1]))
    test.CH1$Test<-as.factor(rep("test", dim(test.CH1)[1]))
    test.CH1$Time<-1:dim(test.CH1)[1]
    test.CH1$Init.O2<-rep(mean(test.CH1$O2[1]), dim(test.CH1)[1])
    test.CH1<-subset(test.CH1, select=c(Chamber.No, Test, Time, Init.O2, Temp, O2))

    test.data<-rbind(test.CH1)
    test.data$delta.O2<-test.data$O2 - test.data$Init.O2

    if (plot.temperature == FALSE){
    }
    else if(plot.temperature == TRUE){
      par(mfrow=c(1,1), ask = T)
      plot(test.CH1$Temp~test.CH1$Time, main="Chamber 1", xlab = "Time (s)", ylab = bquote("Temperature (" ~ C^o ~ ")"), col = "#0082FF", cex=0.8)
    }
    else{
      "Please, choose: TRUE or FALSE"
    }

    if (plot.oxygen == FALSE){
    }
    else if(plot.oxygen == TRUE){
      par(mfrow=c(1,1), ask = T)
      plot(test.CH1$O2~test.CH1$Time, main="Chamber 1", xlab = "Time (s)", ylab = paste("DO (", info.data$DO.unit[1], "/L)", sep = ""), col = "#0082FF", cex=0.8)
    }
    else{
      "Please, choose: TRUE or FALSE"
    }

    return(test.data)
  }

  else if(n.chamber == 2){
    test.CH1<-subset(test.data, select=c(Temp.1, Ox.1))
    row.names(test.CH1)<-NULL
    names(test.CH1)<-c("Temp", "O2")
    test.CH1$Chamber.No<-as.factor(rep("CH1", dim(test.CH1)[1]))
    test.CH1$Test<-as.factor(rep("test", dim(test.CH1)[1]))
    test.CH1$Time<-1:dim(test.CH1)[1]
    test.CH1$Init.O2<-rep(mean(test.CH1$O2[1]), dim(test.CH1)[1])
    test.CH1<-subset(test.CH1, select=c(Chamber.No, Test, Time, Init.O2, Temp, O2))

    test.CH2<-subset(test.data, select=c(Temp.2, Ox.2))
    row.names(test.CH2)<-NULL
    names(test.CH2)<-c("Temp", "O2")
    test.CH2$Chamber.No<-as.factor(rep("CH2", dim(test.CH2)[1]))
    test.CH2$Test<-as.factor(rep("test", dim(test.CH2)[1]))
    test.CH2$Time<-1:dim(test.CH2)[1]
    test.CH2$Init.O2<-rep(mean(test.CH2$O2[1]), dim(test.CH2)[1])
    test.CH2<-subset(test.CH2, select=c(Chamber.No, Test, Time, Init.O2, Temp, O2))
    test.data<-rbind(test.CH1, test.CH2)
    test.data$delta.O2<-test.data$O2 - test.data$Init.O2

    if (plot.temperature == FALSE){
    }
    else if(plot.temperature == TRUE){
      par(mfrow=c(2,1), ask = T)
      plot(test.CH1$Temp~test.CH1$Time, main="Chamber 1", xlab = "Time (s)", ylab = bquote("Temperature (" ~ C^o ~ ")"), col = "#0082FF", cex=0.8)
      plot(test.CH2$Temp~test.CH2$Time, main="Chamber 2", xlab = "Time (s)", ylab = bquote("Temperature (" ~ C^o ~ ")"), col = "#0082FF", cex=0.8)
    }
    else{
      "Please, choose: TRUE or FALSE"
    }

    if (plot.oxygen == FALSE){
    }
    else if(plot.oxygen == TRUE){
      par(mfrow=c(2,1), ask = T)
      plot(test.CH1$O2~test.CH1$Time, main="Chamber 1", xlab = "Time (s)", ylab = paste("DO (", info.data$DO.unit[1], "/L)", sep = ""), col = "#0082FF", cex=0.8)
      plot(test.CH2$O2~test.CH2$Time, main="Chamber 2", xlab = "Time (s)", ylab = paste("DO (", info.data$DO.unit[1], "/L)", sep = ""), col = "#0082FF", cex=0.8)
    }
    else{
      "Please, choose: TRUE or FALSE"
    }

    return(test.data)
  }

  else if(n.chamber == 3){
    test.CH1<-subset(test.data, select=c(Temp.1, Ox.1))
    row.names(test.CH1)<-NULL
    names(test.CH1)<-c("Temp", "O2")
    test.CH1$Chamber.No<-as.factor(rep("CH1", dim(test.CH1)[1]))
    test.CH1$Test<-as.factor(rep("test", dim(test.CH1)[1]))
    test.CH1$Time<-1:dim(test.CH1)[1]
    test.CH1$Init.O2<-rep(mean(test.CH1$O2[1]), dim(test.CH1)[1])
    test.CH1<-subset(test.CH1, select=c(Chamber.No, Test, Time, Init.O2, Temp, O2))

    test.CH2<-subset(test.data, select=c(Temp.2, Ox.2))
    row.names(test.CH2)<-NULL
    names(test.CH2)<-c("Temp", "O2")
    test.CH2$Chamber.No<-as.factor(rep("CH2", dim(test.CH2)[1]))
    test.CH2$Test<-as.factor(rep("test", dim(test.CH2)[1]))
    test.CH2$Time<-1:dim(test.CH2)[1]
    test.CH2$Init.O2<-rep(mean(test.CH2$O2[1]), dim(test.CH2)[1])
    test.CH2<-subset(test.CH2, select=c(Chamber.No, Test, Time, Init.O2, Temp, O2))

    test.CH3<-subset(test.data, select=c(Temp.3, Ox.3))
    row.names(test.CH3)<-NULL
    names(test.CH3)<-c("Temp", "O2")
    test.CH3$Chamber.No<-as.factor(rep("CH3", dim(test.CH3)[1]))
    test.CH3$Test<-as.factor(rep("test", dim(test.CH3)[1]))
    test.CH3$Time<-1:dim(test.CH3)[1]
    test.CH3$Init.O2<-rep(mean(test.CH3$O2[1]), dim(test.CH3)[1])
    test.CH3<-subset(test.CH3, select=c(Chamber.No, Test, Time, Init.O2, Temp, O2))
    test.data<-rbind(test.CH1, test.CH2, test.CH3)
    test.data$delta.O2<-test.data$O2 - test.data$Init.O2

    if (plot.temperature == FALSE){
    }
    else if(plot.temperature == TRUE){
      par(mfrow=c(3,1), ask = T)
      plot(test.CH1$Temp~test.CH1$Time, main="Chamber 1", xlab = "Time (s)", ylab = bquote("Temperature (" ~ C^o ~ ")"), col = "#0082FF", cex=0.8)
      plot(test.CH2$Temp~test.CH2$Time, main="Chamber 2", xlab = "Time (s)", ylab = bquote("Temperature (" ~ C^o ~ ")"), col = "#0082FF", cex=0.8)
      plot(test.CH3$Temp~test.CH3$Time, main="Chamber 3", xlab = "Time (s)", ylab = bquote("Temperature (" ~ C^o ~ ")"), col = "#0082FF", cex=0.8)
    }
    else{
      "Please, choose: TRUE or FALSE"
    }

    if (plot.oxygen == FALSE){
    }
    else if(plot.oxygen == TRUE){
      par(mfrow=c(3,1), ask = T)
      plot(test.CH1$O2~test.CH1$Time, main="Chamber 1", xlab = "Time (s)", ylab = paste("DO (", info.data$DO.unit[1], "/L)", sep = ""), col = "#0082FF", cex=0.8)
      plot(test.CH2$O2~test.CH2$Time, main="Chamber 2", xlab = "Time (s)", ylab = paste("DO (", info.data$DO.unit[1], "/L)", sep = ""), col = "#0082FF", cex=0.8)
      plot(test.CH3$O2~test.CH3$Time, main="Chamber 3", xlab = "Time (s)", ylab = paste("DO (", info.data$DO.unit[1], "/L)", sep = ""), col = "#0082FF", cex=0.8)
    }
    else{
      "Please, choose: TRUE or FALSE"
    }

    return(test.data)
  }

  else if(n.chamber == 4){
    test.CH1<-subset(test.data, select=c(Temp.1, Ox.1))
    row.names(test.CH1)<-NULL
    names(test.CH1)<-c("Temp", "O2")
    test.CH1$Chamber.No<-as.factor(rep("CH1", dim(test.CH1)[1]))
    test.CH1$Test<-as.factor(rep("test", dim(test.CH1)[1]))
    test.CH1$Time<-1:dim(test.CH1)[1]
    test.CH1$Init.O2<-rep(mean(test.CH1$O2[1]), dim(test.CH1)[1])
    test.CH1<-subset(test.CH1, select=c(Chamber.No, Test, Time, Init.O2, Temp, O2))

    test.CH2<-subset(test.data, select=c(Temp.2, Ox.2))
    row.names(test.CH2)<-NULL
    names(test.CH2)<-c("Temp", "O2")
    test.CH2$Chamber.No<-as.factor(rep("CH2", dim(test.CH2)[1]))
    test.CH2$Test<-as.factor(rep("test", dim(test.CH2)[1]))
    test.CH2$Time<-1:dim(test.CH2)[1]
    test.CH2$Init.O2<-rep(mean(test.CH2$O2[1]), dim(test.CH2)[1])
    test.CH2<-subset(test.CH2, select=c(Chamber.No, Test, Time, Init.O2, Temp, O2))

    test.CH3<-subset(test.data, select=c(Temp.3, Ox.3))
    row.names(test.CH3)<-NULL
    names(test.CH3)<-c("Temp", "O2")
    test.CH3$Chamber.No<-as.factor(rep("CH3", dim(test.CH3)[1]))
    test.CH3$Test<-as.factor(rep("test", dim(test.CH3)[1]))
    test.CH3$Time<-1:dim(test.CH3)[1]
    test.CH3$Init.O2<-rep(mean(test.CH3$O2[1]), dim(test.CH3)[1])
    test.CH3<-subset(test.CH3, select=c(Chamber.No, Test, Time, Init.O2, Temp, O2))

    test.CH4<-subset(test.data, select=c(Temp.4, Ox.4))
    row.names(test.CH4)<-NULL
    names(test.CH4)<-c("Temp", "O2")
    test.CH4$Chamber.No<-as.factor(rep("CH4", dim(test.CH4)[1]))
    test.CH4$Test<-as.factor(rep("test", dim(test.CH4)[1]))
    test.CH4$Time<-1:dim(test.CH4)[1]
    test.CH4$Init.O2<-rep(mean(test.CH4$O2[1]), dim(test.CH4)[1])
    test.CH4<-subset(test.CH4, select=c(Chamber.No, Test, Time, Init.O2, Temp, O2))

    test.data<-rbind(test.CH1, test.CH2, test.CH3, test.CH4)
    test.data$delta.O2<-test.data$O2 - test.data$Init.O2

    if (plot.temperature == FALSE){
    }
    else if(plot.temperature == TRUE){
      par(mfrow=c(4,1), ask = T)
      plot(test.CH1$Temp~test.CH1$Time, main="Chamber 1", xlab = "Time (s)", ylab = bquote("Temperature (" ~ C^o ~ ")"), col = "#0082FF", cex=0.8)
      plot(test.CH2$Temp~test.CH2$Time, main="Chamber 2", xlab = "Time (s)", ylab = bquote("Temperature (" ~ C^o ~ ")"), col = "#0082FF", cex=0.8)
      plot(test.CH3$Temp~test.CH3$Time, main="Chamber 3", xlab = "Time (s)", ylab = bquote("Temperature (" ~ C^o ~ ")"), col = "#0082FF", cex=0.8)
      plot(test.CH4$Temp~test.CH4$Time, main="Chamber 4", xlab = "Time (s)", ylab = bquote("Temperature (" ~ C^o ~ ")"), col = "#0082FF", cex=0.8)
    }
    else{
      "Please, choose: TRUE or FALSE"
    }

    if (plot.oxygen == FALSE){
    }
    else if(plot.oxygen == TRUE){
      par(mfrow=c(4,1), ask = T)
      plot(test.CH1$O2~test.CH1$Time, main="Chamber 1", xlab = "Time (s)", ylab = paste("DO (", info.data$DO.unit[1], "/L)", sep = ""), col = "#0082FF", cex=0.8)
      plot(test.CH2$O2~test.CH2$Time, main="Chamber 2", xlab = "Time (s)", ylab = paste("DO (", info.data$DO.unit[1], "/L)", sep = ""), col = "#0082FF", cex=0.8)
      plot(test.CH3$O2~test.CH3$Time, main="Chamber 3", xlab = "Time (s)", ylab = paste("DO (", info.data$DO.unit[1], "/L)", sep = ""), col = "#0082FF", cex=0.8)
      plot(test.CH4$O2~test.CH4$Time, main="Chamber 4", xlab = "Time (s)", ylab = paste("DO (", info.data$DO.unit[1], "/L)", sep = ""), col = "#0082FF", cex=0.8)
    }
    else{
      "Please, choose: TRUE or FALSE"
    }

    return(test.data)
  }

  else if(n.chamber == 5){
    test.CH1<-subset(test.data, select=c(Temp.1, Ox.1))
    row.names(test.CH1)<-NULL
    names(test.CH1)<-c("Temp", "O2")
    test.CH1$Chamber.No<-as.factor(rep("CH1", dim(test.CH1)[1]))
    test.CH1$Test<-as.factor(rep("test", dim(test.CH1)[1]))
    test.CH1$Time<-1:dim(test.CH1)[1]
    test.CH1$Init.O2<-rep(mean(test.CH1$O2[1]), dim(test.CH1)[1])
    test.CH1<-subset(test.CH1, select=c(Chamber.No, Test, Time, Init.O2, Temp, O2))

    test.CH2<-subset(test.data, select=c(Temp.2, Ox.2))
    row.names(test.CH2)<-NULL
    names(test.CH2)<-c("Temp", "O2")
    test.CH2$Chamber.No<-as.factor(rep("CH2", dim(test.CH2)[1]))
    test.CH2$Test<-as.factor(rep("test", dim(test.CH2)[1]))
    test.CH2$Time<-1:dim(test.CH2)[1]
    test.CH2$Init.O2<-rep(mean(test.CH2$O2[1]), dim(test.CH2)[1])
    test.CH2<-subset(test.CH2, select=c(Chamber.No, Test, Time, Init.O2, Temp, O2))

    test.CH3<-subset(test.data, select=c(Temp.3, Ox.3))
    row.names(test.CH3)<-NULL
    names(test.CH3)<-c("Temp", "O2")
    test.CH3$Chamber.No<-as.factor(rep("CH3", dim(test.CH3)[1]))
    test.CH3$Test<-as.factor(rep("test", dim(test.CH3)[1]))
    test.CH3$Time<-1:dim(test.CH3)[1]
    test.CH3$Init.O2<-rep(mean(test.CH3$O2[1]), dim(test.CH3)[1])
    test.CH3<-subset(test.CH3, select=c(Chamber.No, Test, Time, Init.O2, Temp, O2))

    test.CH4<-subset(test.data, select=c(Temp.4, Ox.4))
    row.names(test.CH4)<-NULL
    names(test.CH4)<-c("Temp", "O2")
    test.CH4$Chamber.No<-as.factor(rep("CH4", dim(test.CH4)[1]))
    test.CH4$Test<-as.factor(rep("test", dim(test.CH4)[1]))
    test.CH4$Time<-1:dim(test.CH4)[1]
    test.CH4$Init.O2<-rep(mean(test.CH4$O2[1]), dim(test.CH4)[1])
    test.CH4<-subset(test.CH4, select=c(Chamber.No, Test, Time, Init.O2, Temp, O2))

    test.CH5<-subset(test.data, select=c(Temp.5, Ox.5))
    row.names(test.CH5)<-NULL
    names(test.CH5)<-c("Temp", "O2")
    test.CH5$Chamber.No<-as.factor(rep("CH5", dim(test.CH5)[1]))
    test.CH5$Test<-as.factor(rep("test", dim(test.CH5)[1]))
    test.CH5$Time<-1:dim(test.CH5)[1]
    test.CH5$Init.O2<-rep(mean(test.CH5$O2[1]), dim(test.CH5)[1])
    test.CH5<-subset(test.CH5, select=c(Chamber.No, Test, Time, Init.O2, Temp, O2))

    test.data<-rbind(test.CH1, test.CH2, test.CH3, test.CH4, test.CH5)
    test.data$delta.O2<-test.data$O2 - test.data$Init.O2

    if (plot.temperature == FALSE){
    }
    else if(plot.temperature == TRUE){
      par(mfrow=c(4,1), ask = T)
      plot(test.CH1$Temp~test.CH1$Time, main="Chamber 1", xlab = "Time (s)", ylab = bquote("Temperature (" ~ C^o ~ ")"), col = "#0082FF", cex=0.8)
      plot(test.CH2$Temp~test.CH2$Time, main="Chamber 2", xlab = "Time (s)", ylab = bquote("Temperature (" ~ C^o ~ ")"), col = "#0082FF", cex=0.8)
      plot(test.CH3$Temp~test.CH3$Time, main="Chamber 3", xlab = "Time (s)", ylab = bquote("Temperature (" ~ C^o ~ ")"), col = "#0082FF", cex=0.8)
      plot(test.CH4$Temp~test.CH4$Time, main="Chamber 4", xlab = "Time (s)", ylab = bquote("Temperature (" ~ C^o ~ ")"), col = "#0082FF", cex=0.8)
      plot(test.CH5$Temp~test.CH5$Time, main="Chamber 5", xlab = "Time (s)", ylab = bquote("Temperature (" ~ C^o ~ ")"), col = "#0082FF", cex=0.8)
    }
    else{
      "Please, choose: TRUE or FALSE"
    }

    if (plot.oxygen == FALSE){
    }
    else if(plot.oxygen == TRUE){
      par(mfrow=c(4,1), ask = T)
      plot(test.CH1$O2~test.CH1$Time, main="Chamber 1", xlab = "Time (s)", ylab = paste("DO (", info.data$DO.unit[1], "/L)", sep = ""), col = "#0082FF", cex=0.8)
      plot(test.CH2$O2~test.CH2$Time, main="Chamber 2", xlab = "Time (s)", ylab = paste("DO (", info.data$DO.unit[1], "/L)", sep = ""), col = "#0082FF", cex=0.8)
      plot(test.CH3$O2~test.CH3$Time, main="Chamber 3", xlab = "Time (s)", ylab = paste("DO (", info.data$DO.unit[1], "/L)", sep = ""), col = "#0082FF", cex=0.8)
      plot(test.CH4$O2~test.CH4$Time, main="Chamber 4", xlab = "Time (s)", ylab = paste("DO (", info.data$DO.unit[1], "/L)", sep = ""), col = "#0082FF", cex=0.8)
      plot(test.CH5$O2~test.CH5$Time, main="Chamber 5", xlab = "Time (s)", ylab = paste("DO (", info.data$DO.unit[1], "/L)", sep = ""), col = "#0082FF", cex=0.8)
    }
    else{
      "Please, choose: TRUE or FALSE"
    }

    return(test.data)
  }

  else if(n.chamber == 6){
    test.CH1<-subset(test.data, select=c(Temp.1, Ox.1))
    row.names(test.CH1)<-NULL
    names(test.CH1)<-c("Temp", "O2")
    test.CH1$Chamber.No<-as.factor(rep("CH1", dim(test.CH1)[1]))
    test.CH1$Test<-as.factor(rep("test", dim(test.CH1)[1]))
    test.CH1$Time<-1:dim(test.CH1)[1]
    test.CH1$Init.O2<-rep(mean(test.CH1$O2[1]), dim(test.CH1)[1])
    test.CH1<-subset(test.CH1, select=c(Chamber.No, Test, Time, Init.O2, Temp, O2))

    test.CH2<-subset(test.data, select=c(Temp.2, Ox.2))
    row.names(test.CH2)<-NULL
    names(test.CH2)<-c("Temp", "O2")
    test.CH2$Chamber.No<-as.factor(rep("CH2", dim(test.CH2)[1]))
    test.CH2$Test<-as.factor(rep("test", dim(test.CH2)[1]))
    test.CH2$Time<-1:dim(test.CH2)[1]
    test.CH2$Init.O2<-rep(mean(test.CH2$O2[1]), dim(test.CH2)[1])
    test.CH2<-subset(test.CH2, select=c(Chamber.No, Test, Time, Init.O2, Temp, O2))

    test.CH3<-subset(test.data, select=c(Temp.3, Ox.3))
    row.names(test.CH3)<-NULL
    names(test.CH3)<-c("Temp", "O2")
    test.CH3$Chamber.No<-as.factor(rep("CH3", dim(test.CH3)[1]))
    test.CH3$Test<-as.factor(rep("test", dim(test.CH3)[1]))
    test.CH3$Time<-1:dim(test.CH3)[1]
    test.CH3$Init.O2<-rep(mean(test.CH3$O2[1]), dim(test.CH3)[1])
    test.CH3<-subset(test.CH3, select=c(Chamber.No, Test, Time, Init.O2, Temp, O2))

    test.CH4<-subset(test.data, select=c(Temp.4, Ox.4))
    row.names(test.CH4)<-NULL
    names(test.CH4)<-c("Temp", "O2")
    test.CH4$Chamber.No<-as.factor(rep("CH4", dim(test.CH4)[1]))
    test.CH4$Test<-as.factor(rep("test", dim(test.CH4)[1]))
    test.CH4$Time<-1:dim(test.CH4)[1]
    test.CH4$Init.O2<-rep(mean(test.CH4$O2[1]), dim(test.CH4)[1])
    test.CH4<-subset(test.CH4, select=c(Chamber.No, Test, Time, Init.O2, Temp, O2))

    test.CH5<-subset(test.data, select=c(Temp.5, Ox.5))
    row.names(test.CH5)<-NULL
    names(test.CH5)<-c("Temp", "O2")
    test.CH5$Chamber.No<-as.factor(rep("CH5", dim(test.CH5)[1]))
    test.CH5$Test<-as.factor(rep("test", dim(test.CH5)[1]))
    test.CH5$Time<-1:dim(test.CH5)[1]
    test.CH5$Init.O2<-rep(mean(test.CH5$O2[1]), dim(test.CH5)[1])
    test.CH5<-subset(test.CH5, select=c(Chamber.No, Test, Time, Init.O2, Temp, O2))

    test.CH6<-subset(test.data, select=c(Temp.6, Ox.6))
    row.names(test.CH6)<-NULL
    names(test.CH6)<-c("Temp", "O2")
    test.CH6$Chamber.No<-as.factor(rep("CH6", dim(test.CH6)[1]))
    test.CH6$Test<-as.factor(rep("test", dim(test.CH6)[1]))
    test.CH6$Time<-1:dim(test.CH6)[1]
    test.CH6$Init.O2<-rep(mean(test.CH6$O2[1]), dim(test.CH6)[1])
    test.CH6<-subset(test.CH6, select=c(Chamber.No, Test, Time, Init.O2, Temp, O2))

    test.data<-rbind(test.CH1, test.CH2, test.CH3, test.CH4, test.CH5, test.CH6)
    test.data$delta.O2<-test.data$O2 - test.data$Init.O2

    if (plot.temperature == FALSE){
    }
    else if(plot.temperature == TRUE){
      par(mfrow=c(4,1), ask = T)
      plot(test.CH1$Temp~test.CH1$Time, main="Chamber 1", xlab = "Time (s)", ylab = bquote("Temperature (" ~ C^o ~ ")"), col = "#0082FF", cex=0.8)
      plot(test.CH2$Temp~test.CH2$Time, main="Chamber 2", xlab = "Time (s)", ylab = bquote("Temperature (" ~ C^o ~ ")"), col = "#0082FF", cex=0.8)
      plot(test.CH3$Temp~test.CH3$Time, main="Chamber 3", xlab = "Time (s)", ylab = bquote("Temperature (" ~ C^o ~ ")"), col = "#0082FF", cex=0.8)
      plot(test.CH4$Temp~test.CH4$Time, main="Chamber 4", xlab = "Time (s)", ylab = bquote("Temperature (" ~ C^o ~ ")"), col = "#0082FF", cex=0.8)
      plot(test.CH5$Temp~test.CH5$Time, main="Chamber 5", xlab = "Time (s)", ylab = bquote("Temperature (" ~ C^o ~ ")"), col = "#0082FF", cex=0.8)
      plot(test.CH6$Temp~test.CH6$Time, main="Chamber 6", xlab = "Time (s)", ylab = bquote("Temperature (" ~ C^o ~ ")"), col = "#0082FF", cex=0.8)
    }
    else{
      "Please, choose: TRUE or FALSE"
    }

    if (plot.oxygen == FALSE){
    }
    else if(plot.oxygen == TRUE){
      par(mfrow=c(4,1), ask = T)
      plot(test.CH1$O2~test.CH1$Time, main="Chamber 1", xlab = "Time (s)", ylab = paste("DO (", info.data$DO.unit[1], "/L)", sep = ""), col = "#0082FF", cex=0.8)
      plot(test.CH2$O2~test.CH2$Time, main="Chamber 2", xlab = "Time (s)", ylab = paste("DO (", info.data$DO.unit[1], "/L)", sep = ""), col = "#0082FF", cex=0.8)
      plot(test.CH3$O2~test.CH3$Time, main="Chamber 3", xlab = "Time (s)", ylab = paste("DO (", info.data$DO.unit[1], "/L)", sep = ""), col = "#0082FF", cex=0.8)
      plot(test.CH4$O2~test.CH4$Time, main="Chamber 4", xlab = "Time (s)", ylab = paste("DO (", info.data$DO.unit[1], "/L)", sep = ""), col = "#0082FF", cex=0.8)
      plot(test.CH5$O2~test.CH5$Time, main="Chamber 5", xlab = "Time (s)", ylab = paste("DO (", info.data$DO.unit[1], "/L)", sep = ""), col = "#0082FF", cex=0.8)
      plot(test.CH6$O2~test.CH6$Time, main="Chamber 6", xlab = "Time (s)", ylab = paste("DO (", info.data$DO.unit[1], "/L)", sep = ""), col = "#0082FF", cex=0.8)
    }
    else{
      "Please, choose: TRUE or FALSE"
    }

    return(test.data)
  }


  else if(n.chamber == 7){
    test.CH1<-subset(test.data, select=c(Temp.1, Ox.1))
    row.names(test.CH1)<-NULL
    names(test.CH1)<-c("Temp", "O2")
    test.CH1$Chamber.No<-as.factor(rep("CH1", dim(test.CH1)[1]))
    test.CH1$Test<-as.factor(rep("test", dim(test.CH1)[1]))
    test.CH1$Time<-1:dim(test.CH1)[1]
    test.CH1$Init.O2<-rep(mean(test.CH1$O2[1]), dim(test.CH1)[1])
    test.CH1<-subset(test.CH1, select=c(Chamber.No, Test, Time, Init.O2, Temp, O2))

    test.CH2<-subset(test.data, select=c(Temp.2, Ox.2))
    row.names(test.CH2)<-NULL
    names(test.CH2)<-c("Temp", "O2")
    test.CH2$Chamber.No<-as.factor(rep("CH2", dim(test.CH2)[1]))
    test.CH2$Test<-as.factor(rep("test", dim(test.CH2)[1]))
    test.CH2$Time<-1:dim(test.CH2)[1]
    test.CH2$Init.O2<-rep(mean(test.CH2$O2[1]), dim(test.CH2)[1])
    test.CH2<-subset(test.CH2, select=c(Chamber.No, Test, Time, Init.O2, Temp, O2))

    test.CH3<-subset(test.data, select=c(Temp.3, Ox.3))
    row.names(test.CH3)<-NULL
    names(test.CH3)<-c("Temp", "O2")
    test.CH3$Chamber.No<-as.factor(rep("CH3", dim(test.CH3)[1]))
    test.CH3$Test<-as.factor(rep("test", dim(test.CH3)[1]))
    test.CH3$Time<-1:dim(test.CH3)[1]
    test.CH3$Init.O2<-rep(mean(test.CH3$O2[1]), dim(test.CH3)[1])
    test.CH3<-subset(test.CH3, select=c(Chamber.No, Test, Time, Init.O2, Temp, O2))

    test.CH4<-subset(test.data, select=c(Temp.4, Ox.4))
    row.names(test.CH4)<-NULL
    names(test.CH4)<-c("Temp", "O2")
    test.CH4$Chamber.No<-as.factor(rep("CH4", dim(test.CH4)[1]))
    test.CH4$Test<-as.factor(rep("test", dim(test.CH4)[1]))
    test.CH4$Time<-1:dim(test.CH4)[1]
    test.CH4$Init.O2<-rep(mean(test.CH4$O2[1]), dim(test.CH4)[1])
    test.CH4<-subset(test.CH4, select=c(Chamber.No, Test, Time, Init.O2, Temp, O2))

    test.CH5<-subset(test.data, select=c(Temp.5, Ox.5))
    row.names(test.CH5)<-NULL
    names(test.CH5)<-c("Temp", "O2")
    test.CH5$Chamber.No<-as.factor(rep("CH5", dim(test.CH5)[1]))
    test.CH5$Test<-as.factor(rep("test", dim(test.CH5)[1]))
    test.CH5$Time<-1:dim(test.CH5)[1]
    test.CH5$Init.O2<-rep(mean(test.CH5$O2[1]), dim(test.CH5)[1])
    test.CH5<-subset(test.CH5, select=c(Chamber.No, Test, Time, Init.O2, Temp, O2))

    test.CH6<-subset(test.data, select=c(Temp.6, Ox.6))
    row.names(test.CH6)<-NULL
    names(test.CH6)<-c("Temp", "O2")
    test.CH6$Chamber.No<-as.factor(rep("CH6", dim(test.CH6)[1]))
    test.CH6$Test<-as.factor(rep("test", dim(test.CH6)[1]))
    test.CH6$Time<-1:dim(test.CH6)[1]
    test.CH6$Init.O2<-rep(mean(test.CH6$O2[1]), dim(test.CH6)[1])
    test.CH6<-subset(test.CH6, select=c(Chamber.No, Test, Time, Init.O2, Temp, O2))

    test.CH7<-subset(test.data, select=c(Temp.7, Ox.7))
    row.names(test.CH7)<-NULL
    names(test.CH7)<-c("Temp", "O2")
    test.CH7$Chamber.No<-as.factor(rep("CH7", dim(test.CH7)[1]))
    test.CH7$Test<-as.factor(rep("test", dim(test.CH7)[1]))
    test.CH7$Time<-1:dim(test.CH7)[1]
    test.CH7$Init.O2<-rep(mean(test.CH7$O2[1]), dim(test.CH7)[1])
    test.CH7<-subset(test.CH7, select=c(Chamber.No, Test, Time, Init.O2, Temp, O2))

    test.data<-rbind(test.CH1, test.CH2, test.CH3, test.CH4, test.CH5, test.CH6, test.CH7)
    test.data$delta.O2<-test.data$O2 - test.data$Init.O2

    if (plot.temperature == FALSE){
    }
    else if(plot.temperature == TRUE){
      par(mfrow=c(4,1), ask = T)
      plot(test.CH1$Temp~test.CH1$Time, main="Chamber 1", xlab = "Time (s)", ylab = bquote("Temperature (" ~ C^o ~ ")"), col = "#0082FF", cex=0.8)
      plot(test.CH2$Temp~test.CH2$Time, main="Chamber 2", xlab = "Time (s)", ylab = bquote("Temperature (" ~ C^o ~ ")"), col = "#0082FF", cex=0.8)
      plot(test.CH3$Temp~test.CH3$Time, main="Chamber 3", xlab = "Time (s)", ylab = bquote("Temperature (" ~ C^o ~ ")"), col = "#0082FF", cex=0.8)
      plot(test.CH4$Temp~test.CH4$Time, main="Chamber 4", xlab = "Time (s)", ylab = bquote("Temperature (" ~ C^o ~ ")"), col = "#0082FF", cex=0.8)
      plot(test.CH5$Temp~test.CH5$Time, main="Chamber 5", xlab = "Time (s)", ylab = bquote("Temperature (" ~ C^o ~ ")"), col = "#0082FF", cex=0.8)
      plot(test.CH6$Temp~test.CH6$Time, main="Chamber 6", xlab = "Time (s)", ylab = bquote("Temperature (" ~ C^o ~ ")"), col = "#0082FF", cex=0.8)
      plot(test.CH7$Temp~test.CH7$Time, main="Chamber 7", xlab = "Time (s)", ylab = bquote("Temperature (" ~ C^o ~ ")"), col = "#0082FF", cex=0.8)
    }
    else{
      "Please, choose: TRUE or FALSE"
    }

    if (plot.oxygen == FALSE){
    }
    else if(plot.oxygen == TRUE){
      par(mfrow=c(4,1), ask = T)
      plot(test.CH1$O2~test.CH1$Time, main="Chamber 1", xlab = "Time (s)", ylab = paste("DO (", info.data$DO.unit[1], "/L)", sep = ""), col = "#0082FF", cex=0.8)
      plot(test.CH2$O2~test.CH2$Time, main="Chamber 2", xlab = "Time (s)", ylab = paste("DO (", info.data$DO.unit[1], "/L)", sep = ""), col = "#0082FF", cex=0.8)
      plot(test.CH3$O2~test.CH3$Time, main="Chamber 3", xlab = "Time (s)", ylab = paste("DO (", info.data$DO.unit[1], "/L)", sep = ""), col = "#0082FF", cex=0.8)
      plot(test.CH4$O2~test.CH4$Time, main="Chamber 4", xlab = "Time (s)", ylab = paste("DO (", info.data$DO.unit[1], "/L)", sep = ""), col = "#0082FF", cex=0.8)
      plot(test.CH5$O2~test.CH5$Time, main="Chamber 5", xlab = "Time (s)", ylab = paste("DO (", info.data$DO.unit[1], "/L)", sep = ""), col = "#0082FF", cex=0.8)
      plot(test.CH6$O2~test.CH6$Time, main="Chamber 6", xlab = "Time (s)", ylab = paste("DO (", info.data$DO.unit[1], "/L)", sep = ""), col = "#0082FF", cex=0.8)
      plot(test.CH7$O2~test.CH7$Time, main="Chamber 7", xlab = "Time (s)", ylab = paste("DO (", info.data$DO.unit[1], "/L)", sep = ""), col = "#0082FF", cex=0.8)
    }
    else{
      "Please, choose: TRUE or FALSE"
    }

    return(test.data)
  }


  else if(n.chamber == 8){
    test.CH1<-subset(test.data, select=c(Temp.1, Ox.1))
    row.names(test.CH1)<-NULL
    names(test.CH1)<-c("Temp", "O2")
    test.CH1$Chamber.No<-as.factor(rep("CH1", dim(test.CH1)[1]))
    test.CH1$Test<-as.factor(rep("test", dim(test.CH1)[1]))
    test.CH1$Time<-1:dim(test.CH1)[1]
    test.CH1$Init.O2<-rep(mean(test.CH1$O2[1]), dim(test.CH1)[1])
    test.CH1<-subset(test.CH1, select=c(Chamber.No, Test, Time, Init.O2, Temp, O2))

    test.CH2<-subset(test.data, select=c(Temp.2, Ox.2))
    row.names(test.CH2)<-NULL
    names(test.CH2)<-c("Temp", "O2")
    test.CH2$Chamber.No<-as.factor(rep("CH2", dim(test.CH2)[1]))
    test.CH2$Test<-as.factor(rep("test", dim(test.CH2)[1]))
    test.CH2$Time<-1:dim(test.CH2)[1]
    test.CH2$Init.O2<-rep(mean(test.CH2$O2[1]), dim(test.CH2)[1])
    test.CH2<-subset(test.CH2, select=c(Chamber.No, Test, Time, Init.O2, Temp, O2))

    test.CH3<-subset(test.data, select=c(Temp.3, Ox.3))
    row.names(test.CH3)<-NULL
    names(test.CH3)<-c("Temp", "O2")
    test.CH3$Chamber.No<-as.factor(rep("CH3", dim(test.CH3)[1]))
    test.CH3$Test<-as.factor(rep("test", dim(test.CH3)[1]))
    test.CH3$Time<-1:dim(test.CH3)[1]
    test.CH3$Init.O2<-rep(mean(test.CH3$O2[1]), dim(test.CH3)[1])
    test.CH3<-subset(test.CH3, select=c(Chamber.No, Test, Time, Init.O2, Temp, O2))

    test.CH4<-subset(test.data, select=c(Temp.4, Ox.4))
    row.names(test.CH4)<-NULL
    names(test.CH4)<-c("Temp", "O2")
    test.CH4$Chamber.No<-as.factor(rep("CH4", dim(test.CH4)[1]))
    test.CH4$Test<-as.factor(rep("test", dim(test.CH4)[1]))
    test.CH4$Time<-1:dim(test.CH4)[1]
    test.CH4$Init.O2<-rep(mean(test.CH4$O2[1]), dim(test.CH4)[1])
    test.CH4<-subset(test.CH4, select=c(Chamber.No, Test, Time, Init.O2, Temp, O2))

    test.CH5<-subset(test.data, select=c(Temp.5, Ox.5))
    row.names(test.CH5)<-NULL
    names(test.CH5)<-c("Temp", "O2")
    test.CH5$Chamber.No<-as.factor(rep("CH5", dim(test.CH5)[1]))
    test.CH5$Test<-as.factor(rep("test", dim(test.CH5)[1]))
    test.CH5$Time<-1:dim(test.CH5)[1]
    test.CH5$Init.O2<-rep(mean(test.CH5$O2[1]), dim(test.CH5)[1])
    test.CH5<-subset(test.CH5, select=c(Chamber.No, Test, Time, Init.O2, Temp, O2))

    test.CH6<-subset(test.data, select=c(Temp.6, Ox.6))
    row.names(test.CH6)<-NULL
    names(test.CH6)<-c("Temp", "O2")
    test.CH6$Chamber.No<-as.factor(rep("CH6", dim(test.CH6)[1]))
    test.CH6$Test<-as.factor(rep("test", dim(test.CH6)[1]))
    test.CH6$Time<-1:dim(test.CH6)[1]
    test.CH6$Init.O2<-rep(mean(test.CH6$O2[1]), dim(test.CH6)[1])
    test.CH6<-subset(test.CH6, select=c(Chamber.No, Test, Time, Init.O2, Temp, O2))

    test.CH7<-subset(test.data, select=c(Temp.7, Ox.7))
    row.names(test.CH7)<-NULL
    names(test.CH7)<-c("Temp", "O2")
    test.CH7$Chamber.No<-as.factor(rep("CH7", dim(test.CH7)[1]))
    test.CH7$Test<-as.factor(rep("test", dim(test.CH7)[1]))
    test.CH7$Time<-1:dim(test.CH7)[1]
    test.CH7$Init.O2<-rep(mean(test.CH7$O2[1]), dim(test.CH7)[1])
    test.CH7<-subset(test.CH7, select=c(Chamber.No, Test, Time, Init.O2, Temp, O2))

    test.CH8<-subset(test.data, select=c(Temp.8, Ox.8))
    row.names(test.CH8)<-NULL
    names(test.CH8)<-c("Temp", "O2")
    test.CH8$Chamber.No<-as.factor(rep("CH8", dim(test.CH8)[1]))
    test.CH8$Test<-as.factor(rep("test", dim(test.CH8)[1]))
    test.CH8$Time<-1:dim(test.CH8)[1]
    test.CH8$Init.O2<-rep(mean(test.CH8$O2[1]), dim(test.CH8)[1])
    test.CH8<-subset(test.CH8, select=c(Chamber.No, Test, Time, Init.O2, Temp, O2))

    test.data<-rbind(test.CH1, test.CH2, test.CH3, test.CH4, test.CH5, test.CH6, test.CH7, test.CH8)
    test.data$delta.O2<-test.data$O2 - test.data$Init.O2

    if (plot.temperature == FALSE){
    }
    else if(plot.temperature == TRUE){
      par(mfrow=c(4,1), ask = T)
      plot(test.CH1$Temp~test.CH1$Time, main="Chamber 1", xlab = "Time (s)", ylab = bquote("Temperature (" ~ C^o ~ ")"), col = "#0082FF", cex=0.8)
      plot(test.CH2$Temp~test.CH2$Time, main="Chamber 2", xlab = "Time (s)", ylab = bquote("Temperature (" ~ C^o ~ ")"), col = "#0082FF", cex=0.8)
      plot(test.CH3$Temp~test.CH3$Time, main="Chamber 3", xlab = "Time (s)", ylab = bquote("Temperature (" ~ C^o ~ ")"), col = "#0082FF", cex=0.8)
      plot(test.CH4$Temp~test.CH4$Time, main="Chamber 4", xlab = "Time (s)", ylab = bquote("Temperature (" ~ C^o ~ ")"), col = "#0082FF", cex=0.8)
      plot(test.CH5$Temp~test.CH5$Time, main="Chamber 5", xlab = "Time (s)", ylab = bquote("Temperature (" ~ C^o ~ ")"), col = "#0082FF", cex=0.8)
      plot(test.CH6$Temp~test.CH6$Time, main="Chamber 6", xlab = "Time (s)", ylab = bquote("Temperature (" ~ C^o ~ ")"), col = "#0082FF", cex=0.8)
      plot(test.CH7$Temp~test.CH7$Time, main="Chamber 7", xlab = "Time (s)", ylab = bquote("Temperature (" ~ C^o ~ ")"), col = "#0082FF", cex=0.8)
      plot(test.CH8$Temp~test.CH8$Time, main="Chamber 8", xlab = "Time (s)", ylab = bquote("Temperature (" ~ C^o ~ ")"), col = "#0082FF", cex=0.8)
    }
    else{
      "Please, choose: TRUE or FALSE"
    }

    if (plot.oxygen == FALSE){
    }
    else if(plot.oxygen == TRUE){

      par(mfrow=c(4,1), ask = T)
      plot(test.CH1$O2~test.CH1$Time, main="Chamber 1", xlab = "Time (s)", ylab = paste("DO (", info.data$DO.unit[1], "/L)", sep = ""), col = "#0082FF", cex=0.8)
      plot(test.CH2$O2~test.CH2$Time, main="Chamber 2", xlab = "Time (s)", ylab = paste("DO (", info.data$DO.unit[1], "/L)", sep = ""), col = "#0082FF", cex=0.8)
      plot(test.CH3$O2~test.CH3$Time, main="Chamber 3", xlab = "Time (s)", ylab = paste("DO (", info.data$DO.unit[1], "/L)", sep = ""), col = "#0082FF", cex=0.8)
      plot(test.CH4$O2~test.CH4$Time, main="Chamber 4", xlab = "Time (s)", ylab = paste("DO (", info.data$DO.unit[1], "/L)", sep = ""), col = "#0082FF", cex=0.8)
      plot(test.CH5$O2~test.CH5$Time, main="Chamber 5", xlab = "Time (s)", ylab = paste("DO (", info.data$DO.unit[1], "/L)", sep = ""), col = "#0082FF", cex=0.8)
      plot(test.CH6$O2~test.CH6$Time, main="Chamber 6", xlab = "Time (s)", ylab = paste("DO (", info.data$DO.unit[1], "/L)", sep = ""), col = "#0082FF", cex=0.8)
      plot(test.CH7$O2~test.CH7$Time, main="Chamber 7", xlab = "Time (s)", ylab = paste("DO (", info.data$DO.unit[1], "/L)", sep = ""), col = "#0082FF", cex=0.8)
      plot(test.CH8$O2~test.CH8$Time, main="Chamber 8", xlab = "Time (s)", ylab = paste("DO (", info.data$DO.unit[1], "/L)", sep = ""), col = "#0082FF", cex=0.8)
    }
    else{
      "Please, choose: TRUE or FALSE"
    }

    return(test.data)
  }
}
