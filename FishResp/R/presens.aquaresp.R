#' Convert Respirometry Data from PreSens and AquaResp Software to the FishResp Format
#'
#' The function is used to convert raw data from 'OxyView' (\href{https://www.presens.de}{PreSens}) and a summary file from 'AquaResp' (\href{http://bioold.science.ku.dk/jfsteffensen/AquaResp.htm}{free software}) to 'FishResp' format. This function should be applied before usage of the functions \code{\link{import.test}} and \code{\link{import.meas}}. The output is a file containing raw respirometry data in the 'FishResp' format (see Details in \code{\link{import.test}} to read more information about the 'FishResp' format)
#'
#' @usage
#' presens.aquaresp(presens.file,
#'                  aquaresp.file,
#'                  fishresp.file,
#'                  n.chamber = c(1,2,3,4),
#'                  date.format = c("DMY", "MDY", "YMD"),
#'                  wait.phase = NA, measure.phase = NA)
#'
#' @param presens.file  the name of a file which contains raw data obtained from the 'OxyView' software (\href{https://www.presens.de}{PreSens})
#' @param aquaresp.file  the name of a file which contains summary data obtained from the 'AquaResp' software (\href{http://bioold.science.ku.dk/jfsteffensen/AquaResp.htm}{free software})
#' @param fishresp.file  the name of an exported file containing raw data in the 'FishResp' format
#' @param n.chamber  integer: the number of chambers used in an experiment (including empty ones)
#' @param date.format  string: date format (DMY, MDY or YMD) used in raw data obtained from the 'OxyView' software
#' @param wait.phase  integer: duration of the wait phase (in seconds), see the 'AquaResp' summary file (row #5)
#' @param measure.phase  integer: duration of the measure phase (in seconds), see the 'AquaResp' summary file (row #6)
#'
#' @return The function exports a file containing raw data in the 'FishResp' format
#' @importFrom chron chron
#' @importFrom utils read.table write.table
#'
#' @examples
#'
#' \dontrun{
#' presens.path.1 = system.file("extdata/presens/presens-ch1.txt",
#'                  package = "FishResp")
#' presens.path.2 = system.file("extdata/presens/presens-ch2.txt",
#'                  package = "FishResp")
#' presens.path.3 = system.file("extdata/presens/presens-ch3.txt",
#'                  package = "FishResp")
#' presens.path.4 = system.file("extdata/presens/presens-ch4.txt",
#'                  package = "FishResp")
#' aquaresp.path = system.file("extdata/presens/presens-aquaresp.txt",
#'                  package = "FishResp")
#'
#' presens.aquaresp(presens.file = c(presens.path.1, presens.path.2,
#'                                   presens.path.3, presens.path.4),
#'                  aquaresp.file = aquaresp.path,
#'                  fishresp.file = "fishresp.txt",
#'                  date.format = "DMY",
#'                  n.chamber = 4,
#'                  wait.phase = 60,
#'                  measure.phase = 240)
#' }
#' @export

presens.aquaresp <- function(presens.file,
                             aquaresp.file,
                             fishresp.file,
                             n.chamber = c(1,2,3,4),
                             date.format = c("DMY", "MDY", "YMD"),
                             wait.phase = NA, measure.phase = NA){

  # Loading PreSens data
  V1 <- V2 <- V3 <- V4 <- V5 <- V6 <- V7 <- V8 <- V9 <- V10 <- V11 <- V12 <- NULL

  if (n.chamber == 1){
    L <- readLines(presens.file, n = 100)
    if (grepl(";", L[99])){
      presens.data.ch1 <- read.table(presens.file, sep = ";", text = presens.file, fill = T, stringsAsFactors = F, encoding = Sys.setlocale('LC_ALL','C'), skip = grep("ogtime", readLines(presens.file)))
    }else{
      presens.data.ch1 <- read.table(presens.file, text = presens.file, fill = T, stringsAsFactors = F, encoding = Sys.setlocale('LC_ALL','C'), skip = grep("ogtime", readLines(presens.file)))
      }
    if(any(presens.data.ch1[1] == "Recalibration")){presens.data.ch1 <- presens.data.ch1[-1,]} #remove row containing potential "Recalibration" string
    presens.data.ch1 <- as.data.frame(presens.data.ch1, stringsAsFactors = F)
    presens.data.ch1 <- subset(presens.data.ch1, select=c(V1, V2, V7, V4))
    colnames(presens.data.ch1)<-c("Date", "Time", "Temp.1", "Ox.1")
    for(i in 3:ncol(presens.data.ch1)){presens.data.ch1[,i] = as.numeric(gsub(',','.', presens.data.ch1[,i]))}
    rownames(presens.data.ch1) <- seq(length=nrow(presens.data.ch1))
    presens.data.ch1$Temp.1 <- as.numeric(as.character(presens.data.ch1$Temp.1))
    presens.data.ch1$Ox.1 <- as.numeric(as.character(presens.data.ch1$Ox.1))
    presens.data.ch1$Date.Time <- paste(presens.data.ch1$Date, presens.data.ch1$Time, sep="/")
    presens.data.ch1$Phase <- "F"
    presens.data <- presens.data.ch1[,c(5,6,3,4)]
    }

  else if (n.chamber == 2){
    L <- readLines(presens.file[1], n = 100)
    if (grepl(";", L[99])){
      presens.data.ch1 <- read.table(presens.file[1], sep = ";", text = presens.file[1], fill = T, stringsAsFactors = F, encoding = Sys.setlocale('LC_ALL','C'), skip = grep("ogtime", readLines(presens.file[1])))
    }else{
      presens.data.ch1 <- read.table(presens.file[1], text = presens.file[1], fill = T, stringsAsFactors = F, encoding = Sys.setlocale('LC_ALL','C'), skip = grep("ogtime", readLines(presens.file[1])))
      }
    if(any(presens.data.ch1[1] == "Recalibration")){presens.data.ch1 <- presens.data.ch1[-1,]} #remove row containing potential "Recalibration" string
    presens.data.ch1 <- as.data.frame(presens.data.ch1, stringsAsFactors = F)
    presens.data.ch1 <- subset(presens.data.ch1, select=c(V1, V2, V7, V4))
    colnames(presens.data.ch1)<-c("Date", "Time", "Temp.1", "Ox.1")
    for(i in 3:ncol(presens.data.ch1)){presens.data.ch1[,i] = as.numeric(gsub(',','.', presens.data.ch1[,i]))}
    rownames(presens.data.ch1) <- seq(length=nrow(presens.data.ch1))
    presens.data.ch1$Temp.1 <- as.numeric(as.character(presens.data.ch1$Temp.1))
    presens.data.ch1$Ox.1 <- as.numeric(as.character(presens.data.ch1$Ox.1))
    presens.data.ch1$Date.Time <- paste(presens.data.ch1$Date, presens.data.ch1$Time, sep="/")
    presens.data.ch1$Phase <- "F"
    presens.data.ch1 <- presens.data.ch1[,c(5,6,3,4)]

    L <- readLines(presens.file[2], n = 100)
    if (grepl(";", L[99])){
      presens.data.ch2 <- read.table(presens.file[2], sep = ";", text = presens.file[2], fill = T, stringsAsFactors = F, encoding = Sys.setlocale('LC_ALL','C'), skip = grep("ogtime", readLines(presens.file[2])))
    }else{
      presens.data.ch2 <- read.table(presens.file[2], text = presens.file[2], fill = T, stringsAsFactors = F, encoding = Sys.setlocale('LC_ALL','C'), skip = grep("ogtime", readLines(presens.file[2])))
      }
    if(any(presens.data.ch2[1] == "Recalibration")){presens.data.ch2 <- presens.data.ch2[-1,]} #remove row containing potential "Recalibration" string
    presens.data.ch2 <- as.data.frame(presens.data.ch2, stringsAsFactors = F)
    presens.data.ch2 <- subset(presens.data.ch2, select=c(V1, V2, V7, V4))
    colnames(presens.data.ch2)<-c("Date", "Time", "Temp.2", "Ox.2")
    for(i in 3:ncol(presens.data.ch2)){presens.data.ch2[,i] = as.numeric(gsub(',','.', presens.data.ch2[,i]))}
    rownames(presens.data.ch2) <- seq(length=nrow(presens.data.ch2))
    presens.data.ch2$Temp.2 <- as.numeric(as.character(presens.data.ch2$Temp.2))
    presens.data.ch2$Ox.2 <- as.numeric(as.character(presens.data.ch2$Ox.2))
    presens.data.ch2$Date.Time <- paste(presens.data.ch2$Date, presens.data.ch2$Time, sep="/")
    presens.data.ch2$Phase <- "F"
    presens.data.ch2 <- presens.data.ch2[,c(5,6,3,4)]

    presens.data <- merge(presens.data.ch1, presens.data.ch2)
    }

  else if (n.chamber == 3){
    L <- readLines(presens.file[1], n = 100)
    if (grepl(";", L[99])){
      presens.data.ch1 <- read.table(presens.file[1], sep = ";", text = presens.file[1], fill = T, stringsAsFactors = F, encoding = Sys.setlocale('LC_ALL','C'), skip = grep("ogtime", readLines(presens.file[1])))
    }else{
      presens.data.ch1 <- read.table(presens.file[1], text = presens.file[1], fill = T, stringsAsFactors = F, encoding = Sys.setlocale('LC_ALL','C'), skip = grep("ogtime", readLines(presens.file[1])))
      }
    if(any(presens.data.ch1[1] == "Recalibration")){presens.data.ch1 <- presens.data.ch1[-1,]} #remove row containing potential "Recalibration" string
    presens.data.ch1 <- as.data.frame(presens.data.ch1, stringsAsFactors = F)
    presens.data.ch1 <- subset(presens.data.ch1, select=c(V1, V2, V7, V4))
    colnames(presens.data.ch1)<-c("Date", "Time", "Temp.1", "Ox.1")
    for(i in 3:ncol(presens.data.ch1)){presens.data.ch1[,i] = as.numeric(gsub(',','.', presens.data.ch1[,i]))}
    rownames(presens.data.ch1) <- seq(length=nrow(presens.data.ch1))
    presens.data.ch1$Temp.1 <- as.numeric(as.character(presens.data.ch1$Temp.1))
    presens.data.ch1$Ox.1 <- as.numeric(as.character(presens.data.ch1$Ox.1))
    presens.data.ch1$Date.Time <- paste(presens.data.ch1$Date, presens.data.ch1$Time, sep="/")
    presens.data.ch1$Phase <- "F"
    presens.data.ch1 <- presens.data.ch1[,c(5,6,3,4)]

    L <- readLines(presens.file[2], n = 100)
    if (grepl(";", L[99])){
      presens.data.ch2 <- read.table(presens.file[2], sep = ";", text = presens.file[2], fill = T, stringsAsFactors = F, encoding = Sys.setlocale('LC_ALL','C'), skip = grep("ogtime", readLines(presens.file[2])))
    }else{
      presens.data.ch2 <- read.table(presens.file[2], text = presens.file[2], fill = T, stringsAsFactors = F, encoding = Sys.setlocale('LC_ALL','C'), skip = grep("ogtime", readLines(presens.file[2])))
      }
    if(any(presens.data.ch2[1] == "Recalibration")){presens.data.ch2 <- presens.data.ch2[-1,]} #remove row containing potential "Recalibration" string
    presens.data.ch2 <- as.data.frame(presens.data.ch2, stringsAsFactors = F)
    presens.data.ch2 <- subset(presens.data.ch2, select=c(V1, V2, V7, V4))
    colnames(presens.data.ch2)<-c("Date", "Time", "Temp.2", "Ox.2")
    for(i in 3:ncol(presens.data.ch2)){presens.data.ch2[,i] = as.numeric(gsub(',','.', presens.data.ch2[,i]))}
    rownames(presens.data.ch2) <- seq(length=nrow(presens.data.ch2))
    presens.data.ch2$Temp.2 <- as.numeric(as.character(presens.data.ch2$Temp.2))
    presens.data.ch2$Ox.2 <- as.numeric(as.character(presens.data.ch2$Ox.2))
    presens.data.ch2$Date.Time <- paste(presens.data.ch2$Date, presens.data.ch2$Time, sep="/")
    presens.data.ch2$Phase <- "F"
    presens.data.ch2 <- presens.data.ch2[,c(5,6,3,4)]

    L <- readLines(presens.file[3], n = 100)
    if (grepl(";", L[99])){
      presens.data.ch3 <- read.table(presens.file[3], sep = ";", text = presens.file[3], fill = T, stringsAsFactors = F, encoding = Sys.setlocale('LC_ALL','C'), skip = grep("ogtime", readLines(presens.file[3])))
    }else{
      presens.data.ch3 <- read.table(presens.file[3], text = presens.file[3], fill = T, stringsAsFactors = F, encoding = Sys.setlocale('LC_ALL','C'), skip = grep("ogtime", readLines(presens.file[3])))
      }
    if(any(presens.data.ch3[1] == "Recalibration")){presens.data.ch3 <- presens.data.ch3[-1,]} #remove row containing potential "Recalibration" string
    presens.data.ch3 <- as.data.frame(presens.data.ch3, stringsAsFactors = F)
    presens.data.ch3 <- subset(presens.data.ch3, select=c(V1, V2, V7, V4))
    colnames(presens.data.ch3)<-c("Date", "Time", "Temp.3", "Ox.3")
    for(i in 3:ncol(presens.data.ch3)){presens.data.ch3[,i] = as.numeric(gsub(',','.', presens.data.ch3[,i]))}
    rownames(presens.data.ch3) <- seq(length=nrow(presens.data.ch3))
    presens.data.ch3$Temp.3 <- as.numeric(as.character(presens.data.ch3$Temp.3))
    presens.data.ch3$Ox.3 <- as.numeric(as.character(presens.data.ch3$Ox.3))
    presens.data.ch3$Date.Time <- paste(presens.data.ch3$Date, presens.data.ch3$Time, sep="/")
    presens.data.ch3$Phase <- "F"
    presens.data.ch3 <- presens.data.ch3[,c(5,6,3,4)]

    presens.data.ch12 <- merge(presens.data.ch1, presens.data.ch2)
    presens.data <- merge(presens.data.ch12, presens.data.ch3)
    }

  else if (n.chamber == 4){
    L <- readLines(presens.file[1], n = 100)
    if (grepl(";", L[99])){
      presens.data.ch1 <- read.table(presens.file[1], sep = ";", text = presens.file[1], fill = T, stringsAsFactors = F, encoding = Sys.setlocale('LC_ALL','C'), skip = grep("ogtime", readLines(presens.file[1])))
    }else{
      presens.data.ch1 <- read.table(presens.file[1], text = presens.file[1], fill = T, stringsAsFactors = F, encoding = Sys.setlocale('LC_ALL','C'), skip = grep("ogtime", readLines(presens.file[1])))
      }
    if(any(presens.data.ch1[1] == "Recalibration")){presens.data.ch1 <- presens.data.ch1[-1,]} #remove row containing potential "Recalibration" string
    presens.data.ch1 <- as.data.frame(presens.data.ch1, stringsAsFactors = F)
    presens.data.ch1 <- subset(presens.data.ch1, select=c(V1, V2, V7, V4))
    colnames(presens.data.ch1)<-c("Date", "Time", "Temp.1", "Ox.1")
    for(i in 3:ncol(presens.data.ch1)){presens.data.ch1[,i] = as.numeric(gsub(',','.', presens.data.ch1[,i]))}
    rownames(presens.data.ch1) <- seq(length=nrow(presens.data.ch1))
    presens.data.ch1$Temp.1 <- as.numeric(as.character(presens.data.ch1$Temp.1))
    presens.data.ch1$Ox.1 <- as.numeric(as.character(presens.data.ch1$Ox.1))
    presens.data.ch1$Date.Time <- paste(presens.data.ch1$Date, presens.data.ch1$Time, sep="/")
    presens.data.ch1$Phase <- "F"
    presens.data.ch1 <- presens.data.ch1[,c(5,6,3,4)]

    L <- readLines(presens.file[2], n = 100)
    if (grepl(";", L[99])){
      presens.data.ch2 <- read.table(presens.file[2], sep = ";", text = presens.file[2], fill = T, stringsAsFactors = F, encoding = Sys.setlocale('LC_ALL','C'), skip = grep("ogtime", readLines(presens.file[2])))
    }else{
      presens.data.ch2 <- read.table(presens.file[2], text = presens.file[2], fill = T, stringsAsFactors = F, encoding = Sys.setlocale('LC_ALL','C'), skip = grep("ogtime", readLines(presens.file[2])))
      }
    if(any(presens.data.ch2[1] == "Recalibration")){presens.data.ch2 <- presens.data.ch2[-1,]} #remove row containing potential "Recalibration" string
    presens.data.ch2 <- as.data.frame(presens.data.ch2, stringsAsFactors = F)
    presens.data.ch2 <- subset(presens.data.ch2, select=c(V1, V2, V7, V4))
    colnames(presens.data.ch2)<-c("Date", "Time", "Temp.2", "Ox.2")
    for(i in 3:ncol(presens.data.ch2)){presens.data.ch2[,i] = as.numeric(gsub(',','.', presens.data.ch2[,i]))}
    rownames(presens.data.ch2) <- seq(length=nrow(presens.data.ch2))
    presens.data.ch2$Temp.2 <- as.numeric(as.character(presens.data.ch2$Temp.2))
    presens.data.ch2$Ox.2 <- as.numeric(as.character(presens.data.ch2$Ox.2))
    presens.data.ch2$Date.Time <- paste(presens.data.ch2$Date, presens.data.ch2$Time, sep="/")
    presens.data.ch2$Phase <- "F"
    presens.data.ch2 <- presens.data.ch2[,c(5,6,3,4)]

    L <- readLines(presens.file[3], n = 100)
    if (grepl(";", L[99])){
      presens.data.ch3 <- read.table(presens.file[3], sep = ";", text = presens.file[3], fill = T, stringsAsFactors = F, encoding = Sys.setlocale('LC_ALL','C'), skip = grep("ogtime", readLines(presens.file[3])))
    }else{
      presens.data.ch3 <- read.table(presens.file[3], text = presens.file[3], fill = T, stringsAsFactors = F, encoding = Sys.setlocale('LC_ALL','C'), skip = grep("ogtime", readLines(presens.file[3])))
      }
    if(any(presens.data.ch3[1] == "Recalibration")){presens.data.ch3 <- presens.data.ch3[-1,]} #remove row containing potential "Recalibration" string
    presens.data.ch3 <- as.data.frame(presens.data.ch3, stringsAsFactors = F)
    presens.data.ch3 <- subset(presens.data.ch3, select=c(V1, V2, V7, V4))
    colnames(presens.data.ch3)<-c("Date", "Time", "Temp.3", "Ox.3")
    for(i in 3:ncol(presens.data.ch3)){presens.data.ch3[,i] = as.numeric(gsub(',','.', presens.data.ch3[,i]))}
    rownames(presens.data.ch3) <- seq(length=nrow(presens.data.ch3))
    presens.data.ch3$Temp.3 <- as.numeric(as.character(presens.data.ch3$Temp.3))
    presens.data.ch3$Ox.3 <- as.numeric(as.character(presens.data.ch3$Ox.3))
    presens.data.ch3$Date.Time <- paste(presens.data.ch3$Date, presens.data.ch3$Time, sep="/")
    presens.data.ch3$Phase <- "F"
    presens.data.ch3 <- presens.data.ch3[,c(5,6,3,4)]

    L <- readLines(presens.file[4], n = 100)
    if (grepl(";", L[99])){
      presens.data.ch4 <- read.table(presens.file[4], sep = ";", text = presens.file[4], fill = T, stringsAsFactors = F, encoding = Sys.setlocale('LC_ALL','C'), skip = grep("ogtime", readLines(presens.file[4])))
    }else{
      presens.data.ch4 <- read.table(presens.file[4], text = presens.file[4], fill = T, stringsAsFactors = F, encoding = Sys.setlocale('LC_ALL','C'), skip = grep("ogtime", readLines(presens.file[4])))
      }
    if(any(presens.data.ch4[1] == "Recalibration")){presens.data.ch4 <- presens.data.ch4[-1,]} #remove row containing potential "Recalibration" string
    presens.data.ch4 <- as.data.frame(presens.data.ch4, stringsAsFactors = F)
    presens.data.ch4 <- subset(presens.data.ch4, select=c(V1, V2, V7, V4))
    colnames(presens.data.ch4)<-c("Date", "Time", "Temp.4", "Ox.4")
    for(i in 3:ncol(presens.data.ch4)){presens.data.ch4[,i] = as.numeric(gsub(',','.', presens.data.ch4[,i]))}
    rownames(presens.data.ch4) <- seq(length=nrow(presens.data.ch4))
    presens.data.ch4$Temp.4 <- as.numeric(as.character(presens.data.ch4$Temp.4))
    presens.data.ch4$Ox.4 <- as.numeric(as.character(presens.data.ch4$Ox.4))
    presens.data.ch4$Date.Time <- paste(presens.data.ch4$Date, presens.data.ch4$Time, sep="/")
    presens.data.ch4$Phase <- "F"
    presens.data.ch4 <- presens.data.ch4[,c(5,6,3,4)]

    presens.data.ch12 <- merge(presens.data.ch1, presens.data.ch2)
    presens.data.ch123 <- merge(presens.data.ch12, presens.data.ch3)
    presens.data <- merge(presens.data.ch123, presens.data.ch4)
    head(presens.data)
    }

  else{
    print("Please, choose the number of chambers between 1 and 4")
    }

  if(any(date.format == "DMY")){
    # "." or "/" -> "/"
    presens.data$Date.Time <- gsub("[.]", "/", presens.data$Date.Time)
    # YY or YYYY
    if(nchar(presens.data$Date.Time[1]) == 17){
      presens.data$Date.Time<-strptime(as.character(presens.data$Date.Time), "%d/%m/%y/%H:%M:%S")
      }else{
      presens.data$Date.Time<-strptime(as.character(presens.data$Date.Time), "%d/%m/%Y/%H:%M:%S")
    }
    ts<-times(strftime(presens.data$Date.Time, "%H:%M:%S"))
    ds <- format(presens.data$Date.Time, format="%d/%m/%Y")
    presens.data$Date.Time <- paste(ds,ts, sep = "/")
    }

  else if(any(date.format == "MDY")){
    # "." or "/" -> "/"
    presens.data$Date.Time <- gsub("[.]", "/", presens.data$Date.Time)
    # YY or YYYY
    if(nchar(presens.data$Date.Time[1]) == 17){
      presens.data$Date.Time<-strptime(as.character(presens.data$Date.Time), "%m/%d/%y/%H:%M:%S")
      }else{
      presens.data$Date.Time<-strptime(as.character(presens.data$Date.Time), "%m/%d/%Y/%H:%M:%S")
    }
    ts<-times(strftime(presens.data$Date.Time, "%H:%M:%S"))
    ds <- format(presens.data$Date.Time, format="%m/%d/%Y")
    presens.data$Date.Time <- paste(ds,ts, sep = "/")
    }

  else if(any(date.format == "YMD")){
    # "." or "/" -> "/"
    presens.data$Date.Time <- gsub("[.]", "/", presens.data$Date.Time)
    # YY or YYYY
    if(nchar(presens.data$Date.Time[1]) == 17){
      presens.data$Date.Time<-strptime(as.character(presens.data$Date.Time), "%y/%m/%d/%H:%M:%S")
      }else{
      presens.data$Date.Time<-strptime(as.character(presens.data$Date.Time), "%Y/%m/%d/%H:%M:%S")
    }
    ts<-times(strftime(presens.data$Date.Time, "%H:%M:%S"))
    ds <- format(presens.data$Date.Time, format="%Y/%m/%d")
    presens.data$Date.Time <- paste(ds,ts, sep = "/")
    }

  else{
    print("Please, choose the date format: DMY, MDY or YMD, where D-day, M-month, Y-year")
    }


  # Loading AquaResp data
  aquaresp.data <- read.table(aquaresp.file, sep = ";", skip=16, header=F, strip.white=T)
  aquaresp.data <- subset(aquaresp.data, select=V1)
  names(aquaresp.data) <- "Date.Time"
  aquaresp.data$Date.Time<-strptime(as.character(aquaresp.data$Date.Time), "%Y-%m-%d %H:%M:%S")
  tms<-times(strftime(aquaresp.data$Date.Time, "%H:%M:%S"))

  if(any(date.format == "DMY")){
    dts <- format(aquaresp.data$Date.Time, format="%d/%m/%Y")
    x <- paste(dts,tms, sep = "/")
    }

  else if(any(date.format == "MDY")){
    dts <- format(aquaresp.data$Date.Time, format="%m/%d/%Y")
    x <- paste(dts,tms, sep = "/")
    }

  else if(any(date.format == "YMD")){
    dts <- format(aquaresp.data$Date.Time, format="%Y/%m/%d")
    x <- paste(dts,tms, sep = "/")
    }

  else{
    print("Please, choose the date format: DMY, MDY or YMD, where D-day, M-month, Y-year")
    }

  # Join datasets
  m <- 1 #counter

  for(i in x){
    a <- which(presens.data$Date.Time == i)
    if(length(a) == 0){
      a1 <- substring(i, 1, 18)
      a2 <- as.numeric(substring(i, 19))
      if(a2 >= 9){
        a2 <- a2 - 1
        }
      else{
        a2 <- a2 + 1
        }
      b <- paste(a1, a2, sep = "")
      a <- which(presens.data$Date.Time == b)
      }
    else{
      }
    if(length(presens.data$Date.Time) - a > measure.phase){
      presens.data$Phase[a:(a + (measure.phase-1))] <- paste("M", m, sep = "")
      }
    else{
      }
    if(a - wait.phase > 0){
      presens.data$Phase[(a - wait.phase):(a-1)] <- paste("W", m, sep = "")
      }
    else{
      }
      m = m + 1
    }

  write.table(presens.data, file = fishresp.file, sep = "\t", row.names = F)
  }
