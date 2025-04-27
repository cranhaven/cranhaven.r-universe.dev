#' Prepare Raw Respirometry Data
#'
#' This function is used for preparation of raw data in the FishResp format before the actual respirometry analysis. As namely, the function will create measurement points for each second (the required FishResp format) if the time interval between two measurement points is more than one second. In addition, low and high thresold for both dissolved oxygen and water temperature might be applied here. The measurement points beyond the threshold(s) will be transformed to NA (be careful if use the 'parallel' method for background respiration correction as it might transform DO or Temp to NA as well).
#'
#' @usage
#' prepare.data(import.file, export.file,
#'              date.format = c("DMY", "MDY", "YMD"),
#'              DO.low = NA, DO.high = NA,
#'              Temp.low = NA, Temp.high = NA)
#'
#' @param import.file  the name of a file with raw respirometry data which should be imported for raw data preparation
#' @param export.file  the name of a file with results of raw data preparation
#' @param date.format  string: date format (DMY, MDY or YMD), where D = day, M = month, Y - year.
#' @param DO.low  numeric: the low threshold defining a minimum accepted value for dissolved oxygen (DO)
#' @param DO.high  numeric: the high threshold defining a maximum accepted value for dissolved oxygen (DO)
#' @param Temp.low  numeric: the low threshold defining a minimum accepted value for water temperature (Temp)
#' @param Temp.high  numeric: the high threshold defining a maximum accepted value for water temperature (Temp)
#'
#' @return The function exports a data file with one second interval between measurement points and excluded data beyond the defined threshold(s)
#'
#' @importFrom stats na.omit
#' @importFrom chron chron times
#' @importFrom utils read.table write.table
#'
#' @examples
#' \dontrun{
#' amphipod.path = system.file("extdata/amphipod/amphipod.txt",
#'                  package = "FishResp")
#'
#' prepare.data(import.file = amphipod.path,
#'              export.file = "amphipod_corrected.txt",
#'              date.format = "DMY",
#'              DO.low = 0.5,
#'              DO.high = 12)
#' }
#' @export

prepare.data <- function(import.file, export.file,
                         date.format = c("DMY", "MDY", "YMD"),
                         DO.low = NA, DO.high = NA,
                         Temp.low = NA, Temp.high = NA){

  MR.data <- read.table(import.file, sep = "\t", header=T,
                        check.names=FALSE, strip.white=T)
  colnames(MR.data)[1]  <- "Date.Time"
  for(i in 3:ncol(MR.data)){MR.data[,i] = as.numeric(gsub(',','.', MR.data[,i]))}
  MR.data <- MR.data[!(is.na(MR.data$Date.Time) | MR.data$Date.Time==""), ]  # to remove empty lines

  # indexing the first  measurement point and the last one
  start <- MR.data$Date.Time[1]
  stop <- MR.data$Date.Time[length(MR.data$Date.Time)]

  if(any(date.format == "DMY")){
    Date.Time <- seq(
      from=as.POSIXct(start, "%d/%m/%Y/%H:%M:%S", tz="UTC"),
      to=as.POSIXct(stop, "%d/%m/%Y/%H:%M:%S", tz="UTC"),
      by="sec")
    Date.Time <- as.data.frame(Date.Time)

    MR.data$Date.Time<-strptime(as.character(MR.data[,1]), "%d/%m/%Y/%H:%M:%S", tz="UTC")
    MR.final <- merge(MR.data, Date.Time, by.x = "Date.Time", by.y = "Date.Time", all= TRUE)
    MR.final <- MR.final[!duplicated(MR.final$Date.Time),]

    ts <- format(MR.final$Date.Time, "%H:%M:%S")
    ds <- format(MR.final$Date.Time, format="%d/%m/%Y")
  }

  if(any(date.format == "MDY")){
    Date.Time <- seq(
      from=as.POSIXct(start, "%m/%d/%Y/%H:%M:%S", tz="UTC"),
      to=as.POSIXct(stop, "%m/%d/%Y/%H:%M:%S", tz="UTC"),
      by="sec")
    Date.Time <- as.data.frame(Date.Time)

    MR.data$Date.Time<-strptime(as.character(MR.data[,1]), "%m/%d/%Y/%H:%M:%S", tz="UTC")
    MR.final <- merge(MR.data, Date.Time, by.x = "Date.Time", by.y = "Date.Time", all= TRUE)
    MR.final <- MR.final[!duplicated(MR.final$Date.Time),]

    ts <- format(MR.final$Date.Time, "%H:%M:%S")
    ds <- format(MR.final$Date.Time, format="%m/%d/%Y")
  }

  if(any(date.format == "YMD")){
    Date.Time <- seq(
      from=as.POSIXct(start, "%Y/%m/%d/%H:%M:%S", tz="UTC"),
      to=as.POSIXct(stop, "%Y/%m/%d/%H:%M:%S", tz="UTC"),
      by="sec")
    Date.Time <- as.data.frame(Date.Time)

    MR.data$Date.Time<-strptime(as.character(MR.data[,1]), "%Y/%m/%d/%H:%M:%S", tz="UTC")
    MR.final <- merge(MR.data, Date.Time, by.x = "Date.Time", by.y = "Date.Time", all= TRUE)
    MR.final <- MR.final[!duplicated(MR.final$Date.Time),]

    ts <- format(MR.final$Date.Time, "%H:%M:%S")
    ds <- format(MR.final$Date.Time, format="%Y/%m/%d")
  }

  chron::chron(dates = ds, times = ts)
  MR.final$Date.Time <- paste(ds,ts, sep = "/")

  # Function to replace NA by a value found in the previous row for that column
  na.lomf <- function(x) {
    if (length(x) > 0L) {
      non.na.idx <- which(!is.na(x))
      if (is.na(x[1L])) {
        non.na.idx <- c(1L, non.na.idx)
      }
      rep.int(x[non.na.idx], diff(c(non.na.idx, length(x) + 1L)))
    }
  }

  # This 'for' loop repeats the the function 'na.lomf'
  # for all columns excluding 'Date.Time'
  for(i in 2:ncol(MR.final)){
    MR.final[,i] <- na.lomf(MR.final[,i])
  }
  MR.final <- na.omit(MR.final)

  # make data below a defined low threshold for dissolved oxygen as NA
  if(is.na(DO.low) != T){
    if(ncol(MR.final) == 4){
      MR.final[,4][MR.final[,4] <= DO.low] <- NA
    }
    else if(ncol(MR.final) == 6){
      MR.final[,c(4,6)][MR.final[,c(4,6)] <= DO.low] <- NA
    }
    else if(ncol(MR.final) == 8){
      MR.final[,c(4,6,8)][MR.final[,c(4,6,8)] <= DO.low] <- NA
    }
    else if(ncol(MR.final) == 10){
      MR.final[,c(4,6,8,10)][MR.final[,c(4,6,8,10)] <= DO.low] <- NA
    }
    else if(ncol(MR.final) == 12){
      MR.final[,c(4,6,8,10,12)][MR.final[,c(4,6,8,10,12)] <= DO.low] <- NA
    }
    else if(ncol(MR.final) == 14){
      MR.final[,c(4,6,8,10,12,14)][MR.final[,c(4,6,8,10,12,14)] <= DO.low] <- NA
    }
    else if(ncol(MR.final) == 16){
      MR.final[,c(4,6,8,10,12,14,16)][MR.final[,c(4,6,8,10,12,14,16)] <= DO.low] <- NA
    }
    else if(ncol(MR.final) == 18){
      MR.final[,c(4,6,8,10,12,14,16,18)][MR.final[,c(4,6,8,10,12,14,16,18)] <= DO.low] <- NA
    }else{
      print("The FishResp format of a table is not correct")
    }
  }else{
    print("No low threshold for dissolved oxygen (DO) has been set")
  }

  # make data above a defined high threshold for dissolved oxygen as NA
  if(is.na(DO.high) != T){
    if(ncol(MR.final) == 4){
      MR.final[,4][MR.final[,4] >= DO.high] <- NA
    }
    else if(ncol(MR.final) == 6){
      MR.final[,c(4,6)][MR.final[,c(4,6)] >= DO.high] <- NA
    }
    else if(ncol(MR.final) == 8){
      MR.final[,c(4,6,8)][MR.final[,c(4,6,8)] >= DO.high] <- NA
    }
    else if(ncol(MR.final) == 10){
      MR.final[,c(4,6,8,10)][MR.final[,c(4,6,8,10)] >= DO.high] <- NA
    }
    else if(ncol(MR.final) == 12){
      MR.final[,c(4,6,8,10,12)][MR.final[,c(4,6,8,10,12)] >= DO.high] <- NA
    }
    else if(ncol(MR.final) == 14){
      MR.final[,c(4,6,8,10,12,14)][MR.final[,c(4,6,8,10,12,14)] >= DO.high] <- NA
    }
    else if(ncol(MR.final) == 16){
      MR.final[,c(4,6,8,10,12,14,16)][MR.final[,c(4,6,8,10,12,14,16)] >= DO.high] <- NA
    }
    else if(ncol(MR.final) == 18){
      MR.final[,c(4,6,8,10,12,14,16,18)][MR.final[,c(4,6,8,10,12,14,16,18)] >= DO.high] <- NA
    }else{
      print("The FishResp format of a table is not correct")
    }
  }else{
    print("No high threshold for dissolved oxygen (DO) has been set")
  }

  # make data below a defined low threshold for water temperature as NA
  if(is.na(Temp.low) != T){
    if(ncol(MR.final) == 4){
      MR.final[,3][MR.final[,3] <= Temp.low] <- NA
    }
    else if(ncol(MR.final) == 6){
      MR.final[,c(3,5)][MR.final[,c(3,5)] <= Temp.low] <- NA
    }
    else if(ncol(MR.final) == 8){
      MR.final[,c(3,5,7)][MR.final[,c(3,5,7)] <= Temp.low] <- NA
    }
    else if(ncol(MR.final) == 10){
      MR.final[,c(3,5,7,9)][MR.final[,c(3,5,7,9)] <= Temp.low] <- NA
    }
    else if(ncol(MR.final) == 12){
      MR.final[,c(3,5,7,9,11)][MR.final[,c(3,5,7,9,11)] <= Temp.low] <- NA
    }
    else if(ncol(MR.final) == 14){
      MR.final[,c(3,5,7,9,11,13)][MR.final[,c(3,5,7,9,11,13)] <= Temp.low] <- NA
    }
    else if(ncol(MR.final) == 16){
      MR.final[,c(3,5,7,9,11,13,15)][MR.final[,c(3,5,7,9,11,13,15)] <= Temp.low] <- NA
    }
    else if(ncol(MR.final) == 18){
      MR.final[,c(3,5,7,9,11,13,15,17)][MR.final[,c(3,5,7,9,11,13,15,17)] <= Temp.low] <- NA
    }else{
      print("The FishResp format of a table is not correct")
    }
  }else{
    print("No low threshold for water temperature (Temp) has been set ")
  }

  # make data above a defined high threshold for water temperature as NA
  if(is.na(Temp.high) != T){
    if(ncol(MR.final) == 4){
      MR.final[,3][MR.final[,3] >= Temp.high] <- NA
    }
    else if(ncol(MR.final) == 6){
      MR.final[,c(3,5)][MR.final[,c(3,5)] >= Temp.high] <- NA
    }
    else if(ncol(MR.final) == 8){
      MR.final[,c(3,5,7)][MR.final[,c(3,5,7)] >= Temp.high] <- NA
    }
    else if(ncol(MR.final) == 10){
      MR.final[,c(3,5,7,9)][MR.final[,c(3,5,7,9)] >= Temp.high] <- NA
    }
    else if(ncol(MR.final) == 12){
      MR.final[,c(3,5,7,9,11)][MR.final[,c(3,5,7,9,11)] >= Temp.high] <- NA
    }
    else if(ncol(MR.final) == 14){
      MR.final[,c(3,5,7,9,11,13)][MR.final[,c(3,5,7,9,11,13)] >= Temp.high] <- NA
    }
    else if(ncol(MR.final) == 16){
      MR.final[,c(3,5,7,9,11,13,15)][MR.final[,c(3,5,7,9,11,13,15)] >= Temp.high] <- NA
    }
    else if(ncol(MR.final) == 18){
      MR.final[,c(3,5,7,9,11,13,15,17)][MR.final[,c(3,5,7,9,11,13,15,17)] >= Temp.high] <- NA
    }else{
      print("The FishResp format of a table is not correct")
    }
  }else{
    print("No high threshold for water temperature (Temp) has been set ")
  }

  write.table(MR.final, file = export.file, sep = "\t", row.names = F)
}
