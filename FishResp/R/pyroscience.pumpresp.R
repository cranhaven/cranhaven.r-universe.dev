#' Convert Respirometry Data from PyroScience and the pump controller PumpResp to the FishResp Format
#'
#' The function is used to convert raw data from 'Pyro Oxygen Logger' (\href{https://www.pyroscience.com/}{PyroScience}) and a logger file of the pump controller \href{https://fishresp.org/pumpresp/}{PumpResp} to the 'FishResp' format. This function should be applied before usage of the functions \code{\link{import.test}} and \code{\link{import.meas}}. The output is a file containing raw respirometry data in the 'FishResp' format (see Details in \code{\link{import.test}} to read more information about the 'FishResp' format)
#'
#' @usage
#' pyroscience.pumpresp(pyroscience.file,
#'                      pumpresp.file,
#'                      fishresp.file,
#'                      n.chamber = c(1,2,3,4),
#'                      date.format = c("DMY", "MDY", "YMD"))
#'
#' @param pyroscience.file  the name of a file which contains raw data obtained from the 'Pyro Oxygen Logger' software (\href{https://www.pyroscience.com/}{PyroScience})
#' @param pumpresp.file  the name of a file which contains logger data obtained from the pump conroller \href{https://fishresp.org/pumpresp/}{PumpResp}
#' @param fishresp.file  the name of an exported file containing raw data in the 'FishResp' format
#' @param n.chamber  integer: the number of chambers used in an experiment (including empty ones)
#' @param date.format  string: date format (DMY, MDY or YMD) used in raw data obtained from the 'Pyro Oxygen Logger' software
#'
#' @return The function exports a file containing raw data in the 'FishResp' format
#' @importFrom chron chron
#' @importFrom utils read.table write.table
#'
#' @examples
#' \dontrun{
#' pyroscience.path = system.file("extdata/salmon/salmon_pyroscience.txt",
#'                  package = "FishResp")
#'
#' pumpresp.path = system.file("extdata/salmon/salmon_pumpresp.txt",
#'                  package = "FishResp")
#'
#' pyroscience.pumpresp(pyroscience.path,
#'                      pumpresp.path,
#'                      "fishresp.txt",
#'                      n.chamber = 4,
#'                      date.format = "DMY")
#' }
#' @export

pyroscience.pumpresp <- function(pyroscience.file,
                                 pumpresp.file,
                                 fishresp.file,
                                 n.chamber = c(1,2,3,4),
                                 date.format = c("DMY", "MDY", "YMD")){

  # Loading PyroScience data
  V1 <- V2 <- V3 <- V4 <- V5 <- V6 <- V7 <- V8 <- V9 <- V10 <- V11 <- V12 <- NULL

  if (n.chamber == 1){
    pyroscience.data <- read.table(pyroscience.file, sep = "\t", skip=14, header=F, strip.white=T)
    pyroscience.data <- subset(pyroscience.data, select=c(V1, V2, V9, V5))
    colnames(pyroscience.data)<-c("Date", "Time", "Temp.1", "Ox.1")
    for(i in 3:ncol(pyroscience.data)){pyroscience.data[,i] = as.numeric(gsub(',','.', pyroscience.data[,i]))}
    pyroscience.data$Date.Time <- paste(pyroscience.data$Date, pyroscience.data$Time, sep="/")
    pyroscience.data[pyroscience.data == "---"] <- NA
    pyroscience.data <- pyroscience.data[,c(5,3,4)]
    }

  else if (n.chamber == 2){
    pyroscience.data <- read.table(pyroscience.file, sep = "\t", skip=16, header=F, strip.white=T)
    pyroscience.data <- subset(pyroscience.data, select=c(V1, V2, V9, V5, V10, V6))
    colnames(pyroscience.data)<-c("Date", "Time", "Temp.1", "Ox.1", "Temp.2", "Ox.2")
    for(i in 3:ncol(pyroscience.data)){pyroscience.data[,i] = as.numeric(gsub(',','.', pyroscience.data[,i]))}
    pyroscience.data[pyroscience.data == "---"] <- NA
    pyroscience.data <- pyroscience.data[,c(7,3,4,5,6)]
    }

  else if (n.chamber == 3){
    pyroscience.data <- read.table(pyroscience.file, sep = "\t", skip=18, header=F, strip.white=T)
    pyroscience.data <- subset(pyroscience.data, select=c(V1, V2, V9, V5, V10, V6, V11, V7))
    colnames(pyroscience.data)<-c("Date", "Time", "Temp.1", "Ox.1", "Temp.2", "Ox.2", "Temp.3", "Ox.3")
    for(i in 3:ncol(pyroscience.data)){pyroscience.data[,i] = as.numeric(gsub(',','.', pyroscience.data[,i]))}
    pyroscience.data$Date.Time <- paste(pyroscience.data$Date, pyroscience.data$Time, sep="/")
    pyroscience.data[pyroscience.data == "---"] <- NA
    pyroscience.data <- pyroscience.data[,c(9,3,4,5,6,7,8)]
    }

  else if (n.chamber == 4){
    pyroscience.data <- read.table(pyroscience.file, sep = "\t", skip=20, header=F, strip.white=T)
    pyroscience.data <- subset(pyroscience.data, select=c(V1, V2, V9, V5, V10, V6, V11, V7, V12, V8))
    colnames(pyroscience.data)<-c("Date", "Time", "Temp.1", "Ox.1", "Temp.2", "Ox.2", "Temp.3", "Ox.3", "Temp.4", "Ox.4")
    for(i in 3:ncol(pyroscience.data)){pyroscience.data[,i] = as.numeric(gsub(',','.', pyroscience.data[,i]))}
    pyroscience.data$Date.Time <- paste(pyroscience.data$Date, pyroscience.data$Time, sep="/")
    pyroscience.data[pyroscience.data == "---"] <- NA
    pyroscience.data <- pyroscience.data[,c(11,3,4,5,6,7,8,9,10)]
    }

  else{
    print("Please, choose the number of chambers between 1 and 4")
    }

  if(any(date.format == "DMY")){
    pyroscience.data$Date.Time<-strptime(as.character(pyroscience.data$Date.Time), "%d/%m/%Y/%H:%M:%S")
    start <- pyroscience.data$Date.Time[1]
    end <- tail(pyroscience.data$Date.Time, n=1)
    temp.df <- as.data.frame(seq.POSIXt(start, end, by = "1 sec"))
    names(temp.df) <- "Date.Time"
    temp.df$Date.Time <- as.POSIXlt(temp.df$Date.Time)
    pyroscience.data <- merge(temp.df, pyroscience.data, by = "Date.Time", all = T)
    pyroscience.data$Temp.1 <- na.omit(pyroscience.data$Temp.1)[cumsum(!is.na(pyroscience.data$Temp.1))]
    pyroscience.data$Ox.1 <- na.omit(pyroscience.data$Ox.1)[cumsum(!is.na(pyroscience.data$Ox.1))]
    pyroscience.data$Temp.2 <- na.omit(pyroscience.data$Temp.2)[cumsum(!is.na(pyroscience.data$Temp.2))]
    pyroscience.data$Ox.2 <- na.omit(pyroscience.data$Ox.2)[cumsum(!is.na(pyroscience.data$Ox.2))]
    pyroscience.data$Temp.3 <- na.omit(pyroscience.data$Temp.3)[cumsum(!is.na(pyroscience.data$Temp.3))]
    pyroscience.data$Ox.3 <- na.omit(pyroscience.data$Ox.3)[cumsum(!is.na(pyroscience.data$Ox.3))]
    pyroscience.data$Temp.4 <- na.omit(pyroscience.data$Temp.4)[cumsum(!is.na(pyroscience.data$Temp.4))]
    pyroscience.data$Ox.4 <- na.omit(pyroscience.data$Ox.4)[cumsum(!is.na(pyroscience.data$Ox.4))]
    ts <- times(strftime(pyroscience.data$Date.Time, "%H:%M:%S"))
    ds <- format(pyroscience.data$Date.Time, format="%d/%m/%Y")
    pyroscience.data$Date.Time <- paste(ds,ts, sep = "/")
    rm(temp.df)
    }

  else if(any(date.format == "MDY")){
    pyroscience.data$Date.Time<-strptime(as.character(pyroscience.data$Date.Time), "%m/%d/%Y/%H:%M:%S")
    start <- pyroscience.data$Date.Time[1]
    end <- tail(pyroscience.data$Date.Time, n=1)
    temp.df <- as.data.frame(seq.POSIXt(start, end, by = "1 sec"))
    names(temp.df) <- "Date.Time"
    temp.df$Date.Time <- as.POSIXlt(temp.df$Date.Time)
    pyroscience.data <- merge(temp.df, pyroscience.data, by = "Date.Time", all = T)
    pyroscience.data$Temp.1 <- na.omit(pyroscience.data$Temp.1)[cumsum(!is.na(pyroscience.data$Temp.1))]
    pyroscience.data$Ox.1 <- na.omit(pyroscience.data$Ox.1)[cumsum(!is.na(pyroscience.data$Ox.1))]
    pyroscience.data$Temp.2 <- na.omit(pyroscience.data$Temp.2)[cumsum(!is.na(pyroscience.data$Temp.2))]
    pyroscience.data$Ox.2 <- na.omit(pyroscience.data$Ox.2)[cumsum(!is.na(pyroscience.data$Ox.2))]
    pyroscience.data$Temp.3 <- na.omit(pyroscience.data$Temp.3)[cumsum(!is.na(pyroscience.data$Temp.3))]
    pyroscience.data$Ox.3 <- na.omit(pyroscience.data$Ox.3)[cumsum(!is.na(pyroscience.data$Ox.3))]
    pyroscience.data$Temp.4 <- na.omit(pyroscience.data$Temp.4)[cumsum(!is.na(pyroscience.data$Temp.4))]
    pyroscience.data$Ox.4 <- na.omit(pyroscience.data$Ox.4)[cumsum(!is.na(pyroscience.data$Ox.4))]
    ts<-times(strftime(pyroscience.data$Date.Time, "%H:%M:%S"))
    ds <- format(pyroscience.data$Date.Time, format="%m/%d/%Y")
    pyroscience.data$Date.Time <- paste(ds,ts, sep = "/")
    rm(temp.df)
    }

  else if(any(date.format == "YMD")){
    pyroscience.data$Date.Time<-strptime(as.character(pyroscience.data$Date.Time), "%Y/%m/%d/%H:%M:%S")
    start <- pyroscience.data$Date.Time[1]
    end <- tail(pyroscience.data$Date.Time, n=1)
    temp.df <- as.data.frame(seq.POSIXt(start, end, by = "1 sec"))
    names(temp.df) <- "Date.Time"
    temp.df$Date.Time <- as.POSIXlt(temp.df$Date.Time)
    pyroscience.data <- merge(temp.df, pyroscience.data, by = "Date.Time", all = T)
    pyroscience.data$Temp.1 <- na.omit(pyroscience.data$Temp.1)[cumsum(!is.na(pyroscience.data$Temp.1))]
    pyroscience.data$Ox.1 <- na.omit(pyroscience.data$Ox.1)[cumsum(!is.na(pyroscience.data$Ox.1))]
    pyroscience.data$Temp.2 <- na.omit(pyroscience.data$Temp.2)[cumsum(!is.na(pyroscience.data$Temp.2))]
    pyroscience.data$Ox.2 <- na.omit(pyroscience.data$Ox.2)[cumsum(!is.na(pyroscience.data$Ox.2))]
    pyroscience.data$Temp.3 <- na.omit(pyroscience.data$Temp.3)[cumsum(!is.na(pyroscience.data$Temp.3))]
    pyroscience.data$Ox.3 <- na.omit(pyroscience.data$Ox.3)[cumsum(!is.na(pyroscience.data$Ox.3))]
    pyroscience.data$Temp.4 <- na.omit(pyroscience.data$Temp.4)[cumsum(!is.na(pyroscience.data$Temp.4))]
    pyroscience.data$Ox.4 <- na.omit(pyroscience.data$Ox.4)[cumsum(!is.na(pyroscience.data$Ox.4))]
    ts<-times(strftime(pyroscience.data$Date.Time, "%H:%M:%S"))
    ds <- format(pyroscience.data$Date.Time, format="%Y/%m/%d")
    pyroscience.data$Date.Time <- paste(ds,ts, sep = "/")
    rm(temp.df)
    }


  # Loading PumpResp data
  V1 <- V2 <- V3 <- V4 <- V5 <- V6 <- V7 <- V8 <- V9 <- V10 <- V11 <- V12 <- NULL

  pumpresp.data <- read.table(pumpresp.file, sep = "\t", header=F, skip = 1, strip.white=T)
  pumpresp.data <- subset(pumpresp.data, select=c(V1, V2, V3))
  colnames(pumpresp.data) <- c("Date", "Time", "Phase")
  pumpresp.data <- pumpresp.data[!apply(pumpresp.data == "", 1, all),]
  pumpresp.data$Date.Time <- paste(pumpresp.data$Date, pumpresp.data$Time, sep="/")
  pumpresp.data <- pumpresp.data[,c(4,3)]

  if(any(date.format == "DMY")){
    pumpresp.data$Date.Time<-strptime(as.character(pumpresp.data$Date.Time), "%d/%m/%Y/%H:%M:%S")
    start <- pumpresp.data$Date.Time[1]
    end <- tail(pumpresp.data$Date.Time, n=1)
    temp.df <- as.data.frame(seq.POSIXt(start, end, by = "1 sec"))
    names(temp.df) <- "Date.Time"
    temp.df$Date.Time <- as.POSIXlt(temp.df$Date.Time)
    # if pumpresp.data and temp.df are equal
    if(!all(temp.df$Date.Time %in% pumpresp.data$Date.Time)){
      pumpresp.data <- merge(temp.df, pumpresp.data, by = "Date.Time", all = T)
    }
    pumpresp.data$Phase <- na.omit(pumpresp.data$Phase)[cumsum(!is.na(pumpresp.data$Phase))]
    ts <- times(strftime(pumpresp.data$Date.Time, "%H:%M:%S"))
    ds <- format(pumpresp.data$Date.Time, format="%d/%m/%Y")
    pumpresp.data$Date.Time <- paste(ds,ts, sep = "/")
    rm(temp.df)
    }

  else if(any(date.format == "MDY")){
    pumpresp.data$Date.Time<-strptime(as.character(pumpresp.data$Date.Time), "%m/%d/%Y/%H:%M:%S")
    start <- pumpresp.data$Date.Time[1]
    end <- tail(pumpresp.data$Date.Time, n=1)
    temp.df <- as.data.frame(seq.POSIXt(start, end, by = "1 sec"))
    names(temp.df) <- "Date.Time"
    temp.df$Date.Time <- as.POSIXlt(temp.df$Date.Time)
    # if pumpresp.data and temp.df are equal
    if(!all(temp.df$Date.Time %in% pumpresp.data$Date.Time)){
      pumpresp.data <- merge(temp.df, pumpresp.data, by = "Date.Time", all = T)
    }
    pumpresp.data$Phase <- na.omit(pumpresp.data$Phase)[cumsum(!is.na(pumpresp.data$Phase))]
    ts<-times(strftime(pumpresp.data$Date.Time, "%H:%M:%S"))
    ds <- format(pumpresp.data$Date.Time, format="%m/%d/%Y")
    pumpresp.data$Date.Time <- paste(ds,ts, sep = "/")
    rm(temp.df)
    }

  else if(any(date.format == "YMD")){
    pumpresp.data$Date.Time<-strptime(as.character(pumpresp.data$Date.Time), "%Y/%m/%d/%H:%M:%S")
    start <- pumpresp.data$Date.Time[1]
    end <- tail(pumpresp.data$Date.Time, n=1)
    temp.df <- as.data.frame(seq.POSIXt(start, end, by = "1 sec"))
    names(temp.df) <- "Date.Time"
    temp.df$Date.Time <- as.POSIXlt(temp.df$Date.Time)
    # if pumpresp.data and temp.df are equal
    if(!all(temp.df$Date.Time %in% pumpresp.data$Date.Time)){
      pumpresp.data <- merge(temp.df, pumpresp.data, by = "Date.Time", all = T)
    }
    pumpresp.data$Phase <- na.omit(pumpresp.data$Phase)[cumsum(!is.na(pumpresp.data$Phase))]
    ts<-times(strftime(pumpresp.data$Date.Time, "%H:%M:%S"))
    ds <- format(pumpresp.data$Date.Time, format="%Y/%m/%d")
    pumpresp.data$Date.Time <- paste(ds,ts, sep = "/")
    rm(temp.df)
    }

  pumpresp.data <- pumpresp.data[!duplicated(pumpresp.data$Date.Time,
                                 by = pumpresp.data$Date.Time,
                                 fromLast = FALSE),]
  pyroscience.data <- merge(pumpresp.data,  pyroscience.data, by = "Date.Time")

  write.table(pyroscience.data, file = fishresp.file, sep = "\t", row.names = F)
  }
