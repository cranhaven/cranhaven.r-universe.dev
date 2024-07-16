#' @title Read File FCS

#' @description Reads a FCS file and returns the data sets within the file.
#' @aliases readFileFCS
#' @usage readFileFCS(filename)
#' @param filename the name of the file to read from.
#' @details Read a FCS file using the scan function and extract the data contained in the file.
#' @export
#' @return dataList   A list containing the data sets within the file.
#' @author Raúl Pinto Cámara.
#' 
#' @examples
#' \donttest{
#' raw_fcs <- readFileFCS(FileName)
#' }

readFileFCS <- function(filename){
  lastChar <- substr(filename, start = nchar(filename)-3, stop = nchar(filename))
  if(lastChar != ".fcs"){
    stop(paste("The file ", filename, " is not a FCS file", sep = ""))
  }
  dataF <- scan(file = filename, what = "character")
  lenF <- length(dataF)
  dataB <- dataLim <- nameD <- dataVect <- namesData <- NULL
  lenD <- 0
  dataList <- list()
  idN <- 1
  for(i in 1:lenF){
    if(grepl("Size", dataF[i])){
      lenD <- as.numeric(dataF[i+2])
      if(lenD > 0){
        nameD <- dataF[i+3]
        dataB <- i + 7
        dataLim <- i + 6 + (lenD * 2)
        subData <- as.numeric(dataF[dataB:dataLim])
        dataVect <- array(data = subData, dim = c(dataF[i+6], dataF[i+5]))
        nameDT <- paste(nameD, 1, sep = "_")
        while(nameDT %in% namesData) {
          idN <- idN + 1
          nameDT <- paste(nameD, idN, sep = "_")
        }
        if(idN == 1){
          nameDT <- paste(nameD, idN, sep = "_")
        }
        idN <- 1
        namesData <- c(namesData, nameDT)
        dataList[[nameDT]] <- dataVect
      }
    }
  }
  return(dataList)
}