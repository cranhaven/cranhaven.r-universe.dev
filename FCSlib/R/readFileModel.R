#' @title Read File Model

#' @description Reads a txt file and returns the parameters and the model (equation).
#' @aliases readFileModel
#' @usage readFileModel(filename)
#' @param filename The name of the file to read from.
#' @details Read a txt file using the scan function and extracts the parameters and the model (equation) in the file.
#' @export
#' @import stringr
#' @return params   A list containing the parameters as well as the model.
#' @author Raúl Pinto Cámara.
#' 
#' @seealso \code{\link{fitFCS}}
#' 
#' @examples
#' \donttest{
#' modelData <- readFileModel(filename)
#' }

readFileModel <- function(filename){
  lastChar <- substr(filename, start = nchar(filename)-3, stop = nchar(filename))
  if(lastChar != ".txt"){
    stop(paste("The file ", filename, " is not a txt file", sep = ""))
  }
  dataF <- scan(file = filename, what = "character", sep = "\n")
  lenD <- length(dataF)
  params <- list()
  for(i in 1:lenD){
    if(substr(dataF[i], start = 1, stop = 1) != '#' & i != lenD){
      subdata <- strsplit(gsub(x = dataF[i], pattern = " ", ""), "=")[[1]]
      params[[subdata[1]]] <- as.numeric(subdata[2])
    } else if(i == lenD){
      dataF[i] <- str_replace(string = dataF[i], pattern = "=", replacement = "~")
      params["model"] <- dataF[i]
    }
  }
  return(params)
}