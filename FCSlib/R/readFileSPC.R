#' @title Read File SPC-140/150/130/830
#' 
#' @description Reads a SPC file and returns the Macrotime and Microtime.
#' @aliases readFileSPC
#' @usage readFileSPC(filename, nData = 1E8)
#' @param filename the name of the file to read from.
#' @param nData parameter that defines the length of data to read.
#' @details Read a SPC file, with SPC-140/150/130/830 version, using the readBin function and extract the data contained in the file.
#' 
#' @note The nData parameter is used to overestimate the amount of data that the file can contain.
#' @export
#' @import bitops
#' @return A list containing the Macrotime and the Microtime vectors.
#' @references Becker, W., 2019. The Bh TCSPC Handbook. 8th ed. Berlin, Germany: Becker & Hickl GmbH, pp. 855-856.
#' @author Raúl Pinto Cámara, José Damián Martínez Reyes.
#' 
#' @seealso \code{\link{asynACTCSPC}}
#' 
#' @examples
#' \donttest{
#' spcData <- readFileSPC(FileName)
#' }

readFileSPC <- function(filename, nData = 1E8){
  lastChar <- substr(filename, start = nchar(filename)-3, stop = nchar(filename))
  if(lastChar != ".spc"){
    stop(paste("The file ", filename, " is not a FCS file", sep = ""))
  }
  if(!is.numeric(nData)){
    stop("nData must be a numeric type")
  }
  fr = file(filename, "rb")
  firstData <- readBin(fr, 1, what = "integer", endian = "little")
  ByteRecord <- readBin(fr, nData, what = "integer", endian = "little")
  close(fr)
  Rout <- sapply(ByteRecord, FUN = function(x){bitShiftR(x, 8)})
  Rout <- sapply(Rout, FUN = function(x){bitAnd(x, 240)})
  Mark <- sapply(ByteRecord, FUN = function(x){bitShiftR(x, 28)})
  Mark <- sapply(Mark, FUN = function(x){bitAnd(x, 15)})
  Mark <- Mark + Rout
  MT <- rep(0,length(Mark))
  MT[(sapply(Mark, FUN = function(x){bitAnd(x, 12)})==4)] <- 1
  MT[(sapply(Mark, FUN = function(x){bitAnd(x, 13)})==13)] <- 1
  MT[(sapply(Mark, FUN = function(x){bitAnd(x, 13)})==12)] <- sapply(ByteRecord[(sapply(Mark, FUN = function(x){bitAnd(x, 13)})==12)], FUN = function(x){bitAnd(x, 268435455)})
  Macrotime <- rep(0,length(Mark))
  Macrotime[(sapply(Mark, FUN = function(x){bitAnd(x, 13)})!=12)] <- sapply(ByteRecord[(sapply(Mark, FUN = function(x){bitAnd(x, 13)})!=12)], FUN = function(x){bitAnd(x, 4095)})
  MI <- 4095 - sapply(sapply(ByteRecord, FUN = function(x){bitShiftR(x, 16)}), FUN = function(x){bitAnd(x, 4095)})
  MT <- cumsum(MT) * 4096 + Macrotime
  return(list("MacroTime" = MT, "MicroTime" = MI))
}