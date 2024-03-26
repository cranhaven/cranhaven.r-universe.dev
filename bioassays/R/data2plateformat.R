#'@name data2plateformat
#'
#'@aliases data2plateformat
#'
#'@title Renaming column and Row of Multiwell Data to Match Plate Format
#'
#'@description Convert the data (example: readings from mutli well plate) to appropriate plate format by renaming column and rownames.
#'
#'@usage data2plateformat(data, platetype = 96)
#'
#'@param data Matrix data to be formatted
#'
#'@param platetype Plate from which the data is coming. It can take 6, 12, 24, 96 and 384 values to represent the corresponding multi well plate.
#'
#'@details This function will label the columns and rows correctly to match the plate format, and discard the extras. For example, if the 'data' is coming from a a '96' well plate ('platetype'), the function will rename rows as A to H and columns as 1 to 12. Extra columns and rows of 'data' is discarded.
#'
#'@return A data frame with columns and rows matching (label and numbers) the mutli well plate format.
#'
#'@author A.A Palakkan
#'
#'@examples
#' ## loading data
#' data(rawdata24,rawdata96,rawdata384)
#'
## eg:1 spectrophotometer reading from 24 well plate
#' data2plateformat(rawdata24, platetype = 24)
#'
#' ## eg:2 spectrophotometer reading from 96 well plate
#' data2plateformat(rawdata96, platetype = 96)
#'
#' ## eg:3 spectrophotometer reading from 384 well plate
#' data2plateformat(rawdata384, platetype = 384)
#'
#'@keywords manip
#'
#'@export




data2plateformat <- function (data,platetype = 96){


if(platetype==96){
    rawdata<-data[seq(1,8,1),seq(2,13,1)]
    rownames(rawdata)[seq(1,8,1)]<-LETTERS[seq(1,8,1)]
    colnames(rawdata)[seq(1,12,1)]<-c(seq(1,12,1))
    return(rawdata) }

if(platetype==24){
    rawdata<-data[seq(1,4,1),seq(2,7,1)]
    rownames(rawdata)[seq(1,4,1)]<-LETTERS[seq(1,4,1)]
    colnames(rawdata)[seq(1,6,1)]<-c(seq(1,6,1))
    return(rawdata)}

if(platetype==12){
    rawdata<-data[seq(1,3,1),seq(2,5,1)]
    rownames(rawdata)[seq(1,3,1)]<-LETTERS[seq(1,3,1)]
    colnames(rawdata)[seq(1,4,1)]<-c(seq(1,4,1))
    return(rawdata)}

if(platetype==6){
    rawdata<-data[seq(1,2,1),seq(2,4,1)]
    rownames(rawdata)[seq(1,2,1)]<-LETTERS[seq(1,2,1)]
    colnames(rawdata)[seq(1,3,1)]<-c(seq(1,3,1))
    return(rawdata) }

if(platetype==384){
    rawdata<-data[seq(1,16,1),seq(2,25,1)]
    rownames(rawdata)[seq(1,16,1)]<-LETTERS[seq(1,16,1)]
    colnames(rawdata)[seq(1,24,1)]<-c(seq(1,24,1))
    return(rawdata)}

}
