
#' Title checkmat
#'
#' @param dat MUST be a data.frame where each column represent a parameter of the thermogram you need to check
#' @param header present or not in your data.frame
#' @param selected a vector that include the coded position of the parameters present in the dataset. 0 equal not present, while if you insert a number its value will refer to
#' the index of the column of the input matrix where the parameter is stored. the coding of the vector selected is the following  1. "time.minutes" 2. "time.seconds" 3."temperature.s" 4."temperature.r" 5."temperature.s.K" 6."temperature.r.K"7."heat.flow"8. "id"
#' @return Checked data frame
#' @export
#' @details i.e. selected=c(1,0,2,0,0,0,3) means that your first column is time.seconds, the second column is the temperature of the sample
#' 	and the this column is the heat flow. 0 represents the other column of your files that are not present in your dataset
#' @examples npoints=1000
#'x=seq(1,npoints)
#'y=(dnorm(x, mean=npoints/2, sd=npoints/10))
#'x=seq(1,1000)
#'x2=seq(200,500,length.out=1000)
#'dat=data.frame(x,x2,y)
#'colnames(dat) <- c("time.seconds", "temperature.s","heat.flow")
#'cmat<- checkmat(dat,selected=c(1,0,2,0,0,0,3,0))
#'

checkmat <- function(dat, header=TRUE, selected=c(0, 1, 2, 0, 0, 0, 4,0)) {

  its.K <- NULL
  #require(tools)

  ncol(dat)

  itm <- selected[1]
  its <- selected[2]
  iTs <- selected[3]
  iTr <- selected[4]
  iTs.K <- selected[5]
  iTr.K <- selected[6]
  i.heat.flow <- selected[7]
  i.id <- selected[8]

  if (itm != 0) {
    time.minutes <- dat[itm]
  }
  else {
    print("time in minutes is missing")
    time.minutes <- dat[its] / 60
  }


  if (its != 0) {
    time.seconds <- dat[its]
  }
  else {
    print("time in seconds is missing")
    time.seconds <- dat[itm] * 60
  }


  if (iTs != 0) {
    temperature.s <- dat[iTs]
  }
  else {
    print("Temperature (?C) is missing")
    temperature.s <- dat[iTs.K] - 273.15
  }


  if (iTs.K != 0) {
    temperature.s.K <- dat[its.K]
  }
  else {
    print("Temperature (K) is missing")
    temperature.s.K <- dat[iTs] + 273.15
  }


  if ((iTr == 0) & (iTr.K == 0)) {
    print("Temperature of the reference is missing.. adding it")
    temperature.r <- dat[iTs]
    temperature.r.K <- dat[iTs] + 273.15
  }


  if (iTr != 0) {
    temperature.r <- dat[iTr]
  }
  else {
    print("Temperature of the reference is missing.. adding it")
    temperature.r <- dat[iTs]
  }


  if (iTr.K != 0) {
    temperature.r.K <- dat[iTr.K]
  }
  else {
    print("Temperature of the reference (K) is missing.. adding it")
    temperature.r.K <- dat[iTs] + 273.15
  }

  if (i.id != 0) {
    id <- dat[i.id]
  }
  else {
    print("id not present")
    id <- "0"
  }

  heat.flow <- dat[i.heat.flow]
  ds <- data.frame(time.minutes, time.seconds, temperature.s, temperature.r, temperature.s.K, temperature.r.K, heat.flow,id)
  colnames(ds) <- c("time.minutes", "time.seconds", "temperature.s", "temperature.r", "temperature.s.K", "temperature.r.K", "heat.flow","id")
  return(ds)
}
