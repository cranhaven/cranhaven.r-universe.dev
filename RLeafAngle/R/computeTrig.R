#' Compute the parameter of Verhoef's leaf angle distribution given leaf angle mearuements.
#' @description Compute the parameter of Verhoef's leaf angle distribution given leaf angle distribution mearuements.
#' @param LeafAngles The measurements of leaf angle distribution.
#' @return The two parameters of Verhoef's leaf angle distribution.
#' @author Wei-Min Wang (wmwang AT gmail.com)
#' @references  Wang, W. M., Li, Z. L., & Su, H. B. (2007). 
#' Comparison of leaf angle distribution functions: effects on extinction coefficient and 
#' fraction of sunlit foliage. Agricultural and Forest Meteorology, 143(1), 106-122.
#' @examples
#' data(Pisek)
#' computeTrig(Pisek[[2]])
#' @export
#' 

computeTrig<-function(LeafAngles)
{
  histo <- hist(LeafAngles, plot=FALSE)  
  ang <- c()
  for(i in 1:(length(histo[[1]])-1))
  {
    ang[i] <- (histo[[1]][i]+histo[[1]][i+1])/2
  }
  perz <- histo[[2]]/sum(histo[[2]])
  sumper <- sum(perz)
  perc <- 0
  sumt <- 0
  
  angInterval <- abs(ang[2] - ang[1])  
  per <- 0
  
  for(i in 1:length(perz))
  {
    per[i] <- perz[i]/sumper
    sumt <- sumt + per[i]
    perc[i] <- sumt
  } 
  
  ec <- 0
  fc <- 0
  gcc <- 0
  hc <- 0
  ic <- 0
  jc <- 0
  kc <- 0
  lc <- 0
  mc <- 0  
  for(i in 1:length(perz))
  {
    ec[i] <- (perc[i] + (ang[i] + angInterval/2)/90)*pi/2
    fc[i] <- (perc[i] - (ang[i] + angInterval/2)/90)*pi/2
    
    gcc[i] <- sin(ec[i])
    hc[i] <- sin(2*ec[i])
    
    ic[i] <- gcc[i] * gcc[i]
    jc[i] <- gcc[i] * hc[i]
    
    kc[i] <- hc[i] * hc[i]
    lc[i] <- fc[i] * gcc[i]
    
    mc[i] <- fc[i] * hc[i]
  }
  
  sumic <- sum(ic)
  sumjc <- sum(jc)
  sumlc <- sum(lc)
  sumkc <- sum(kc)
  summc <- sum(mc)
  
  a <- (sumkc*sumlc-sumjc*summc)/(sumkc*sumic - sumjc*sumjc)
  b <- 2*(-sumjc*sumlc+sumic*summc)/(sumkc*sumic - sumjc*sumjc)
  
  c(a, b)
}