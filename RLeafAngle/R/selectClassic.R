#' Retrieve the density of leaf area given leaf angle measurements.
#' @description Retrieve the density of leaf area given leaf angle measurements.
#' @param LeafAngles The measurements of leaf angle distribution.
#' @return The classic leaf angle distribution type 
#' @author Wei-Min Wang (wmwang AT gmail.com)
#' @references  Wang, W. M., Li, Z. L., & Su, H. B. (2007). 
#' Comparison of leaf angle distribution functions: effects on extinction coefficient and 
#' fraction of sunlit foliage. Agricultural and Forest Meteorology, 143(1), 106-122.
#' @examples
#' data(Falster) 
#' selectClassic(Falster[[2]])
#' @export

selectClassic<-function(LeafAngles)
{
  anga<-c(0, 20, 40, 60, 80, 100, 120, 140, 160, 180, 200, 220, 240, 260, 280,
            300, 320, 340, 360)
  angzc<-c(5, 15, 25, 35, 45, 55, 65, 75, 85) 
  angz<-c(0, 10.0, 20.0, 30.0, 40.0, 50.0, 60.0, 70.0, 80.0, 90.0)
  angzs<-c(5, 15, 25, 35, 45, 55, 65, 75, 85)
  angac<-c(10,30,50,70,90,110,130,150,170,190,210,230,250,270,290,310,330,350)
  pi180<-pi/180
  
  deviVector <- function(x, y)
  {
    deviSum <- 0 
    for(i in 1:length(x))
    {
      deviSum = deviSum + (x[i]-y[i])*(x[i]-y[i])
    }
    deviSum
  }
  
  spherical<-function(x)
  {
    sin(x)
  }
  
  uniform<-function(x)
  {
    (2/pi)*rep(1, length(x)) 
  }    
  
  planophile<-function(x)
  {
    2*(1+cos(2*x))/pi 
  } 
  
  erectophile<-function(x)
  {
    2*(1-cos(2*x))/pi
  }        
  plagiophile<-function(x)
  {
    2*(1-cos(4*x))/pi
  }        
  
  extremophile<-function(x)
  {
    2*(1+cos(4*x))/pi
  }  
  
  computeG<-function(angleZ, ladZ, angleA, ladA, theta, alpha)
  {
    pi180 <- pi/180
    sumGA <- 0
    sumGZ <- 0
    sumGAT <- 0
    for(i in 1:length(angleZ))
    {
      sumGA <- 0
      ###angleZ[i] <- 90 - angleI[i]
      for(j in 1:length(angleA))
      {
        cosr <- cos(theta*pi180)*cos(angleZ[i]*pi180)+sin(theta*pi180)*
          sin(angleZ[i]*pi180)*cos((alpha-angleA[j])*pi180)
        sumGA <- sumGA + abs(cosr) * ladA[j]
      }
      sumGAT[i] <- sumGA
      sumGZ <- sumGZ + sumGA * ladZ[i]
    }
    sumGZ
  }
  
  symA<-function(x)
  {
    #symmetric azimuth distribution.  
    #rewrite this code to predict Leaf Area percent
    #for given leaf azimuthal angle.    
    (1/360.0)*rep(1, length(x)) 
  } 
  
  LADdensity<-function(LeafAngles)
  {
    hh <- hist(LeafAngles, plot=FALSE, breaks=c(0,10,20,30,40,50,60,70,80,90))
    return (hh[[3]]*10.0)
  }
  
  ladz<-LADdensity(LeafAngles)
  
  ladA<-0
  for(i in 1:(length(anga)-1))
  {
    ladA[i] <- integrate(symA, stop.on.error = TRUE, lower =anga[i], upper = anga[i+1])[1][[1]]
    
  }
  
  pcSix <- array(1:(length(angzc))*6, dim=c(6, length(angzc)))
  GSix <- array(1:(length(angzc))*6, dim=c(6, length(angzc)))
  
  Bs<-array(length(angzs))
  
  SixTypes <- c("planophile", "erectophile", "plagiophile", "extremophile", "uniform", "spherical")
 
  for (iis in 1:length(angzs))
  {
    Bs[iis] <- computeG(angzc, ladz, angac, ladA, angzs[iis], 0)
    
  }
  
  for(i in 1:(length(angz)-1))
  {
    
    pcSix[1,i] <- integrate(planophile, stop.on.error = TRUE, lower = angz[i]*pi180, upper = angz[i+1]*pi180)[1][[1]]
    
    pcSix[2,i] <- integrate(erectophile, stop.on.error = TRUE, lower = angz[i]*pi180, upper = angz[i+1]*pi180)[1][[1]]
    
    pcSix[3,i] <- integrate(plagiophile, stop.on.error = TRUE, lower = angz[i]*pi180, upper = angz[i+1]*pi180)[1][[1]]
    
    pcSix[4,i] <- integrate(extremophile, stop.on.error = TRUE, lower = angz[i]*pi180, upper = angz[i+1]*pi180)[1][[1]]
    
    pcSix[5,i] <- integrate(uniform, stop.on.error = TRUE, lower = angz[i]*pi180, upper = angz[i+1]*pi180)[1][[1]]
    
    pcSix[6,i] <- integrate(spherical, stop.on.error = TRUE, lower = angz[i]*pi180, upper = angz[i+1]*pi180)[1][[1]]
    
  }
  

  for(i in 1:(length(angzs)))
  {
    GSix[1, i] <-  computeG(angzc, pcSix[1,], angac, ladA, angzs[i], 0)
    GSix[2, i] <-  computeG(angzc, pcSix[2,], angac, ladA, angzs[i], 0)
    GSix[3, i] <-  computeG(angzc, pcSix[3,], angac, ladA, angzs[i], 0)
    GSix[4, i] <-  computeG(angzc, pcSix[4,], angac, ladA, angzs[i], 0)
    GSix[5, i] <-  computeG(angzc, pcSix[5,], angac, ladA, angzs[i], 0)
    GSix[6, i] <-  computeG(angzc, pcSix[6,], angac, ladA, angzs[i], 0)
  }
  
  deviG <- 10
  iSix <- 0 
  for(i in 1:6)
  { 
    dvTemp <- deviVector(Bs, GSix[i,])
    if(deviG > dvTemp)
    {
      deviG = dvTemp 
      iSix <- i
    }
  }
  SixTypes[iSix]
}  
  