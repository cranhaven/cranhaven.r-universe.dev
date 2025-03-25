# relationship level-volume in CSO tanks 
# author: J.A. Torres-Matallana, K. Klepiszewski, G. Schutz
# organization: Luxembourg Institute of Science and Technology (LIST), Luxembourg
#               Wagenigen University and Research Centre (WUR), Wageningen, The Netherlands   
# date: 24.06.2015 - 15.05.2017

Level2Volume <- function(lev, lev2vol){
  ## relationship level -> volume (Goesdorf CSO tank)
  # lev <- c(.25,.95,1.71,3.41)
  # vol <- c(0,13,43,199)

  # plot(lev, vol, main = "approx(.) and approxfun(.)")
  # points(approx(lev, vol), col = 2, pch = "*")
  # approx(lev,vol, xout=7.5, yleft=0, yright=max(lev2vol$vol))
  # approx(lev,vol, xout=350, yleft=0, yright=max(lev2vol$vol))
  # approx(lev,vol, xout=60, yleft=0, yright=max(lev2vol$vol))

  # str(lev2vol)
  vol_out <- approx(lev2vol$lev, lev2vol$vol, xout=lev, yleft=min(lev2vol$vol), yright=max(lev2vol$vol))
  vol_out <- vol_out$y
  return(vol_out)
}
