# relationship volume-level in CSO tanks 
# author: J.A. Torres-Matallana, K. Klepiszewski, G. Schutz
# organization: LIST
# date: 29.08.2016 - 29.08.2016

Volume2Level <- function(vol, lev2vol){
  ## relationship level -> volume (Goesdorf CSO tank)
  ## lev2vol <- list(lev = c(.25, 1.10, 1.30, 3.30), vol = c(0, 31, 45, 190))

  # plot(elev, vol, main = "approx(.) and approxfun(.)")
  # points(approx(elev, vol), col = 2, pch = "*")
  # approx(vol, elev, xout=0, yleft=25, yright=341)
  
  lev_out <- approx(lev2vol$vol, lev2vol$lev, xout=vol, yleft=min(lev2vol$lev), yright=max(lev2vol$lev))
  lev_out <- lev_out$y
  return(lev_out)
}

# Volume2Level(0, lev2vol)
