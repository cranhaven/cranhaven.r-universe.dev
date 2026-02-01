############################################################################
#################   Detecting Genotyping error markers  ####################
###########################################################################

detect.err <- function(netgwas.map,  err.prob = 0.01, cutoff= 4, pop.type= NULL, map.func = "haldane" ){
  
  if(is.null(pop.type))  stop("Population type needs to be \"BC\",\"DH\",\"ARIL\" or \"RILn\" (see ?netgwas.as.cross).")
  
  map <- cal.pos(netgwas.map, pop.type= pop.type , map.func = map.func )
  err <- calc.errorlod(map, error.prob= err.prob)
  top.errorlod(err, cutoff= cutoff)
  
}