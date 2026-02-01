netgwas2cross <- function(netgwasmap, pop.type= NULL, map.func = "haldane" ){
  if(is.null(pop.type)) stop("Population type needs to be \"BC\",\"DH\",\"ARIL\" or \"RILn\" ")
  map <- as.cross(netgwasmap, pop.type= pop.type)  
  map <- quickEst( map, map.function = map.func)
  map <- jittermap(map, amount=1e-6)
  return(map)
}