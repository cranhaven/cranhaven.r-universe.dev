#' Formatting for longitude scales in ggplot spatial maps
#'
#' @importFrom scales trans_new
#' 

lon_trans = function() {
  
  trans_new( name = 'lon', transform = function(x){x}, 
             inverse = function(x){x}, 
             format = function(x) {
               # map longitudes onto a 0-360 degree scale (1-179 E, 181-359 W)
               x = x %% 360
               # identify western longitudes
               W = x > 180
               # re-index western longitudes (accounts for NAs in x)
               W.ind = !is.na(W)
               x[W.ind][W[W.ind]] = - x[W.ind][W[W.ind]] %% 180
               # build labels
               lab = paste(as.character(x), ifelse(W, 'W', 'E'), sep='')
               # remove direction for 0 and 180 degrees
               lab = gsub('^(0E|0W)$', '0', lab)
               lab = gsub('^(180E|180W)$', '0', lab)
               # return labels
               lab
             } )
  
}


#' Formatting for longitude scales in ggplot spatial maps
#'
#' @importFrom scales trans_new
#' 

lat_trans = function() {
  
  trans_new( name = 'lat', transform = function(x){x}, 
             inverse = function(x){x}, 
             format = function(x) { 
               S = x < 0
               gsub('^0N$', '0', paste(gsub('-', '', as.character(x)), 
                                       ifelse(S, 'S', 'N'), sep=''))
             } )
  
}