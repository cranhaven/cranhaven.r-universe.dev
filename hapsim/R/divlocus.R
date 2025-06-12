"divlocus" <-
function(dat){
   n <- nrow(dat)
   hp.div <- apply(dat, 2, function(x) ( 2 * ( n * sum(x^2) - sum(x)^2 ) ) )
   return(hp.div)
}

