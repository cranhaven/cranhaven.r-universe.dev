## scaleline extractor function
scaleline<- function(obj,...)UseMethod("scaleline")

scaleline.scaledTrellis<- function(obj,...){ obj$addScales$args$scaleline}



