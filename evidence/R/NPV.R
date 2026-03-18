NPV <- function(sens, spec, prev) return(((1 - prev) * 
    spec)/((1 - sens) * prev + spec * (1 - prev)))
