PPV <- function(sens, spec, prev) return((sens * prev)/((sens * 
    prev) + (1 - spec) * (1 - prev)))
