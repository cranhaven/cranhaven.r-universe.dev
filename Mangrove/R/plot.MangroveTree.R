plot.MangroveTree <-
function(x,...) stop("Cannot plot pedigrees using Mangrove. Perhaps use \"kinship2\" package instead? If you want to print the results of a simulation, use plot(",deparse(substitute(x)),"$getPrevs()).")

