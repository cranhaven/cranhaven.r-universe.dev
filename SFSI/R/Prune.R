
#====================================================================
# Prune elements based on pairwise similarity R^2
#====================================================================
Prune <- function(X, alpha = 0.95, pos = NULL, d.max = NULL,
                  centered = FALSE, scaled = FALSE,
                  verbose = FALSE)
{
    if(!is.null(d.max) & is.null(pos)){
      message(" Maximum distance 'd.max' is ignored when vector 'pos=NULL'")
      d.max <- NULL
    }
    if(is.null(d.max) & !is.null(pos)){
      stop("A 'd.max' value must be provided along with the vector 'pos'")
    }

    if(all(c("scaled:center","scaled:scale") %in% names(attributes(X)))){
      if(!(centered & scaled)){
        stop("Columns of 'X' are centered and scaled as per the 'base::scale' function.\n",
             "  Set inputs 'centered=TRUE' and 'scaled=TRUE' if a centering to mean zero and\n",
             "  scaling to standard deviation one was applied to 'X'")
      }
    }

    #dyn.load("c_prune.so")
    res <- .Call("R_prune", X, alpha, pos, d.max,
                 centered, scaled, verbose)
    #dyn.unload("c_prune.so")

    return(list(prune.in=res[[1]], prune.out=res[[2]]))
}
