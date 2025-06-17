prepare_variable <-
function(data, lower, upper) {
    # prepare_variable creates an nx2 SAS-style matrix out of the
    # given lower and upper bound variables.
    #
    # Arguments
    # data: A data frame containing variables as named in lower and upper
    # lower: A variable containing the lower limit of the interval containing
    #        the observed time
    # upper: A variable containing the upper limit of the interval containing
    #        the observed time
    
    Y = list()
    for (j in 1:2) {
        
        vname <- c(lower, upper)[j]
        
        if (!(vname %in% colnames(data))) {
            err <- sprintf("Variable '%s' is not in the data frame.", vname)
            stop(err)
        }
        ii <- match(vname, colnames(data))
        
        Y[[j]] <- data[,ii]
    }
    Y <- cbind(Y[[1]], Y[[2]])
    
    ii <- which(is.finite(Y[,1]) & is.finite(Y[,2]) & (Y[,1]>Y[,2]))
    if (length(ii) > 0) {
        stop(sprintf("There are %d cases with LB > UB", length(ii)))
    }

    return(Y)
}
